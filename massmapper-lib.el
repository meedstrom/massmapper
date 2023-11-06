;;; massmapper-lib.el -- Library for deianira and massmapper -*- lexical-binding: t; nameless-current-name: "massmapper"; -*-

;; (read-symbol-shorthands . '("massmapper-" . "massmapper-lib-"))

;; Copyright (C) 2018-2023 Martin Edström

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.

(require 'dash)
(require 'compat)
(require 'cl-lib)
(require 'subr-x)

(defconst massmapper--ignore-keys-control-chars
  '("ESC" "C-["
    "RET" "C-m"
    "TAB" "C-i"))

(defconst massmapper--ignore-keys-irrelevant
  '("compose" "Scroll_Lock" "drag-n-drop" "help"
    "mouse" "remap" "scroll-bar" "select" "switch" "state"
    "which-key" "corner" "divider" "edge" "header" "mode-line"
    "vertical-line" "frame" "open" "chord" "tool-bar" "fringe"
    "touch" "margin" "pinch" "tab-bar" "menu-bar")
  "List of strings matching key events unlikely to matter to the
user's keyboard setup.")

(defconst massmapper--modifier-regexp
  (regexp-opt '("A-" "C-" "H-" "M-" "S-" "s-"))
  "Regexp for any of the modifiers: \"C-\", \"M-\" etc.
Upon match, the string segment it matched is always two characters long.

Beware that it will also match a few obscure named function keys,
such as <ns-drag-n-drop>, where it will match against s- in the
ns- part.  To guard this, use `massmapper--modifier-regexp-safe'
instead, although that has its own flaws.")

(defconst massmapper--modifier-regexp-safe
  (rx (or "<" "-" bol " ")
      (or "A-" "C-" "H-" "M-" "S-" "s-"))
  "Like `massmapper--modifier-regexp', but check a preceding character.
Benefit: it will always match if there's at least one modifier,
and not count spurious modifier-looking things such as
<ns-drag-n-drop> which contains an \"s-\".

Drawback: you can't match twice on the same string.  Look at the
case of M-s-<down>: it'll match M-, but if the search continues
from there, it will fail to match s- since it's only looking for
s- preceded by a dash (i.e. \"-s-\"), and our second search
starts past the first dash.  However, it's fine if you cut the
string and start a new search on the cut string.")

(defun massmapper--raw-keymap (map)
  "If MAP is a keymap, return it; if a symbol, evaluate first."
  (if (keymapp map)
      map
    (if (symbolp map)
        (progn
          (when (local-variable-if-set-p map)
            (message "Keymap is buffer-local: %S" map))
          (let ((evaluated (symbol-value map)))
            (if (keymapp evaluated)
                evaluated
              (error "Doesn't evaluate to a keymap: %s" map))))
      (error "Not a keymap or keymap name: %s" map))))

;; Unused.  Not sure it's sane in the context of buffer-local variables.
(defun massmapper--raw-keymap-recursive (map)
  "If MAP is a keymap, return it; if a symbol, evaluate first.
If the symbol value is another symbol, evaluate again until it
results in a keymap."
  ;; Keep re-evaluating in case of indirection.  Rare, but happens: the value of
  ;; `iedit-occurrence-keymap' is another quoted symbol
  ;; `iedit-occurrence-keymap-default', until buffer-locally set to a child
  ;; keymap of that one's default value.
  (while (symbolp map)
    (setq map (symbol-value map)))
  (if (keymapp map)
      map
    (error "Doesn't evaluate to a keymap: %s" map)))


;;;; Handlers for key descriptions

(defun massmapper--last-key (single-key-or-chord)
  "Return the last key in SINGLE-KEY-OR-CHORD.
If it's not a chord, return the input unmodified.

USE WITH CARE.  Presupposes that the input has no spaces and has
been normalized by (key-description (kbd KEY))!"
  (declare (pure t) (side-effect-free t))
  (let ((s single-key-or-chord))
    (cond ((or (string-search "--" s)
               (string-match-p "-$" s)) ;; same thing given it's a single chord!
           "-")
          ((string-match-p "<$" s)
           "<")
          ((string-search "<" s)
           (save-match-data
             (string-match (rx "<" (* nonl) ">") s)
             (match-string 0 s)))
          (t
           (car (last (split-string s "-" t)))))))

(defun massmapper--key-contains-multi-chord (keydesc)
  "Check if KEYDESC has C-M- or such simultaneous chords.
Assumes KEYDESC was normalized by (key-description (kbd KEY))."
  (declare (pure t) (side-effect-free t))
  ;; assume keydesc was already normalized.
  (string-match-p (rx (= 2 (regexp massmapper--modifier-regexp)))
                  keydesc))

(defun massmapper--key-contains (symlist keydesc)
  "Check if any key in KEYDESC matches a member of SYMLIST.
Ignores modifier keys.

To check for shifted symbols such as capital letters, pass
`massmapper--all-shifted-symbols-list' as SYMLIST."
  (declare (pure t) (side-effect-free t))
  (->> (split-string keydesc " ")
       (-map #'massmapper--last-key)
       (-intersection symlist)))

(defun massmapper--key-has-more-than-one-chord (keydesc)
  "Return nil if KEYDESC has exactly one or zero chords.
otherwise always return t, even if the additional chords use the
same modifier.  In other words:

C-c C-o returns t
C-c M-o returns t
C-c o returns nil
<f1> o returns nil
<f1> C-f returns nil

Does not check for shifted symbols, such as capital letters.  For
that, see `massmapper--key-contains-any'."
  (declare (pure t) (side-effect-free t))
  (when-let ((first-modifier-pos
              (string-match-p massmapper--modifier-regexp-safe keydesc)))
    ;; We use +1 and not +2 b/c of the peculiarities of this regexp, but it
    ;; gets the job done.
    (string-match-p massmapper--modifier-regexp-safe
                    keydesc
                    (1+ first-modifier-pos))))

(defun massmapper--key-starts-with-modifier (keydesc)
  "Return t if KEYDESC starts with a modifier."
  (declare (pure t) (side-effect-free t))
  (when-let ((first-modifier-pos
              (string-match-p massmapper--modifier-regexp-safe keydesc)))
    (zerop first-modifier-pos)))

(defun massmapper--key-mixes-modifiers (keydesc)
  "Return t if KEYDESC has more than one kind of modifier.
Does not catch shiftsyms such as capital letters; to check for
those, see `massmapper--key-contains-any'.  Does catch e.g. C-S-RET."
  (declare (pure t) (side-effect-free t))
  (let ((case-fold-search nil)
        (first-match-pos (string-match-p massmapper--modifier-regexp-safe
                                         keydesc)))
    (when first-match-pos
      (let* (;; Compensate if `massmapper--modifier-regexp-safe' matched a dash
             ;; or space preceding the actual modifier.
             (first-match-pos (if (zerop first-match-pos)
                                  first-match-pos
                                (1+ first-match-pos)))
             (caught (substring keydesc first-match-pos (1+ first-match-pos)))
             (mods '("A" "C" "H" "M" "S" "s"))
             (now-forbidden-mods-regexp
              (concat "\\(-\\|^\\| \\)\\(["
                      (string-join (remove caught mods))
                      "]-\\)")))
        (string-match-p now-forbidden-mods-regexp keydesc)))))

(defun massmapper--key-seq-steps=1 (keydesc)
  "Does KEYDESC represent a single event rather than a sequence?
Tiny trivial function, but useful for `mapcar' and ilk."
  (declare (pure t) (side-effect-free t))
  (not (string-search " " keydesc)))

(defun massmapper--key-seq-steps-length (keydesc)
  "Length of key sequence KEYDESC.
Useful predicate for `seq-sort-by' or `cl-sort'."
  (declare (pure t) (side-effect-free t))
  (length (split-string keydesc " ")))

(defun massmapper--key-seq-is-permachord (keydesc)
  "If sequence KEYDESC has one chord on every step, return t.
This chord must be the same throughout the sequence."
  (declare (pure t) (side-effect-free t))
  (when (> (length keydesc) 2)
    (let* ((first-2-chars (substring keydesc 0 2))
           (root-modifier (when (string-suffix-p "-" first-2-chars)
                            first-2-chars)))
      (when root-modifier
        (not (cl-loop
              for step in (split-string keydesc " ")
              unless (and
                      (> (length step) 2)
                      (string-prefix-p root-modifier
                                       (substring step 0 2))
                      (not (string-match-p massmapper--modifier-regexp-safe
                                           (substring step 2))))
              return t))))))

(defun massmapper--stem-to-parent-keydesc (stem)
  "Return a valid key by trimming STEM from the right.
In practice, you'd use this to figure out the prefix key that
maps to the keymap implied by there being a stem here.  For
example, inputting a STEM of \"C-x \" returns \"C-x\"."
  (declare (pure t) (side-effect-free t))
  (if (string-suffix-p " " stem)
      (substring stem 0 -1)
    (if (string-match-p (concat massmapper--modifier-regexp "$") stem)
        (replace-regexp-in-string
         (rx " " (+ (regexp massmapper--modifier-regexp)) eol)
         ""
         stem)
      (error "Non-stem passed to `massmapper--stem-to-parent-keydesc': %s"
             stem))))

(defun massmapper--get-leaf (keydesc)
  "Return leaf key of KEYDESC."
  (declare (pure t) (side-effect-free t))
  (->> (split-string keydesc " ")
       (-last-item)
       (massmapper--last-key)))

(defun massmapper--drop-leaf (keydesc)
  "Chop the leaf off KEYDESC and return the resulting stem."
  (declare (pure t) (side-effect-free t))
  (replace-regexp-in-string (rx (literal (massmapper--get-leaf keydesc)) eol)
                            ""
                            keydesc))

(defun massmapper--parent-stem (stem)
  "Return a parent stem to STEM.
Note what is considered a parent.  The stem \"C-s C-\" is in some
information-theoretic sense nested more deeply than \"C-s \", but
speaking in terms of keymap nesting, they refer to the same
sub-keymap.  As such, both of these return the same parent: \"C-\"."
  (declare (pure t) (side-effect-free t))
  (if (or (= 2 (length stem))
          (massmapper--key-seq-steps=1 stem))
      nil
    (massmapper--drop-leaf (massmapper--stem-to-parent-keydesc stem))))

(defun massmapper--parent-key (keydesc)
  "Return immediate prefix of KEYDESC, or nil if it hasn't one."
  (declare (pure t) (side-effect-free t))
  (let ((steps (split-string keydesc " ")))
    (if (= 1 (length steps))
        ;; Return nil so functions like `-keep' can take advantage.
        nil
      (string-join (butlast steps) " "))))

(defun massmapper--prefix-to-stem (keydesc)
  "Add a space to the end of KEYDESC.
Trivial function, but useful for `mapcar' and friends."
  (declare (pure t) (side-effect-free t))
  (concat keydesc " "))


;;;; Stuff used only by massmapper, not by deianira

(defun massmapper--key-seq-has-non-prefix-in-prefix (keymap keydesc)
  "Is any prefix of KEYDESC bound to a command?
For example: C-x C-v is bound to a command by default, so if
you attempt to bind C-x C-v C-x to a command, you get an error.
Here we check for that.

If KEYDESC is for example C-x C-v C-x, return non-nil if either
C-x or C-x C-v are bound to a command.  Specifically, return the
first of those key descriptions.  If both of them are bound to
either nothing or a prefix map, it's okay, so return nil.

Note: Does not additionally check that the KEYDESC you passed is
not itself bound to a prefix map with children bound, but that's
another thing that can make it impossible to bind KEYDESC to a
command.  (You may have to unbind all the children first).

KEYMAP is the keymap in which to look."
  (let ((steps (split-string keydesc " "))
        (ret nil))
    (when (> (length steps) 1)
      ;; TODO: don't use dotimes, but some sort of "until" pattern.  This
      ;; function is very un-Lispy right now.  You can tell, because it takes
      ;; time to understand wtf it's doing.
      (dotimes (i (- (length steps) 1))
        (let ((subseq (string-join (-take (1+ i) steps) " ")))
          (when (massmapper--lookup keymap subseq t)
            (push subseq ret))))
      (car ret))))

(defun massmapper--root-modifier-chunk (keydesc)
  "Return the first modifier chunk in KEYDESC.
For example, if KEYDESC is C-M-x C-S-RET, return the substring
\"C-M-\".  If there are no modifiers on the first key, return
nil."
  (declare (pure t) (side-effect-free t))
  (let* ((first-item (car (split-string keydesc " ")))
         (last-mod-pos (save-match-data
                         (string-match
                          (rx bol (* (regexp massmapper--modifier-regexp)))
                          first-item)
                         (match-end 0))))
    (when (not (zerop last-mod-pos))
      (substring first-item 0 last-mod-pos))))
;; (massmapper--root-modifier-chunk "C-M-x C-S-RET")
;; (massmapper--root-modifier-chunk "x C-S-RET")

(defun massmapper--permachord-p (keydesc)
  "Non-nil if KEYDESC can be described as permachord.
Be permissive towards multi-chords: simply determine modifiers
from the first step and see if they're present on every step.
Also return nil if there's an additional modifier anywhere.

Assumes KEYDESC is a sequence, not a single key."
  (declare (pure t) (side-effect-free t))
  (when (> (length keydesc) 2)
    (let ((rootmod (massmapper--root-modifier-chunk keydesc)))
      (when rootmod
        (cl-loop
         for step in (split-string keydesc " ")
         if (or (not (string-prefix-p rootmod step))
                (string-match-p massmapper--modifier-regexp-safe
                                (substring step (length rootmod))))
         return nil
         else finally return t)))))

(defun massmapper--ensure-permachord (keydesc)
  "Return KEYDESC as perma-chord.
If it's already that, return it unmodified.

The key sequence KEYDESC must not contain any modifiers that are
not part of the first key in the sequence.  If it satisfies
`massmapper--is-chordonce' or `massmapper--permachord-p', or
dissatisfies `massmapper--key-mixes-modifiers', there'll be no problem."
  (declare (pure t) (side-effect-free t))
  (let ((rootmod (massmapper--root-modifier-chunk keydesc)))
    (if rootmod
        (string-join
         (cl-loop
          for step in (split-string keydesc " ")
          if (string-prefix-p rootmod step)
          collect step
          and do (when (string-match-p massmapper--modifier-regexp-safe
                                       (substring step (length rootmod)))
                   (error "Key contains other modifiers: %s" keydesc))
          else collect (concat rootmod step)
          and do (when (string-match-p massmapper--modifier-regexp-safe step)
                   (error "Key contains other modifiers: %s" keydesc)))
         " ")
      (warn "massmapper--ensure-permachord probably shouldn't be called on: %s"
            keydesc)
      keydesc)))

(defun massmapper--ensure-chordonce (keydesc)
  "Strip chords from most of key sequence KEYDESC.
Leave alone the first step of the key sequence.  Technically,
since we don't check the first step, the resulting sequence could
be entirely unchorded."
  (declare (pure t) (side-effect-free t))
  (let ((steps (split-string keydesc " ")))
    (string-join (cons (car steps)
                       (-map #'massmapper--get-leaf (cdr steps)))
                 " ")))

;; it occurs to me this was unnecessary to split up, but well, at least it's
;; recomposable code
(defun massmapper--modernize (keydesc)
  (massmapper--modernize-tab (massmapper--modernize-ret keydesc)))

(defun massmapper--modernize-tab (keydesc)
  "Replace C-i with <tab> in KEYDESC.
Takes care of all possible permutations, such as C-M-i, C-S-i,
C-s-i, A-C-H-M-S-s-i..., as well as when TAB is printed
instead of C-i."
  (declare (pure t) (side-effect-free t))
  (string-join
   (save-match-data
     (cl-loop
      for bit in (split-string keydesc " " t)
      ;; Special case: S-<tab> doesn't exist, instead it's named <backtab> or
      ;; S-<iso-lefttab>.  Another special case is that in principle we could
      ;; encounter a key C-I as an alias for C-S-i, but fortunately Control keys
      ;; are always case-insensitive so I figure Emacs won't ever print out a
      ;; keymap as having C-I bound.
      if (and (string-search "C-" bit)
              (string-search "S-" bit)
              (string-suffix-p "i" bit))
      collect (concat (string-replace "C-" "" (substring bit 0 -1)) "<iso-lefttab>")
      else if (and (string-search "C-" bit)
                   (string-suffix-p "i" bit))
      collect (concat (string-replace "C-" "" (substring bit 0 -1)) "<tab>")
      else if (and (string-search "S-" bit)
                   (string-suffix-p "TAB" bit))
      collect (concat (substring bit 0 -3) "<iso-lefttab>")
      else if (string-suffix-p "TAB" bit)
      collect (concat (substring bit 0 -3) "<tab>")
      else collect bit))
   " "))

(defun massmapper--modernize-ret (keydesc)
  "Replace C-m with <return> in KEYDESC.
Takes care of all possible permutations, such as C-M-m, C-H-m,
C-s-m, C-S-m, A-C-H-M-S-s-m..., as well as when RET is printed
instead of C-m."
  (declare (pure t) (side-effect-free t))
  (string-join
   (save-match-data
     (cl-loop
      for bit in (split-string keydesc " " t)
      if (and (string-search "C-" bit)
              (string-suffix-p "m" bit))
      collect (concat (string-replace "C-" "" (substring bit 0 -1)) "<return>")
      else if (string-suffix-p "RET" bit)
      collect (concat (substring bit 0 -3) "<return>")
      else collect bit))
   " "))

;; (define-key org-mode-map "<tab>" (lookup-key org-mode-map "TAB"))
;; (define-key org-mode-map "<tab>" (keymap-lookup org-mode-map "TAB"))
;; (massmapper--modernize "TAB")
;; (global-set-key (kbd "S-<iso-lefttab>") #'embark-act)
;; (global-set-key (kbd "S-TAB") #'embark-act)


;; TODO: Maybe test reimplementing kmu-lookup-local-key (which strips the
;; parents from MAP). Could it fix some errors / strange behaviors?
;;
;; https://github.com/tarsius/keymap-utils/blob/main/keymap-utils.el
;;
;; Could be cool to make an experimental branch that uses as many kmu functions
;; as possible. Then talk to tarsius about maybe merging with some of massmapper-lib.
;; (require 'keymap-utils)



;; having S- instead of a capital is disallowed!
;; (lookup-key global-map (key-parse "C-S-x <return> p")) ;; invalid
;; (lookup-key global-map (key-parse "C-X <return> p")) ;; valid
;; and yet... (key-valid-p "C-S-x <return> p").  so key-valid-p accepts some
;; things that lookup-key won't.

;; and then this pair... notably, even though "s-S-<backspace>" fails
;; key-valid-p, lookup-key will cope with it just fine.

;; (lookup-key global-map (key-parse "s-S-<backspace>")) ;; valid
;; (lookup-key global-map (key-parse "s-S-<backspace> p")) ;; invalid
(defun massmapper--normalize (keydesc)
  "Reform KEYDESC to pass both `key-valid-p' and `lookup-key'.
Assumes KEYDESC was output by `key-description', which
already normalizes some aspects of it."
  (declare (pure t) (side-effect-free t))
  (save-match-data
    (string-join
     (cl-loop
      with case-fold-search = nil
      for step in (split-string keydesc " " t)
      as last-key = (massmapper--last-key step)
      as chords = (substring step 0 (- (length last-key)))
      collect
      (progn
        ;; Replace S- with a capital if the last key is alphabetic
        (when (and (string-search "S-" chords)
                   (and (string-match-p "^[[:alpha:]]+$" last-key)
                        (not (string-match-p "[[:upper:]]" last-key))))
          (setq chords (string-replace "S-" "" chords))
          (setq last-key (upcase last-key)))
        ;; Reorder modifiers so they follow A-C-H-M-S-s.  Key insight: that's
        ;; just an alphabetic sort.
        (setq chords
              (thread-first chords
                            (string-split "-" t)
                            (sort #'string-lessp)
                            (string-join "-")))
        (concat chords (unless (string-empty-p chords) "-") last-key)))
     " ")))

;; (massmapper--normalize "s-S-<backspace>")
;; (massmapper--normalize "H-A-C-S-i")
;; (massmapper--normalize "s-c H-A-p 4 a")

;; TODO: Deprecate.  It's mainly for debugging; now that I've understood things,
;; can just wrap keymap-lookup in ignore-errors.  Although what if there's a
;; numeric return at some point?  Guess it's still more idiomatic to check at
;; the caller.
(cl-defun massmapper--lookup (map key &optional no-warn-numeric no-warn-invalid)
  "Variant of Emacs 29 `keymap-lookup', with different behavior."
  (declare (compiler-macro (lambda (form) (keymap--compile-check key) form)))

  (unless no-warn-invalid
    ;; Instead of signaling an error, prefer to keep trying to bind most
    ;; bindings, since most attempts usually work.  Giving up at the first error
    ;; can leave an user stranded without the keys they're used to.
    (with-demoted-errors "%s"
      (keymap--check key)))

  (let* ((raw-map (massmapper--raw-keymap map))
         ;; (raw-map (kmu--strip-keymap (massmapper--raw-keymap map)))
         (value (lookup-key raw-map (key-parse key))))
    ;; It's good for debugging to know when `lookup-key' returns numeric, so warn.
    (if (numberp value)
        (prog1 nil
          (unless no-warn-numeric
            (warn "Massmapper: lookup-key returned numeric value for %s in %S"
                  key
                  (if (symbolp map)
                      map
                    (help-fns-find-keymap-name map)))))
      ;; an oversight in upstream: didn't pass keymap to `command-remapping'
      (or (command-remapping value nil raw-map) value))))

(provide 'massmapper-lib)

;;; massmapper-lib.el -- Library for deianira and massmap -*- lexical-binding: t; nameless-current-name: "massmapper"; -*-

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
(eval-when-compile (require 'cl-lib))

(defconst massmapper--ignore-keys-control-chars
  '("ESC" "C-["
    "RET" "C-m"
    "TAB" "C-i"))

(defconst massmapper--ignore-keys-irrelevant
  '("compose" "Scroll_Lock" "drag-n-drop" "help"
    "mouse" "remap" "scroll-bar" "select" "switch" "state"
    "which-key" "corner" "divider" "edge" "header" "mode-line"
    "vertical-line" "frame" "open" "chord" "tool-bar" "fringe"
    "touch" "margin" "pinch" "tab-bar" )
  "List of strings matching key events unlikely to matter to the
user's keyboard setup.")

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
  "Return t if kEYDESC starts with a modifier."
  (declare (pure t) (side-effect-free t))
  (when-let ((first-modifier-pos
              (string-match-p massmapper--modifier-regexp-safe keydesc)))
    (zerop first-modifier-pos)))

(defun massmapper--key-mixes-modifiers (keydesc)
  "Return t if KEYDESC has more than one kind of modifier.
Does not catch shiftsyms such as capital letters; to check for
those, see `massmapper--key-contains-any'.  Does catch e.g. C-S-<RET>."
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
For example: C-x C-v is bound to a simple command by default, so if
you attempt to bind C-x C-v C-x to a command, you get an error.
So here we check for that.

If KEYDESC is for example C-x C-v C-x, return non-nil if either
C-x or C-x C-v are bound to a command.  If both of them are bound
to either nothing or a prefix map, it's okay, so return nil.

Does not additionally check that KEYDESC is not itself a prefix
map with children bound: that's another thing that can make KEYDESC
unbindable.

KEYMAP is the keymap in which to look."
  (let ((steps (split-string keydesc " "))
        (ret nil))
    (when (> (length steps) 1)
      ;; TODO: don't use dotimes, but some sort of "until" pattern.  This
      ;; function is very un-Lispy right now.  You can tell, because it takes
      ;; time to understand wtf it's doing.
      (dotimes (i (- (length steps) 1))
        (let ((subseq (string-join (-take (1+ i) steps) " ")))
          (when (lookup-key-ignore-too-long keymap (kbd subseq))
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

(provide 'massmapper-lib)

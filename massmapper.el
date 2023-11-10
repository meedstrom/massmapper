;;; massmapper.el --- Remap keys en masse -*- lexical-binding: t -*-

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

;; Author:  <meedstrom91@gmail.com>
;; Created: 2018-08-03
;; Version: 0.1.2-snapshot
;; Keywords: convenience
;; Homepage: https://github.com/meedstrom/massmapper
;; Package-Requires: ((emacs "24.4") (dash "2.19.1") (compat "29.1.4.3"))

;;; Commentary:

;; Setup may go like this:

;; (require 'massmapper)
;; (add-hook 'after-init-hook #'massmapper-mode)
;; (add-hook 'massmapper-keymap-found-hook #'massmapper-define-super-like-ctl)
;; (add-hook 'massmapper-keymap-found-hook #'massmapper-homogenize -50))

;;; Code:

(require 'massmapper-lib)
(require 'dash)
(require 'compat)
(require 'cl-lib)
(require 'subr-x)
(require 'help-fns) ;; help-fns-find-keymap-name
(require 'map) ;; map-let

;; REVIEW: Verify that command remappings apply

;; TODO: What to do about C-c C-x p  (org-set-property)?
;;       Because there is also C-c x p which copies to C-c C-x C-p.
;;       Maybe the user should be alerted to bastard sequences.

;; TODO: Promote error signals, which currently get demoted to a "Error during
;;       redisplay" message (because of where we put the hook)

;; TODO: Test binding keyboard macros and lambdas, and see if they get copied.
;;       I don't never bind either, so it's worth a look.
;;       https://www.gnu.org/software//emacs/manual/html_node/elisp/Key-Lookup.html
;;       A keyboard macro is represented as a string or vector, a lambda is a
;;       list with the symbol lambda as car.

;; TODO: Handle capitals and Shift just as well as any other mod


;;; Basics

(defgroup massmapper nil
  "Remap keys en masse."
  :group 'keyboard)

(defcustom massmapper-debug-level 0
  "Verbosity of debug.  0, 1, or 2."
  :type 'integer
  :group 'massmapper)

(defvar massmapper--remap-record nil
  "Record of work done.")

(define-derived-mode massmapper-list-remaps-mode tabulated-list-mode
  "Remaps List"
  nil
  (setq tabulated-list-format
        [("Keymap" 20 t)
         ("Reason" 45 t)
         ("Key bound" 15 t)
         ("New command" 20 t)
         ("Old command" 20 t)])
  (add-hook 'tabulated-list-revert-hook #'massmapper-list-remaps nil t)
  (tabulated-list-init-header))

(defun massmapper--pretty-print-def (def)
  "Return a string that tries to say what DEF refers to.
  DEF should be a key definition such as that returned by
  `lookup-key'; most of the time, it's a command, and then this
  function simply returns its name."
  (cond ((symbolp def)
         (symbol-name def))
        ((keymapp def)
         (let ((named (help-fns-find-keymap-name def)))
           (if named
               (symbol-name named)
             "(some sub-keymap)")))
        ((functionp def)
         "Anonymous lambda")
        ((listp def)
         "Likely a menu item")
        ((stringp def)
         def)
        (t
         (error "Unable to identify: %s" def))))

(defun massmapper-list-remaps ()
  "List the key-bindings made so far by massmapper."
  (interactive)
  (let ((buf (get-buffer-create "*Massmapper remaps*")))
    (with-current-buffer buf
      (setq tabulated-list-entries nil)
      (massmapper-list-remaps-mode)
      (cl-loop
       for item in massmapper--remap-record
       do
       ;; (cl-destructuring-bind (&key keydesc cmd map reason olddef) item)
       (map-let ((:keydesc keydesc) (:cmd cmd) (:map map) (:reason reason) (:olddef oldcmd)) item
         (let ((cmd-string (massmapper--pretty-print-def cmd))
               (oldcmd-string
                (if oldcmd
                    (concat "(was " (massmapper--pretty-print-def oldcmd)
                            ")")
                  "")))
           (push (list (sxhash item)
                       (vector (symbol-name map)
                               reason
                               keydesc
                               cmd-string
                               oldcmd-string))
                 tabulated-list-entries))))
      (tabulated-list-print)
      (display-buffer buf))))

;; TODO: also copy any associated repeat-map...
(defun massmapper-execute (actions)
  "Carry out remaps specified by ACTIONS."
  (while actions
    (let ((action (pop actions)))
      (if (member action massmapper--remap-record)
          (when (> massmapper-debug-level 1)
            (message "Massmapper already took this action: %S" action))
        (map-let ((:keydesc keydesc) (:cmd cmd) (:map map)) action
          (let ((raw-map (massmapper--raw-keymap map)))
            (when-let* ((conflict-prefix
                         (massmapper--key-seq-has-non-prefix-in-prefix
                          raw-map keydesc))
                        (conflict-def (massmapper--lookup map conflict-prefix t)))
              (unless (keymapp conflict-def)
                ;; If the prefix is bound and it's not to a keymap, unbind the
                ;; prefix so we'll be allowed to bind our key. (prevent error
                ;; "Key sequence starts with non-prefix key")
                (keymap-unset raw-map conflict-prefix t)
                ;; In fact, create a keymap so that `lookup-key' won't whine
                ;; later (by returning a numeric value).  Hopefully.
                ;; REVIEW: Necessary?  If so, maybe -non-prefix-in-prefix should
                ;; return every prefix so we can do this for each.  Or just the
                ;; longest prefix -- I believe the parents create themselves.
                (keymap-set raw-map conflict-prefix (make-sparse-keymap))))
            (if (null cmd)
                (progn
                  (message "Massmapper unbound %s in %S (was %s)"
                           keydesc map cmd)
                  (keymap-unset raw-map keydesc t))
              (keymap-set raw-map keydesc cmd))))
        (push action massmapper--remap-record)))))

(defun massmapper-revert ()
  "Experimental command to undo all remaps made.
  It's recommended to just restart Emacs, but this might work.

  It won't restore everything: it's likely a few prefix keys
  were unbound and will stay unbound."
  (interactive)
  (cl-loop
   for item in massmapper--remap-record
   do (map-let ((:keydesc keydesc) (:map map) (:olddef olddef)) item
        (if (null olddef)
            (keymap-unset (massmapper--raw-keymap map) keydesc t)
          (keymap-set (massmapper--raw-keymap map) keydesc olddef)))
   finally do
   (setq massmapper--remap-record nil)
   (setq massmapper--homogenized-keymaps nil)
   (setq massmapper--reflected-maps-per-mod nil)
   (setq massmapper--keymaps-conserving-tabret nil)))

(defcustom massmapper-keymap-found-hook nil
  "Run after adding one or more keymaps to `massmapper--known-keymaps'.
  See `massmapper-record-keymap-maybe', which triggers this hook.

  You may be interested in hooking some of these functions:

  - `massmapper-homogenize'
  - `massmapper-define-super-like-ctl'
  - `massmapper-define-super-like-ctlmeta'
  - `massmapper-define-super-like-meta'
  - `massmapper-define-alt-like-ctl'
  - `massmapper-define-alt-like-ctlmeta'
  - `massmapper-define-alt-like-meta'
  - `massmapper-define-hyper-like-ctl'
  - `massmapper-define-hyper-like-ctlmeta'
  - `massmapper-define-hyper-like-meta'

  or some of these EXPERIMENTAL! functions:

  - `massmapper-define-althyper-like-ctlmeta'
  - `massmapper-define-altsuper-like-ctlmeta'
  - `massmapper-define-hypersuper-like-ctlmeta'
  - `massmapper-conserve-ret-and-tab'"
  :type 'hook
  :options '(massmapper-homogenize
             massmapper-define-super-like-ctl
             massmapper-define-super-like-ctlmeta
             massmapper-define-super-like-meta
             massmapper-define-alt-like-ctl
             massmapper-define-alt-like-ctlmeta
             massmapper-define-alt-like-meta
             massmapper-define-hyper-like-ctl
             massmapper-define-hyper-like-ctlmeta
             massmapper-define-hyper-like-meta
             massmapper-define-althyper-like-ctlmeta
             massmapper-define-altsuper-like-ctlmeta
             massmapper-define-hypersuper-like-ctlmeta
             massmapper-conserve-ret-and-tab)
  :group 'massmapper)

(defvar massmapper--known-keymaps '(global-map)
  "List of named keymaps seen active.
  This typically gets populated (by `massmapper-record-keymap-maybe') with
  just mode maps, rarely (never?) those used as transient maps and never
  so-called prefix commands like `Control-X-prefix', nor the category
  of sub-keymaps like `ctl-x-map' or `help-map.'")

(defvar massmapper--known-keymap-composites nil
  "List of unique keymap composites seen active.
  These are identified by their hashes; each one was a product
  of (abs (sxhash (current-active-maps))), called in different
  places and times.")

(defun massmapper-record-keymap-maybe (&optional _)
  "If Emacs has seen new keymaps, record them in a variable.
  This simply checks the output of `current-active-maps' and adds
  to `massmapper--known-keymaps' anything not already added.  Every time
  we find one or more new keymaps, trigger `massmapper-keymap-found-hook'.

  Suitable to hook on `window-buffer-change-functions' like this:

  \(add-hook 'window-buffer-change-functions #'massmapper-record-keymap-maybe)"
  (let* ((maps (current-active-maps))
         (composite-hash (abs (sxhash maps))))
    ;; Make sure we only iterate the expensive `help-fns-find-keymap-name' once
    ;; for this keymap composite.
    (unless (member composite-hash massmapper--known-keymap-composites)
      (push composite-hash massmapper--known-keymap-composites)
      (let* ((named-maps (-uniq (-keep #'help-fns-find-keymap-name maps)))
             (new-maps (-difference named-maps massmapper--known-keymaps)))
        (setq new-maps (remove 'context-menu-mode-map new-maps))
        ;; For whatever reason, (help-fns-find-keymap-name global-map) returns
        ;; `widget-global-map'.  Try it!  Fortunately,
        ;; (equal widget-global-map global-map) returns t, so it doesn't matter
        ;; for most purposes, as edits to the raw value of one also affects the
        ;; raw value of the other (keymap variables are weird that way).
        ;; However, where we want to identify the keymap by e.g. membership in a
        ;; list or with `equal', I expect it to take much more CPU time to
        ;; compare their raw values instead of simply their symbols.  The user
        ;; is likely to set options that refer to `global-map'.  Therefore,
        ;; ensure we use that name everywhere by prepopulating
        ;; `massmapper--known-keymaps' with `global-map' and removing
        ;; `widget-global-map' in this step.  As a bonus, user won't need to see
        ;; and be puzzled by `widget-global-map' in M-x massmapper-list-remaps.
        ;; These shenanigans wouldn't be necessary if keymap values contained a
        ;; canonical symbol name (and we could eliminate the expensive function
        ;; `help-fns-find-keymap-name'), but IDK if upstream would like that.
        (setq new-maps (remove 'widget-global-map new-maps))

        ;; Rare situation, and maybe it's only iedit that has this hack, but the
        ;; default value of `iedit-occurrence-keymap' is the symbol
        ;; `iedit-occurrence-keymap-default', and we should work strictly with
        ;; the latter.  I assume we should do likewise all similar cases, so
        ;; just recursively evaluate all such "indirect variables".  Especially
        ;; in that example, it's appropriate since it additionally has the odd
        ;; behavior that the former is a buffer-local variable that sometimes
        ;; actually evaluates to a (buffer-local) keymap instead of a symbol,
        ;; which is why it's sometimes picked up by `current-active-maps'.  Were
        ;; Elisp statically typed, we wouldn't ever have to watch for strange
        ;; usages like this.
        (setq new-maps
              (cl-loop for map in new-maps
                       collect (cl-loop
                                until (keymapp (default-value map))
                                do (setf map (default-value map))
                                finally return map)))
        ;; After the above de-hack, we must re-check.
        (setq new-maps (-difference new-maps massmapper--known-keymaps))

        (when new-maps
          (setq massmapper--known-keymaps
                (append new-maps massmapper--known-keymaps))
          (run-hooks 'massmapper-keymap-found-hook))))))


;;; Cleaning
;; Unused for now.  No point to purging ugly key bindings except that it
;; declutters which-key popups and can make Deianira build hydras a bit
;; faster.

;; (defvar massmapper--cleaned-maps nil)

;; (defvar massmapper--clean-actions nil)

;; (defun massmapper-unbind-illegal-keys ()
;;   "Push keys to unbind onto `massmapper--clean-actions'."
;;   (cl-loop
;;    for map in (-difference massmapper--known-keymaps massmapper--cleaned-maps)
;;    with doom = (and (bound-and-true-p doom-leader-alt-key)
;;                     (bound-and-true-p doom-localleader-alt-key))
;;    do (push
;;        (cons map
;;              (cl-sort
;;               (cl-loop
;;                for x being the key-seqs of (massmapper--raw-keymap map)
;;                as key = (key-description x)
;;                when (and
;;                      (not (string-match-p massmapper--ignore-keys-regexp key))
;;                      (or (dei--key-is-illegal key)
;;                          ;; I don't want to touch these, I want to see what
;;                          ;; Doom does with them.
;;                          (when doom
;;                            (or (string-prefix-p doom-localleader-alt-key key)
;;                                (string-prefix-p doom-leader-alt-key key)))))
;;                collect key)
;;               #'> :key #'massmapper--key-seq-steps-length))
;;        massmapper--clean-actions)))


;;; Reflecting one stem in another

(defun massmapper--how-define-a-like-b-in-keymap (recipient-mod donor-mod map)
  "Return actions needed to clone one set of keys to another set.
  Inside keymap MAP, take all keys and key sequences that contain
  DONOR-MOD \(a substring such as \"C-\"\), replace the substring
  wherever it occurs in favor of RECIPIENT-MOD \(a substring such
  as \"H-\"\), and assign them to the same commands.

  Key sequences that already contain RECIPIENT-MOD are ignored,
  even if they also contain DONOR-MOD."
  (cl-loop
   with actions = nil
   with case-fold-search = nil
   with reason = (concat "Define " recipient-mod " like " donor-mod)
   for vec being the key-seqs of (massmapper--raw-keymap map)
   using (key-bindings cmd)
   as key = (massmapper--normalize (key-description vec))
   when (and cmd
             (string-search donor-mod key)
             (not (string-search recipient-mod key)))
   do (let ((recipient (massmapper--normalize
                        (string-replace donor-mod recipient-mod key))))
        (if (massmapper--lookup map recipient t)
            (and (> massmapper-debug-level 0)
                 (message "User bound key, leaving it alone: %s in %S"
                          recipient
                          map))
          (push (list :keydesc recipient
                      :cmd cmd
                      :map map
                      :reason reason
                      :olddef nil)
                actions)))
   finally return actions))

(defvar massmapper--reflected-maps-per-mod nil)

(defun massmapper--define-a-like-b-everywhere (recipient-mod donor-mod)
  "Copy all bindings starting with DONOR-MOD to RECIPIENT-MOD."
  (cl-loop
   for map in (-difference massmapper--known-keymaps
                           (alist-get (intern recipient-mod)
                                      massmapper--reflected-maps-per-mod))
   as start = (current-time)
   as actions = (massmapper--how-define-a-like-b-in-keymap
                 recipient-mod donor-mod map)
   when actions do
   (massmapper-execute actions)
   (when (> massmapper-debug-level 0)
     (message
      "(In %.3fs) Copied keys from %s to %s in %S: %d"
      (float-time (time-since start))
      donor-mod
      recipient-mod
      map
      (length actions)))
   do (push map (alist-get (intern recipient-mod)
                           massmapper--reflected-maps-per-mod))))

(defun massmapper-define-super-like-ctl ()
  "Copy all Control bindings so they exist also on Super."
  (massmapper--define-a-like-b-everywhere "s-" "C-"))

(defun massmapper-define-super-like-ctlmeta ()
  "Copy all Control-Meta bindings so they exist also on Super."
  (massmapper--define-a-like-b-everywhere "s-" "C-M-"))

(defun massmapper-define-super-like-meta ()
  "Copy all Meta bindings so they exist also on Super."
  (massmapper--define-a-like-b-everywhere "s-" "M-"))

(defun massmapper-define-hyper-like-ctl ()
  "Copy all Control bindings so they exist also on Hyper."
  (massmapper--define-a-like-b-everywhere "H-" "C-"))

(defun massmapper-define-hyper-like-ctlmeta ()
  "Copy all Control-Meta bindings so they exist also on Hyper."
  (massmapper--define-a-like-b-everywhere "H-" "C-M-"))

(defun massmapper-define-hyper-like-meta ()
  "Copy all Meta bindings so they exist also on Hyper."
  (massmapper--define-a-like-b-everywhere "H-" "M-"))

(defun massmapper-define-alt-like-ctl ()
  "Copy all Control bindings so they exist also on Alt."
  (massmapper--define-a-like-b-everywhere "A-" "C-"))

(defun massmapper-define-alt-like-ctlmeta ()
  "Copy all Control-Meta bindings so they exist also on Alt."
  (massmapper--define-a-like-b-everywhere "A-" "C-M-"))

(defun massmapper-define-alt-like-meta ()
  "Copy all Meta bindings so they exist also on Alt."
  (massmapper--define-a-like-b-everywhere "A-" "M-"))

;; The following three technically have the problem of e.g. not picking up
;; C-H-M- bindings as a C-M- binding because they have a H in between, but it
;; will affect very little.

(defun massmapper-define-althyper-like-ctlmeta ()
  "Experimental!
  Copy all Control-Meta bindings so they exist also on Meta-Super."
  (massmapper--define-a-like-b-everywhere "A-H-" "C-M-"))

(defun massmapper-define-altsuper-like-ctlmeta ()
  "Experimental!
  Copy all Control-Meta bindings so they exist also on Meta-Super."
  (massmapper--define-a-like-b-everywhere "A-s-" "C-M-"))

(defun massmapper-define-hypersuper-like-ctlmeta ()
  "Experimental!
  Copy all Control-Meta bindings so they exist also on Meta-Super."
  (massmapper--define-a-like-b-everywhere "H-s-" "C-M-"))


;;; Lightweight alternative to Super as Ctl: sanitize some control chars

(defvar massmapper--keymaps-conserving-tabret nil)

(defcustom massmapper-Cm-Ci-override nil
  "Alist of bindings to bind after running `massmapper-conserve-ret-and-tab'.
  The alist should follow this structure:

  \(\(KEYMAP . \(\(KEY . COMMAND)
\(KEY . COMMAND)
...))
\(KEYMAP . \(\(KEY . COMMAND)
\(KEY . COMMAND)
...))
...)

After `massmapper-conserve-ret-and-tab' has operated on a given KEYMAP,
it will apply the bindings in the associated sublist -- i.e. bind
each KEY to COMMAND.

These KEYs can technically be any key, but there's no reason to
put in any keys that don't involve C-m or C-i."
  :group 'massmapper
  :type '(alist
          :key-type (symbol :tag "Keymap")
          :value-type (alist :key-type key
                             :value-type (sexp :tag "Command"))))

(defun massmapper--last-step (keydesc)
  (declare (pure t) (side-effect-free t))
  (replace-regexp-in-string ".* " "" keydesc))

(defun massmapper--first-step (keydesc)
  (declare (pure t) (side-effect-free t))
  (replace-regexp-in-string " .*" "" keydesc))

(defun massmapper--ends-in-ret (keydesc)
  (or (string-suffix-p "RET" keydesc)
      (let ((last (massmapper--last-step keydesc)))
        (and (string-suffix-p "m" last)
             (string-search "C-" last)))))

(defun massmapper--ends-in-tab (keydesc)
  (or (string-suffix-p "TAB" keydesc)
      (let ((last (massmapper--last-step keydesc)))
        (and (string-suffix-p "i" last)
             (string-search "C-" last)))))

(defun massmapper--how-conserve-ret-and-tab (map)
  "Actions for conserving Return and Tab behavior in MAP."
  (cl-loop
   with case-fold-search = nil
   with raw-map = (massmapper--raw-keymap map)
   for vec being the key-seqs of raw-map using (key-bindings cmd)
   as key = (massmapper--normalize (key-description vec))
   as modernized-key = (massmapper--modernize key)
   as action =
   (unless (equal modernized-key key)
     (let ((olddef (massmapper--lookup map modernized-key)))
       (unless (equal cmd olddef)
         (list :keydesc modernized-key
               :cmd (if (equal cmd #'self-insert-command)
                        (if (massmapper--ends-in-tab key)
                            #'massmapper-ins-tab
                          (if (massmapper--ends-in-ret key)
                              #'massmapper-ins-ret
                            cmd)
                          cmd)
                      cmd)
               :map map
               :reason "Conserve behavior of Return & Tab keys"
               :olddef olddef))))
   when action collect action))

(defun massmapper--how-rebind-Cm-Ci (map)
  "Actions for applying `massmapper-Cm-Ci-override' to MAP."
  (cl-loop
   with raw-map = (massmapper--raw-keymap map)
   for (key . cmd) in (alist-get map massmapper-Cm-Ci-override)
   collect (list :keydesc key
                 :cmd cmd
                 :map map
                 :reason "User specified in massmapper-Cm-Ci-override"
                 :olddef (massmapper--lookup map key))))

;; Note that the following two commands come into play less often than you
;; might think.
(defun massmapper-ins-ret (&optional arg)
  "Ersatz self-insert-command for <return>."
  (interactive "P")
  (newline arg t))

(defun massmapper-ins-tab (&optional arg)
  "Ersatz self-insert-command for <tab>."
  (interactive "P")
  (insert-tab arg))

;; (global-set-key (kbd "<tab>") #'self-insert-command)
;; (global-set-key (kbd "<tab>") #'massmapper-ins-tab)
;; (global-set-key (kbd "<tab>") #'indent-for-tab-command)

;; EXPERIMENTAL
;; TODO: vertico-map binds TAB but not <tab>, why isn't it getting bound?
(defun massmapper-conserve-ret-and-tab ()
  "Experimental.
In every keymap, look for keys involving C-m or RET, and C-i or
TAB, and copy their bindings onto the keys that instead involve
the function keys <return> and <tab>.  This permits you to then
bind C-m and C-i to other commands under GUI Emacs without
clobbering the Return and Tab keys' behavior.

Be warned: on the TTY, they would still get clobbered.

Note that you have to defer binding your shiny new C-m and C-i
commands by specifying them in `massmapper-Cm-Ci-override',
instead of calling `define-key' yourself."
  (cl-loop
   for map in (-difference massmapper--known-keymaps
                           massmapper--keymaps-conserving-tabret)
   as start = (current-time)
   as actions = (massmapper--how-conserve-ret-and-tab map)
   when actions do
   (massmapper-execute actions)
   (when (> massmapper-debug-level 0)
     (message "(In %.3fs) Conserved RET and TAB in %S: %d new bindings"
              (float-time (time-since start))
              map
              (length actions)))
   do
   (push map massmapper--keymaps-conserving-tabret)
   (let* ((rebind-start (current-time))
          (rebind-actions (massmapper--how-rebind-Cm-Ci map))
          (rebind-overwrites (cl-loop for action in rebind-actions
                                      when (plist-get action :olddef)
                                      count action)))
     (when rebind-actions
       (massmapper-execute rebind-actions)
       (when (> massmapper-debug-level 0)
         (message "(In %.3fs) Bound keys per -Cm-Ci-override in %S: %d new bindings and %d overwrites"
                  (float-time (time-since rebind-start))
                  map
                  (- (length rebind-actions) rebind-overwrites)
                  rebind-overwrites))))))

;; To test:
;; (setq foo (massmapper--how-conserve-ret-and-tab 'global-map))
;; (setq bar (massmapper--how-conserve-ret-and-tab 'vertico-map))
;; (massmapper-execute foo)
;; (massmapper-execute bar)

(define-obsolete-variable-alias 'massmapper-protect-ret-and-tab
  'massmapper-conserve-ret-and-tab "2023-11-06")


;;; Homogenizing

(defcustom massmapper-homogenizing-winners '()
  "Alist of keys that always win the homogenizing battle.
Normally, the behavior of a chord-once sequence such as C-x k e
will be kept as is, and cloned to the perma-chord sequence
C-x C-k C-e, overwriting any binding there, so they both do what
the former always did.

If this list contains the member (\"C-x C-k C-e\"), the opposite
will happen in that particular case, overwriting C-x k e.

Each item in the list has the format (KEY-OR-COMMAND . KEYMAP).

KEY-OR-COMMAND can be either a `kbd'-compatible key description
or a symbol assumed to refer to a command.  In the event that two
items conflict, the first item wins.

Let's take an example.  Assume that
- C-x f is bound to `set-fill-column',
- C-x C-f is bound to `find-file',

and assume you add both (\"C-x C-f\") and (set-fill-column) to
this list.  What happens? If (\"C-x C-f\") came first, then both
C-x f and C-x C-f will be bound to `find-file'.
If (set-fill-column) came first, then both C-x f and C-x C-f will
be bound to `set-fill-column'.

Moving on.

If the optional cdr KEYMAP is left at nil, make KEY-OR-COMMAND
win in all keymaps in which it exists.  If non-nil, make it win
only inside KEYMAP.

KEYMAP should be a major or minor mode map.  It will likely have
no effect if it is a so-called named prefix-command such as
Control-X-prefix or kmacro-keymap (you can find these with
`describe-function', whereas you can't find org-mode-map, as
that's a proper mode map)."
  :type '(repeat (cons (sexp :tag "Key sequence (in quotes) or command (unquoted)")
                       (symbol :tag "Keymap (nil means all keymaps)")))
  :group 'massmapper
  :set
  (lambda (var new)
    (set-default
     var (cl-loop for cell in new
                  collect (cons (if (stringp (car cell))
                                    (key-description (key-parse (car cell)))
                                  (car cell))
                                (cdr cell))))))

(defun massmapper--nightmare-p (keydesc)
  "Non-nil if homogenizing KEYDESC can cause bugs.
This has to do with e.g. C-x \[ being duplicated to C-x C-\[,
which is the same as C-x ESC, which is the same as
C-x M-anything.  You do this, then Magit tries to bind C-x M-g
and complains it's not a prefix key and you can't even load
Magit.  That's a mild issue.  A more silent bug is C-x i becoming
C-x C-i which means it overrides your C-x TAB binding, something
you're blissfully unaware of until you try to use C-x TAB and
have no idea why it isn't bound to what you thought.

The root problem is anachronistic Unix control character
behavior, which Emacs has chosen not to deprecate, for the sake
of functioning inside basic terminal emulators, TTYs and ssh
connections.  We have a clean solution in
`massmapper-define-super-like-ctl' and never pressing the Control key
again in our lives.  Alternatively, we have an untested partial
solution in `massmapper-conserve-ret-and-tab'.

If you don't apply the solution, it pays to know this: always
bind the function key <tab> instead of the control character TAB,
<return> instead of RET, and <escape> instead of ESC.  GUI Emacs
always looks up the function key if bound, and only falls back to
the control character if the function key is unbound.  The
function keys may not work on the terminal/TTY, but neither do
Super, Hyper or many other niceties, and I recommend just using
chemacs to run a barebone Emacs for the odd time you're on the
TTY.

While it is possible to rescue C-i and C-m from the mummified
hands of Unix, you cannot ever use C-\[ as anything other than an
ESC shorthand.  As for C-g, the problem is only a hardcoded
default in Emacs, and I deem it's not fully fixable without a
patchset that lets you decide via \(say) a command-line flag
which key should act as keyboard-quitter instead of C-g.

To sum up, we return non-nil if the key sequence starts with
Control and involves any of \[, m, i, or g anywhere in the
sequence.  So we even count the sequence C-h g as a nightmare
key: homogenizing it means binding C-h C-g to the same, creating
a situation when C-g is not available for the usual
`abort-recursive-edit'."
  (declare (pure t) (side-effect-free t))
  (or (and (string-prefix-p "C-" keydesc)
           (string-match-p (rx (any "[" "m" "i" "g")) keydesc))
      (string-match-p (eval-when-compile (regexp-opt '("ESC" "TAB" "RET")))
                      keydesc)))

(defcustom massmapper-all-upcase-letters
  (-map #'char-to-string
        (string-to-list "AÄÂÀÁBCÇDEËÊÈÉFGHIÏÎÌÍJKLMNOÖÔÒÓPQRSTUÜÛÙÚVWXYZØÆÅÞẞ"))
  "List of capital letters, non-exhaustive.
  Keys involving one of these will be ignored by
  `massmapper--how-homogenize-key-in-keymap' because Emacs treats
  Control-chords as case-insensitive.

  In principle, we could permit capitals for other chords than
  Control -- email or file a GitHub issue if this interests you."
  :group 'massmapper
  :type '(repeat string))

(defconst massmapper--homogenize-ignore-regexp
  "Regexp matching keys to skip homogenizing.
  These include keys such as <help> which simply clutter up the
  output of \\[massmapper-list-remaps]."
  (regexp-opt massmapper--ignore-keys-irrelevant))

;; NOTE: must return either nil or a list
(defun massmapper--how-homogenize-key-in-keymap (this-key this-cmd keymap)
  "Return the action to homogenize THIS-KEY in KEYMAP.
THIS-CMD is expected to be the command to which THIS-KEY is
mapped, so the function need not look it up for itself.

See `massmapper-homogenizing-winners' for explanation."
  (unless (stringp this-key)
    (error "Expected `kbd'-compatible string: %s" this-key))
  ;; let ((case-fold-search nil)) ;; hopefully boosts performance
  (when (>= massmapper-debug-level 2)
    ;; NOTE: these debug messages don't mean anything is wrong
    (when (and this-cmd (not (functionp this-cmd)) (symbolp this-cmd))
      (message "Massmapper found symbol binding not (yet) a function: %s" this-cmd))
    (when (keymapp this-cmd)
      (message "Massmapper found keymap at: %s in %S" this-key keymap)))
  (and
   this-cmd
   ;; Use symbolp, not functionp, b/c of not-yet-defined commands, e.g. when
   ;; initfiles bind an eshell command globally but eshell hasn't loaded.
   (symbolp this-cmd)
   (not (massmapper--key-seq-steps=1 this-key)) ; nothing to homogenize
   ;; There's no sense to homogenizing e.g. <f1> C-f because you'd never do
   ;; C-<f1> to start the sequence.  As a bonus, this assumption simplifies
   ;; functions like `massmapper--ensure-permachord'.  Although you could make
   ;; the case that we'd want to homogenize all subsequences after <f1>,
   ;; i.e. ensure <f1> C-f is duplicated to <f1> f, but that's so rare we can
   ;; just offload the work on to the user if it's important to them.
   (massmapper--key-starts-with-modifier this-key)
   (not (massmapper--nightmare-p this-key))
   ;; HACK: ignore capital letters because binding C-x C-K also binds C-x C-k
   ;; (think there's a setting to control case sensitivity since Emacs 27 or
   ;; so -- we need to check its value), messing up the expected binding
   ;; there.  The specific example of C-x C-K comes from a Doom Emacs binding
   ;; of C-x K, and the error is caused by the fact that unlike other
   ;; modifiers, control-bindings are always case insensitive! See (info
   ;; "(emacs)Modifier Keys") TL;DR: fix pending: If Control, ignore
   ;; shiftsyms.  For other modifiers, check case sensitivity first.
   (not (massmapper--key-contains massmapper-all-upcase-letters this-key))
   ;; Drop bastard sequences
   (not (and (massmapper--key-has-more-than-one-chord this-key)
             (not (massmapper--permachord-p this-key))))
   (let* (;; NOTE: we filtered out "bastard sequences",
          ;; so we don't bother to ensure the alternative is a chordonce.
          (this-is-permachord (massmapper--permachord-p this-key))
          (permachord-key
           (if this-is-permachord
               this-key
             (massmapper--ensure-permachord this-key)))
          (permachord-cmd
           (if this-is-permachord
               this-cmd
             ;; it's expected and ok that lookup-key definitely can return
             ;; numeric values here (on account of nested keymaps that don't
             ;; yet exist)
             (massmapper--lookup keymap permachord-key t)))
          (chordonce-key
           (if this-is-permachord
               (massmapper--ensure-chordonce this-key)
             this-key))
          (chordonce-cmd
           (if this-is-permachord
               (massmapper--lookup keymap chordonce-key t)
             this-cmd))
          (sibling-keydesc (if this-is-permachord
                               chordonce-key
                             permachord-key))
          (sibling-cmd (if this-is-permachord
                           chordonce-cmd
                         permachord-cmd))
          (winners (->> massmapper-homogenizing-winners
                        (-remove #'cdr) ;; drop items with a keymap
                        (-map #'car)))
          ;; Here we'd have run into issue fixed at
          ;; `massmapper-record-keymap-maybe'.
          (winners-for-this-keymap (->> massmapper-homogenizing-winners
                                        (-select #'cdr)
                                        (--select (equal keymap (cdr it)))
                                        (-map #'car))))

     (cond
      ;; Simple case #1: This key or the sibling key has already been dealt
      ;; with.  Then we just no-op.
      ;; TODO: May need fixing, I think the clause always fails now, but it
      ;; hasn't affected function.  Maybe just remove the clause?
      ;; ((equal keymap (nth 2 (or
      ;;                        (assoc sibling-keydesc massmapper--remap-record)
      ;;                        (assoc this-key massmapper--remap-record))))
      ;;  nil)

      ;; Simple case #2: only one of the two is bound, so just duplicate to
      ;; the other.  We don't need to do it both directions, b/c this
      ;; invocation is operating on this-key, which must be the one known to
      ;; have a command.
      ((null sibling-cmd)
       (list :keydesc sibling-keydesc
             :cmd this-cmd
             :map keymap
             :reason "Homogenize: clone to unbound sibling"
             :olddef sibling-cmd))

      ;; Complex case: both keys have a command, which do we choose?
      ;; Eeny meny miny moe...?  No.  Let's start by checking if one of
      ;; them is a specified winner, then fall back on a rule.
      ((functionp sibling-cmd)
       (cond ((< 1 (length
                    (-non-nil
                     (list (member sibling-keydesc winners-for-this-keymap)
                           (member sibling-cmd winners-for-this-keymap)
                           (member this-key winners-for-this-keymap)
                           (member this-cmd winners-for-this-keymap)))))
              (warn "Found a contradiction in massmapper-homogenizing-winners.")
              nil)

             ((or (member sibling-keydesc winners-for-this-keymap)
                  (member sibling-cmd winners-for-this-keymap))
              (list :keydesc this-key
                    :cmd sibling-cmd
                    :map keymap
                    :reason "Homogenize: winning sibling overwrites this"
                    :olddef this-cmd))

             ((or (member this-key winners-for-this-keymap)
                  (member this-cmd winners-for-this-keymap))
              (list :keydesc sibling-keydesc
                    :cmd this-cmd
                    :map keymap
                    :reason "Homogenize: preset winner overwrites sibling"
                    :olddef sibling-cmd))

             ((< 1 (length
                    (-non-nil
                     (list (member sibling-keydesc winners)
                           (member sibling-cmd winners)
                           (member this-key winners)
                           (member this-cmd winners)))))
              ;; Leave it on the user to fix this mess.
              (warn "Found a contradiction in massmapper-homogenizing-winners.")
              nil)

             ((or (member sibling-keydesc winners)
                  (member sibling-cmd winners))
              (list :keydesc this-key
                    :cmd sibling-cmd
                    :map keymap
                    :reason "Homogenize: winning sibling overwrites this"
                    :olddef this-cmd))

             ((or (member this-key winners)
                  (member this-cmd winners))
              (list :keydesc sibling-keydesc
                    :cmd this-cmd
                    :map keymap
                    :reason "Homogenize: preset winner overwrites sibling"
                    :olddef sibling-cmd))

             ;; Neither key and neither command is rigged to win, so take
             ;; the default action.  (Back when we had the boolean
             ;; massmapper-permachord-wins-homogenizing, this was the only place
             ;; it'd apply)
             (t
              (list :keydesc permachord-key
                    :cmd chordonce-cmd
                    :map keymap
                    :reason "Homogenize: chord-once overwrites perma-chord"
                    :olddef permachord-cmd))))

      ;; Default.
      (t
       (list :keydesc permachord-key
             :cmd chordonce-cmd
             :map keymap
             :reason "Homogenize: chord-once overwrites perma-chord"
             :olddef permachord-cmd))))))

(defvar massmapper--homogenized-keymaps nil
  "Keymaps that have been homogenized.")

(defun massmapper--how-homogenize-keymap (map)
  "Actions for homogenizing most of keymap MAP."
  (cl-loop
   for vec being the key-seqs of (massmapper--raw-keymap map)
   using (key-bindings cmd)
   as key = (key-description vec)
   ;; REVIEW: consider pre-filtering the keymap with nonessential filters
   ;; like these--avoid clobbering things the user could ignore anyway?
   ;; (not (member this-cmd '(self-insert-command
   ;;                         ignore
   ;;                         ignore-event
   ;;                         company-ignore)))
   ;; (not (string-match-p dei--shift-chord-regexp this-key))
   ;; (not (massmapper--key-contains dei--all-shifted-symbols-list this-key))
   ;; (not (massmapper--key-contains-multi-chord this-key))
   ;; (not (massmapper--key-mixes-modifiers this-key))
   as action = (unless (string-match-p massmapper--homogenize-ignore-regexp key)
                 (massmapper--how-homogenize-key-in-keymap (massmapper--normalize key) cmd map))
   when action collect action))

(defun massmapper-homogenize ()
  "Homogenize all keymaps.
  What this means is that we forbid any difference between
  \"similar\" key sequences such as C-x C-e and C-x e; we bind both
  to the same command, so it won’t matter if you keep holding down
  Control or not, after initiating that key sequence.

  The \"master copy\" is the one with fewer chords, i.e. C-x e,
  whose binding overrides the one on C-x C-e.  This can be
  customized on a case-by-case basis in `massmapper-homogenizing-winners'.

  Why we would do something so hare-brained?  It supports
  cross-training with the Deianira input paradigm, which by design
  cannot represent a difference between such key sequences.  Learn
  more at:  https://github.com/meedstrom/deianira"
  (cl-loop
   for map in (-difference massmapper--known-keymaps
                           massmapper--homogenized-keymaps)
   as start = (current-time)
   as actions = (massmapper--how-homogenize-keymap map)
   as overwritten = (cl-loop
                     for action in actions
                     when (plist-get action :olddef)
                     count action)
   when actions do
   (massmapper-execute actions)
   (when (> massmapper-debug-level 0)
     (message "(In %.3fs) Homogenized %S: %d new bindings and %d overwrites"
              (float-time (time-since start))
              map
              (- (length actions) overwritten)
              overwritten))
   do (push map massmapper--homogenized-keymaps)))

(define-obsolete-function-alias 'dei-homogenize-all-keymaps
  'massmapper-homogenize "2023-10-30")

(define-obsolete-function-alias 'massmapper-homogenize-all-keymaps
  'massmapper-homogenize "2023-10-30")

(define-obsolete-function-alias 'massmapper-homogenize-keymaps
  'massmapper-homogenize "2023-10-31")


;;; Mode

;;;###autoload
(define-minor-mode massmapper-mode
  "Actively rebind keys as Emacs loads new keymaps."
  :global t
  :group 'massmapper
  (if massmapper-mode
      (add-hook 'window-buffer-change-functions
                #'massmapper-record-keymap-maybe -70)
    (remove-hook 'window-buffer-change-functions
                 #'massmapper-record-keymap-maybe)))

(provide 'massmapper)

;;; massmapper.el ends here

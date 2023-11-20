;;; massmapper-tests.el --- tests -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'massmapper)
(require 'massmapper-lib)
(require 'ert)

(ert-deftest keydesc-handling-1 ()

  ;; (massmapper--contains-upcase "C-x d")
  ;; (massmapper--key-starts-with-modifier "C-x d")
  ;; (massmapper--key-seq-is-allchord "C-x d")
  ;; (massmapper--key-has-more-than-one-modifier "C-x d")

  ;; (massmapper--key-has-more-than-one-modifier "s-i")
  ;; (massmapper--contains-upcase keydesc)
  ;; (massmapper--key-contains-multi-chord "s-i")
  ;; (massmapper--key-mixes-modifiers "s-i")
  ;; (massmapper--contains-upcase "s-i")
  ;; (massmapper--contains-upcase "s-i")


  ;; (functionp (massmapper--lookup global-map "C-x v" t))

  (should (massmapper--key-seq-has-non-prefix-in-prefix global-map "C-f C-x"))
  (should-not (massmapper--key-seq-has-non-prefix-in-prefix global-map "C-f"))
  (should-not (massmapper--key-seq-has-non-prefix-in-prefix global-map "C-x C-v"))
  (should-not (massmapper--key-seq-has-non-prefix-in-prefix global-map "C-x C-v C-x"))

  (should (massmapper--key-mixes-modifiers "C-s M-s"))
  (should (massmapper--key-mixes-modifiers "C-M-s"))
  (should-not (massmapper--key-mixes-modifiers "s-s"))
  (should-not (massmapper--key-mixes-modifiers "s M-s"))
  (should-not (massmapper--key-mixes-modifiers "C-s C-s"))
  (should-not (massmapper--key-mixes-modifiers "C-s s"))

  (should (massmapper--key-contains-multi-chord "C-M-s"))
  (should-not (massmapper--key-contains-multi-chord "C-s M-s"))
  ;; (should (massmapper--key-contains-multi-chord "C-<M-return>")) ;; fail ok b/c we assume normalized input

  (should (massmapper--key-starts-with-modifier "C-x"))
  (should (massmapper--key-starts-with-modifier "C-x c f b"))
  (should (massmapper--key-starts-with-modifier "C-<f1> f"))
  (should-not (massmapper--key-starts-with-modifier "<f1> C-f"))

  (should (equal (massmapper--parent-stem "C-x ") "C-"))
  (should (equal (massmapper--parent-stem "C-x C-") "C-"))
  (should (equal (massmapper--stem-to-parent-keydesc "C-x ") "C-x"))

  ;; debatable if it should have these effects or be named this way
  (should (equal (massmapper--parent-key "C-x ") "C-x"))
  (should (equal (massmapper--parent-key "C-x C-") "C-x"))
  (should (equal (massmapper--parent-key "C-x C-x") "C-x"))

  (should (equal "C-x k e" (massmapper--ensure-chordonce "C-x C-k C-e")))
  (should (equal "x k e" (massmapper--ensure-chordonce "x C-k C-M-e")))

  (should (equal "C-x C-k C-e" (massmapper--ensure-permachord "C-x k C-e")))
  (should (equal "C-x C-k C-e" (massmapper--ensure-permachord "C-x k C-e")))
  (should (equal "C-x C-k C-e" (massmapper--ensure-permachord "C-x k e")))
  (should (equal "C-x C-k C-e" (massmapper--ensure-permachord "C-x C-k C-e")))
  (should (equal "C-x C-k C-e" (massmapper--ensure-permachord "C-x C-k e")))

  ;; invalid inputs just cause a warning and return the input
  (should (equal "x k e") (massmapper--ensure-permachord "x k e"))

  (should (massmapper--immediate-child-p "C-x " "C-x f"))
  (should (massmapper--immediate-child-p "C-x C-" "C-x C-f"))
  ;; (should-not (massmapper--immediate-child-p "C-x C-" "C-x C-M-f"))
  (should-not (massmapper--immediate-child-p "C-x C-" "C-x f"))
  (should-not (massmapper--immediate-child-p "C-x C-" "C-x C-f f"))



  (should (massmapper--permachord-p "C-x C-f C-e"))
  (should (massmapper--permachord-p "M-<f1> M-f M-e"))
  (should (massmapper--permachord-p "C-x"))
  (should-not (massmapper--permachord-p "C-x C-M-f C-e"))
  (should-not (massmapper--permachord-p "C-x C-f e"))
  (should-not (massmapper--permachord-p "<f1> C-f C-e"))
  (should-not (massmapper--permachord-p "C-x M-f C-e"))
  (should-not (massmapper--permachord-p "M-x M-f C-e"))
  (should-not (massmapper--permachord-p "C-x M-f M-e"))



  ;; (massmapper--last-key "C-M-<backspace>")
  ;; (massmapper--last-key "C-M--")
  ;; (massmapper--last-key "-")
  ;; (massmapper--last-key "<")
  ;; (massmapper--last-key "C-<")
  ;; (massmapper--last-key "C-F")

  )

;; TODO: the squash function should insert <> around RET, TAB, ESC etc.
(ert-deftest keydesc-handling-2 ()
  (let ((problematic-key-descriptions
         '(;; raw            normalized        leaf      1step?
           ("C-x 8 RET"      "C-x 8 RET"      "RET"        nil)
           ("<f2> 8 RET"     "<f2> 8 RET"     "RET"        nil)
           ("<f2> f r"       "<f2> f r"       "r"          nil)
           ("<f2> <f2>"      "<f2> <f2>"      "<f2>"       nil)
           ("ESC <C-down>"   "ESC C-<down>"   "<down>"     nil)
           ("C-x RET C-\\"   "C-x RET C-\\"   "\\"         nil)
           ("TAB"            "TAB"            "TAB"          t)
           ("A-T A-B"        "A-T A-B"        "B"          nil)
           ("A-T A B"        "A-T A B"        "B"          nil)
           ("A-TAB"          "A-TAB"          "TAB"          t)
           ("C-<M-return>"   "C-M-<return>"   "<return>"     t)
           ("<C-M-return>"   "C-M-<return>"   "<return>"     t)
           ("<M-wheel-down>" "M-<wheel-down>" "<wheel-down>" t)
           ("C-- - -"        "C-- - -"        "-"          nil)
           ("<tab>"          "<tab>"          "<tab>"        t)
           ;; ("s-S-M-H-C-A-<return>" "A-C-H-M-S-s-<return>" "dei-ACHMSs<return>" "<return>" t)
           )))
    (dolist (case problematic-key-descriptions)
      (seq-let (raw normalized leaf 1step?) case
        (should (string= normalized (key-description (key-parse raw))))
        (should (string= leaf (dei--get-leaf normalized)))
        (should (eq 1step? (dei--key-seq-steps=1 normalized)))))))

(ert-deftest keydesc-handling-limited ()
  "Test the functions that have strong assumptions on input."
  (let ((example-key-descriptions
         '(;; normalized        parent
           ("C-x 8 <RET>"   "C-x 8")
           ("<f2> 8 <RET>" "<f2> 8")
           ("<f2> f r"     "<f2> f")
           ("<f2> <f2>"    "<f2>")
           ("TAB"         nil)
           ("A-TAB"       "A-")
           ;; ("C-- - -"      "C-- - -"         "dei-C---"       "-"        nil  )
           ;; ("<TAB>"        "<TAB>"          "dei-<TAB>"        "<TAB>" t  )
           )))
    (dolist (case example-key-descriptions)
      (seq-let (normalized parent) case
        (should (string= parent (dei--parent-stem normalized)))))))

(ert-deftest homogenizing ()
  (setq dei-homogenizing-winners '(("C-c C-c")
                                   ("C-x a")
                                   ("C-x g")
                                   ("C-x b")
                                   ("C-c C-c" . org-mode-map)))

  ;; (dei--homogenize-binding-in-keymap "C-c C-c" 'org-mode-map)
  ;; (dei--homogenize-binding-in-keymap "C-c c" 'org-mode-map)
  ;; (dei--homogenize-binding "C-x f")
  ;; (dei--homogenize-binding "")
  ;; (dei--homogenize-binding "C-c c")
  ;; (dei--homogenize-binding "")
  )

;; (provide 'massmapper-tests)
;;; massmapper-tests.el ends here

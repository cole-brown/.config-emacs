;;; emacs/imp/require.el -*- lexical-binding: t; -*-


;;                                 ──────────                                 ;;
;; ╔════════════════════════════════════════════════════════════════════════╗ ;;
;; ║                            Require Features                            ║ ;;
;; ╚════════════════════════════════════════════════════════════════════════╝ ;;
;;                                   ──────                                   ;;
;;                     Require Imp feature symbol paths.                      ;;
;;                                 ──────────                                 ;;


;;------------------------------------------------------------------------------
;; Public API: Require
;;------------------------------------------------------------------------------


(defun imp:require (&rest feature)
  "Ensures file(s) for FEATURE:BASE keyword & FEATURE symbols are provided.

Returns non-nil on success."
  (let ((feature:normal (int<imp>:feature:normalize feature)))
    ;; Already provided?
    (cond ((imp:provided? feature:normal)
           t)

          ;; Can we load it?
          ((progn
             (condition-case err
                (int<imp>:load:feature feature:normal)
              ;; If loading by feature failed, then user needs to check their order of loading/requiring things. Let's give them some more info.
              (error
               (int<imp>:error:user "imp:require"
                                    '("Failed to find/load required feature: \n"
                                      "  input feature: %S\n"
                                      "  normalized:    %S\n"
                                 "Check your order of providing/loading, "
                                 "or make sure it initializes its root and "
                                 "features with imp first.\n"
                                 "Error: %S\n"
                                 "  %S")
                                    feature
                               feature:normal
                               (car err)
                               (cdr err))
              )
           ;; Yes; so add to imp's feature tree.
              (int<imp>:feature:add feature:normal)))
           )

          ;; Nope; return nil.
          (t
           nil))))
;; (imp:require 'test 'this)


;; TODO: I want to have a plist version of `imp:require' that works like `imp:load':
;;   (imp:require:foo :feature '(foo bar) :error nil ...)
;; But I don't know what to call it, and I don't think I want to replace
;; `imp:require' since it's what I want in 99% of the cases.
;;
;; So... What is a good name for `imp:require:but-with-a-plist'?
;;
;; "eval.el" uses `ignore-error' currently to ignore `imp:require' user-errors.


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide:with-emacs :imp 'require)

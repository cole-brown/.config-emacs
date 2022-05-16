;;; emacs/imp/eval.el -*- lexical-binding: t; -*-

;;                                 ──────────                                 ;;
;; ╔════════════════════════════════════════════════════════════════════════╗ ;;
;; ║                                Evaluate                                ║ ;;
;; ╚════════════════════════════════════════════════════════════════════════╝ ;;
;;                                   ──────                                   ;;
;;                      Not now though, later... Maybe?                       ;;
;;                                 ──────────                                 ;;

(require 'cl-lib)


;; TODO: unit test


;;------------------------------------------------------------------------------
;; Evaluate After Package(s)/Feature(s) Load
;;------------------------------------------------------------------------------
;; Happily stolen from Doom. This is Doom's `after!' macro (from
;; "core/core-lib.el"), broken up so I could grok it.

;; TODO: Currently only supports stuff provided to Emacs as a feature.
;;   - Is that acceptable or should it work also on imp-only features?
(defmacro imp:eval:after (feature &rest body)
  "Evaluate BODY after FEATURE(s) have loaded.

FEATURE is a symbol or list of them. These are package/feature names, not modes,
functions or variables. It can be:

- An unquoted imp or Emacs feature symbol (the name of a package/feature)
    (imp:eval:after helm BODY...)
    (imp:eval:after :foo BODY...)
- An unquoted imp feature symbol list
    (imp:eval:after (:foo bar) BODY...)
- An unquoted, nested list of compound feature lists, using any combination of
  :or/:any and :and/:all
    (imp:eval:after (:or feature-a feature-b ...)  BODY...)
    (imp:eval:after (:or :jeff (:foo bar) ...)  BODY...)
    (imp:eval:after (:and feature-a feature-b ...) BODY...)
    (imp:eval:after (:and feature-a (:or feature-b feature-c) ...) BODY...)

This is a wrapper around `eval-after-load' that:
1. Suppresses warnings for disabled features at compile-time
2. Supports compound feature statements
3. Prevents eager expansion pulling in autoloaded macros all at once"
  (declare (indent 1) (debug t))
  ;;------------------------------
  ;; Error Cases
  ;;------------------------------
  (cond ((null feature)
         (error "imp:eval:after: FEATURE must not be null! Got: %S" feature))

        ((and (listp feature)
              (eq 'quote (car feature)))
         (error "imp:eval:after: FEATURE should not be quoted! Got: %S" feature))

        ;;------------------------------
        ;; Single Feature
        ;;------------------------------
        ((symbolp feature)
         (list (if (or (not (bound-and-true-p byte-compile-current-file))
                       ;; `imp:require' will check Emacs and imp for:
                       ;;   1) Is the feature already provided?
                       ;;   2) Can the feature be provided right now?
                       ;; It signals a `user-error' if the answers are no, which
                       ;; we need to prevent; this is just a check for if it's
                       ;; ready right now.
                       (ignore-error '(error user-error)
                         (imp:require feature)))
                   #'progn
                 #'with-no-warnings)
               ;; We intentionally avoid `with-eval-after-load' to prevent eager
               ;; macro expansion from pulling (or failing to pull) in autoloaded
               ;; macros/features.
               `(eval-after-load ',(if (keywordp feature)
                                       (int<imp>:feature:normalize:imp->emacs feature)
                                     feature)
                  ',(macroexp-progn body))))

        ((and (listp feature)
              (not (memq (car feature) '(:and :all :or :any))))
         ;; Convert imp feature list to Emacs feature symbol & recurse to hit the above case.
         `(imp:eval:after ,(apply #'int<imp>:feature:normalize:imp->emacs feature) ,@body))

        ;;------------------------------
        ;; Multiple Features
        ;;------------------------------
        ;; Figure out `:and'/`:or' condition, then recurse.
        (t
         (let ((condition (car feature))
               (rest      (cdr feature)))
           ;;---
           ;; OR
           ;;---
           (cond ((memq condition '(:or :any))
                  ;; Make an `imp:eval:after' for each feature and let them each
                  ;; run after their feature loads, so... can/will eval BODY multiple
                  ;; times.
                  (macroexp-progn
                   (cl-loop for next in rest
                            collect `(imp:eval:after ,next ,@body))))
                 ;;---
                 ;; AND
                 ;;---
                 ((memq condition '(:and :all))
                  ;; Chain `imp:eval:after' for the features in the order they
                  ;; are supplied. So... Waits for the first feature to be loaded
                  ;; before waiting for the second, etc.
                  (dolist (next (reverse rest) (car body))
                    (setq body `((imp:eval:after ,next ,@body)))))

                 ;;---
                 ;; ERROR?!
                 ;;---
                 ;; Should not get here, since anything that is not
                 ;; `:and'/`:all'/`:or'/`:any' should be considered an imp feature
                 ;; keyword, but to be complete: signal an error.
                 (t
                  `(int<imp>:error "imp:eval:after"
                                   "Unhandled condition `%S' for features: %S"
                                   condition
                                   feature)))
           ))))
;; (imp:eval:after nil (message "hi"))
;; (imp:eval:after ':imp (message "hi"))
;; (imp:eval:after :imp (message "hi"))
;; (imp:eval:after imp (message "hi"))
;; (imp:eval:after (:imp eval) (message "hi"))
;; (imp:eval:after (:and :imp (imp eval)) (message "hi"))
;; Incorrect:
;;   (imp:eval:after 'zenburn (message "hi"))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide:with-emacs :imp 'eval)

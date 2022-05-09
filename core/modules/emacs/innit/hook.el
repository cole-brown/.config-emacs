;;; hook.el --- With Blackjack! -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Cole Brown
;;
;; Author: Cole Brown <code@brown.dev>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created: May 09, 2022
;; Modified: May 09, 2022
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/work/hook
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  With Blackjack!
;;
;;; Code:

;;; theme.el --- Theme Helpers -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2022-05-06
;; Modified:   2022-05-06
;; URL:        https://github.com/cole-brown/.config-emacs
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;  Theme Helpers
;;
;;; Code:


;; On loan from Doom:
;;   - "core/autoload/themes.el"
;;   - "core/core-lib.el"


(imp:require :innit 'error)


;;------------------------------------------------------------------------------
;; Running Hooks Helpers
;;------------------------------------------------------------------------------

(defun int<innit>:hook:run (hook)
  "Run HOOK (a hook function) with better error handling.
Meant to be used with `run-hook-wrapped'."
  (condition-case-unless-debug e
      (funcall hook)
    (error
     (signal 'innit:error:hook (list hook e))))
  ;; return nil so `run-hook-wrapped' won't short circuit
  nil)


(defun innit:hook:run (&rest hooks)
  "Run HOOKS (a list of hook variable symbols) with better error handling.
Is used as advice to replace `run-hooks'."
  (dolist (hook hooks)
    (condition-case-unless-debug e
        (run-hook-wrapped hook #'int<innit>:hook:run)
      ;; Catch our error signal and warn about it before allowing the error to continue on up.
      (innit:error:hook
       (unless debug-on-error
         (lwarn hook :error "Error running hook %S because: %s"
                (if (symbolp (cadr e))
                    (symbol-name (cadr e))
                  (cadr e))
                (caddr e)))
       (signal 'innit:error:hook (cons hook (cdr e))))
      ;; TODO: Catch other errors too?
      )))


;; TODO: Add if needed:
;; (defun doom-run-hook-on (hook-var trigger-hooks)
;;   "Configure HOOK-VAR to be invoked exactly once when any of the TRIGGER-HOOKS
;; are invoked *after* Emacs has initialized (to reduce false positives). Once
;; HOOK-VAR is triggered, it is reset to nil.

;; HOOK-VAR is a quoted hook.
;; TRIGGER-HOOK is a list of quoted hooks and/or sharp-quoted functions."
;;   (dolist (hook trigger-hooks)
;;     (let ((fn (intern (format "%s-init-on-%s-h" hook-var hook))))
;;       (fset
;;        fn (lambda (&rest _)
;;             ;; Only trigger this after Emacs has initialized.
;;             (when (and after-init-time
;;                        (or (daemonp)
;;                            ;; In some cases, hooks may be lexically unset to
;;                            ;; inhibit them during expensive batch operations on
;;                            ;; buffers (such as when processing buffers
;;                            ;; internally). In these cases we should assume this
;;                            ;; hook wasn't invoked interactively.
;;                            (and (boundp hook)
;;                                 (symbol-value hook))))
;;               (innit:hook:run hook-var)
;;               (set hook-var nil))))
;;       (cond ((daemonp)
;;              ;; In a daemon session we don't need all these lazy loading
;;              ;; shenanigans. Just load everything immediately.
;;              (add-hook 'after-init-hook fn 'append))
;;             ((eq hook 'find-file-hook)
;;              ;; Advise `after-find-file' instead of using `find-file-hook'
;;              ;; because the latter is triggered too late (after the file has
;;              ;; opened and modes are all set up).
;;              (advice-add 'after-find-file :before fn '((depth . -101))))
;;             ((add-hook hook fn -101)))
;;       fn)))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :innit 'hook)

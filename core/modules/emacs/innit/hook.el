;;; hook.el --- With Blackjack! -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2022-05-09
;; Modified:   2022-06-01
;; URL:        https://github.com/cole-brown/.config-emacs
;; Copyright (C) 2022 Cole Brown
;;
;;; Commentary:
;;
;;  With Blackjack!
;;
;;; Code:


;; Some code on loan from Doom:
;;   - "core/autoload/themes.el"
;;   - "core/core-lib.el"


(require 'dash)
(imp:require :path)

(imp:require :innit 'error)


;;------------------------------------------------------------------------------
;; Helpers for Hooks...
;;------------------------------------------------------------------------------

;; Originally from here:
;; https://www.reddit.com/r/emacs/comments/1m7fqv/avoid_lambda_in_hooks_use_defun_instead/cc83axz/
(defmacro spy:hook/defun-and-hooker (hook-var options &rest body)
  "Macro that `defun's a function called 'sss:hook/<hook-name>'
and adds the hook function to HOOK-VAR.

OPTIONS is a plist of optional vars:
  :name     - Hook function will be named 'sss:hook/<name>'.
              If no `:name', name will be set to `(symbol-name HOOK-VAR)'.

  :quiet    - Do not output 'Running hook' message.

  :postpend - Passed on to `add-hook'. If non-nil, hook will be postpended to
              the list of hooks. If nil, hook will be prepended.

  :file     - Filename where your macro is called from... in case you happen
              to lose your hook.

  :docstr   - A string to use as the defined function's docstring.

BODY is the code to run in the hook.
"
  (declare (indent 1))
  ;; Eval inputs once.
  (-let* ((_m//hook hook-var)
          ;; dash-let's plist match pattern to non-keys in ARGS.
          ((&plist :name _m//name
                   :quiet _m//quiet
                   :postpend _m//postpend
                   :file _m//file
                   :docstr _m//docstr)
           (eval options))
          (_m//hook-name (concat "sss:hook/"
                                  (if _m//name
                                      _m//name
                                    ;; Remove "-hook"?
                                    (s-chop-suffix "-hook" (symbol-name _m//hook)))))
          (_m//hook-fn (intern _m//hook-name)))

    `(progn
       (defun ,_m//hook-fn () ,_m//docstr
              (unless ,_m//quiet
                ;; Nice info message maybe?
                (message
                 "Running hook `%s'%s..."
                 ,_m//hook-name
                 (if (not (stringp ,_m//file))
                     ""
                   (concat " from "
                           (path:relative ,_m//file)))))

              ,@body)
       (add-hook ',_m//hook #',_m//hook-fn ',_m//postpend))))
;; (setq test-hook nil)
;; (makunbound sss:hook/test)
;; (spy:hook/defun-and-hooker test-hook nil (message "Hello there."))
;; (spy:hook/defun-and-hooker test-hook nil (message "Hello there."))
;; test-hook
;; (run-hooks 'test-hook)
;; (setq debug-on-error t)
;; (setq test-hook nil)
;; (spy:hook/defun-and-hooker test-hook '(:name "jeff/mcjefferson" :file "here") (message "Hello there."))
;; test-hook
;; (run-hooks 'test-hook)


(defmacro spy:hook/defun (hook-var options &rest body)
  "Macro that `defun's a function called 'sss:hook/<hook-name>'.

HOOK-VAR isn't use; currently here for consistency with `spy:hook/defun-and-hooker'.
TODO: Remove HOOK-VAR.

OPTIONS is a plist of optional vars:
  :name     - Hook function should be named 'sss:hook/<name>'.
              If no `:name', name will be set to `(symbol-name HOOK-VAR)'
              minus any '-hook' suffix.

  :quiet    - Do not output 'Running hook' message.

  :file     - Filename where your macro is called from... in case you happen
              to lose your hook.

  :docstr   - A string to use as the defined function's docstring.

BODY is the code to run in the hook.

Use this over `spy:hook/defun-and-hooker' only in cases where you aren't
`add-hook'ing directly (e.g. for use-package's ':hook').
"
  (declare (indent 1))
  ;; Eval inputs once.
  (-let* ((_m//hook hook-var)
          ;; dash-let's plist match pattern to non-keys in ARGS.
          ((&plist :name _m//name
                   :quiet _m//quiet
                   :postpend _m//postpend
                   :file _m//file
                   :docstr _m//docstr)
           (eval options))
          (_m//hook-name (concat "sss:hook/"
                                 (if _m//name
                                     _m//name
                                   ;; Remove "-hook"?
                                   (s-chop-suffix "-hook" (symbol-name _m//hook)))))
          (_m//hook-fn (intern _m//hook-name)))

    `(defun ,_m//hook-fn () ,_m//docstr
            (unless ,_m//quiet
              ;; Nice info message maybe?
              (message
               "Running hook `%s'%s..."
               ,_m//hook-name
               (if (not (stringp ,_m//file))
                   ""
                 (concat " from "
                         (path:relative ,_m//file)))))

            ,@body)
    ))
;; (setq test-hook nil)
;; (spy:hook/defun test-hook nil (message "Hello there."))
;; (add-hook 'test-hook 'sss:hook/test)
;; test-hook
;; (run-hooks 'test-hook)
;; (setq test-hook nil)
;; (spy:hook/defun test-hook '(:name "captain-hook" :file "here") (message "hi."))
;; (add-hook 'test-hook 'sss:hook/captain)
;; test-hook
;; (run-hooks 'test-hook)


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

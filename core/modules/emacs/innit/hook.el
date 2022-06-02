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


(imp:require :path)
(imp:require :nub)
(imp:require :innit 'error)


;;------------------------------------------------------------------------------
;; Customs
;;------------------------------------------------------------------------------

(defcustom innit:hook:func-name/prefix "mantle:hook:"
  "Prefix to use for hook function names created by `innit:hook/defun-and-hooker'."
  :group 'innit:group
  :type  '(string))


;;------------------------------------------------------------------------------
;; Helpers for Hooks...
;;------------------------------------------------------------------------------

;; Originally from here:
;; https://www.reddit.com/r/emacs/comments/1m7fqv/avoid_lambda_in_hooks_use_defun_instead/cc83axz/
(defmacro innit:hook:defun-and-add (hook-var options &rest body)
  "`defun' a hook function (which will run BODY) and add it to HOOK-VAR.

OPTIONS is a plist of optional vars:
  :name     - Hook function will be named:
                (concat innit:hook:func-name/prefix '<hook-name>')
              If no `:name', '<hook-name>' will be `(symbol-name HOOK-VAR)' sans
              \"-hook\" suffix.

  :quiet    - Do not output the 'Running hook [...]' message.

  :postpend - Passed on to `add-hook'. If non-nil, hook will be postpended to
              the list of hooks. If nil, hook will be prepended.

  :file     - Filename where your macro is called from... in case you happen
              to lose your hook and need to find it.

  :docstr   - A string to use as the defined function's docstring."
  (declare (indent 2))
  ;; Eval inputs once.
  (let* ((macro<innit>:hook      hook-var)
         (macro<innit>:options   (eval options))
         (macro<innit>:name      (plist-get macro<innit>:options :name))
         (macro<innit>:quiet     (plist-get macro<innit>:options :quiet))
         (macro<innit>:postpend  (plist-get macro<innit>:options :postpend))
         (macro<innit>:file      (plist-get macro<innit>:options :file))
         (macro<innit>:docstr    (plist-get macro<innit>:options :docstr))
         (macro<innit>:hook-name (concat innit:hook:func-name/prefix
                                         (if macro<innit>:name
                                             macro<innit>:name
                                           ;; Remove "-hook"?
                                           (string-remove-suffix "-hook" (symbol-name macro<innit>:hook)))))
         (macro<innit>:hook-fn (intern macro<innit>:hook-name)))

    `(progn
       ;; Create function...
       (defun ,macro<innit>:hook-fn ()
         ,macro<innit>:docstr
         (unless ,macro<innit>:quiet
           ;; Nice info message maybe?
           (nub:out
            :innit
            :info
            ,macro<innit>:hook-name
            "Running hook `%s'%s..."
            ,macro<innit>:hook-name
            (if (not (stringp ,macro<innit>:file))
                ""
              (concat " from "
                      (path:relative ,macro<innit>:file)))))
         ;; And run the actual hook.
         ,@body)
       ;; ...add the new hook  function to the hook variable.
       (add-hook ',macro<innit>:hook #',macro<innit>:hook-fn ',macro<innit>:postpend))))
;; (setq test-hook nil)
;; (makunbound mantle:hook:test)
;; (innit:hook:defun-and-add test-hook nil (message "Hello there."))
;; (innit:hook:defun-and-add test-hook nil (message "Hello there."))
;; test-hook
;; (run-hooks 'test-hook)
;; (setq debug-on-error t)
;; (setq test-hook nil)
;; (innit:hook:defun-and-add test-hook '(:name "jeff/mcjefferson" :file (path:current:file) (message "Hello there."))
;; test-hook
;; (run-hooks 'test-hook)


(defmacro innit:hook:defun (hook-var options &rest body)
  "`defun' a hook function (which will run BODY).

OPTIONS is a plist of optional vars:
  :name     - Hook function will be named:
                (concat innit:hook:func-name/prefix '<hook-name>')
              If no `:name', '<hook-name>' will be `(symbol-name HOOK-VAR)' sans
              \"-hook\" suffix.

  :quiet    - Do not output the 'Running hook [...]' message.

  :file     - Filename where your macro is called from... in case you happen
              to lose your hook and need to find it.

  :docstr   - A string to use as the defined function's docstring.

Use this over `innit:hook:defun-and-add' only in cases where you aren't
`add-hook'ing directly (e.g. for use-package's ':hook')."
  (declare (indent 2))
  ;; Eval inputs once.
  (let* ((macro<innit>:hook      hook-var)
         (macro<innit>:options   (eval options))
         (macro<innit>:name      (plist-get macro<innit>:options :name))
         (macro<innit>:quiet     (plist-get macro<innit>:options :quiet))
         (macro<innit>:file      (plist-get macro<innit>:options :file))
         (macro<innit>:docstr    (plist-get macro<innit>:options :docstr))
         (macro<innit>:hook-name (concat innit:hook:func-name/prefix
                                         (if macro<innit>:name
                                             macro<innit>:name
                                           ;; Remove "-hook"?
                                           (string-remove-suffix "-hook" (symbol-name macro<innit>:hook)))))
         (macro<innit>:hook-fn (intern macro<innit>:hook-name)))

    `(defun ,macro<innit>:hook-fn ()
       ,macro<innit>:docstr
       (unless ,macro<innit>:quiet
         ;; Nice info message maybe?
         (nub:out
          :innit
          :info
          ,macro<innit>:hook-name
          "Running hook `%s'%s..."
          ,macro<innit>:hook-name
          (if (not (stringp ,macro<innit>:file))
              ""
            (concat " from "
                    (path:relative ,macro<innit>:file)))))
       ;; And run the actual hook.
       ,@body)))
;; (setq test-hook nil)
;; (makunbound mantle:hook:test)
;; (innit:hook:defun test-hook nil (message "Hello there."))
;; (add-hook 'test-hook 'mantle:hook:test)
;; test-hook
;; (run-hooks 'test-hook)
;; (setq test-hook nil)
;; (makunbound mantle:hook:captain)
;; (innit:hook:defun test-hook '(:name "captain-hook" :file "here") (message "hi."))
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

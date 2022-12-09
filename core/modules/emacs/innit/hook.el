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
(imp:require :innit 'squelch)
(imp:require :elisp 'utils 'functions)


;;------------------------------------------------------------------------------
;; Customs
;;------------------------------------------------------------------------------

(defcustom innit:hook:func/name:prefix "mantle:hook:"
  "Prefix to use for hook function names created by `innit:hook:defun-and-add'."
  :group 'innit:group
  :type  '(string))


;;------------------------------------------------------------------------------
;; Helpers for Hooks...
;;------------------------------------------------------------------------------

(defun innit:hook:func/name:string (name)
  "Return a hook function name string for NAME.

NAME must be a string or a symbol; any '-hook' suffix will be removed.

Hook function name returned will be a string:
  (concat innit:hook:func/name:prefix '<hook-name>')"
  ;;------------------------------
  ;; Check for Errors.
  ;;------------------------------
  (when (and (not (stringp name))
              (not (symbolp name)))
    (nub:error
       :innit
       "innit:hook:func/name:string"
     "NAME must be a string or (quote) symbol. Got: %S"
     name))

  ;;------------------------------
  ;; Build Hook Function's Name.
  ;;------------------------------
  (concat innit:hook:func/name:prefix
          (string-remove-suffix "-hook"
                                (if (stringp name)
                                    name
                                  (symbol-name name)))))
;; (innit:hook:func/name:string 'jeff)
;; (innit:hook:func/name:string 'jeff-hook)
;; (innit:hook:func/name:string "jeff")
;; (innit:hook:func/name:string "jeff-hook")


(defun innit:hook:func/name:symbol (name hook-var)
  "Return a hook function name symbol for NAME & HOOK-VAR.

NAME must be nil or a string.

HOOK-VAR must be nil or a quote hook variable.

NAME and HOOK-VAR cannot both be nil - an error will be signaled. NAME is
preferred over HOOK-VAR.

Hook function name returned will be an interned symbol from
`innit:hook:func/name:string' string return."
  (intern (innit:hook:func/name:string (or name
                                           hook-var))))
;; (innit:hook:func/name:symbol nil 'test:jeff)
;; (innit:hook:func/name:symbol "test:jill" 'test:jeff)


(defun int<innit>:hook:option (keyword options)
  "Get KEYWORD from OPTIONS and evaluate if necessary?"
  (let ((option (plist-get options keyword)))
    (if (and (listp option)
             (functionp (car option)))
        (eval option)
      option)))
;; (int<innit>:hook:option :name '(:name nil))
;; (int<innit>:hook:option :name '(:name "jeff"))
;; (int<innit>:hook:option :name '(:name jeff))
;; (int<innit>:hook:option :name '(:name (concat "j" "eff")))
;; (int<innit>:hook:option :dne '(:name (concat "j" "eff")))


(defun innit:cmd:hook:func/name (name)
  "Create hook function name symbol for NAME and insert at point.

NAME must be nil or a string."
  (interactive "sSuffix or Hook Var Name: ")
  (insert (innit:hook:func/name:string name)))


;; Originally from here:
;; https://www.reddit.com/r/emacs/comments/1m7fqv/avoid_lambda_in_hooks_use_defun_instead/cc83axz/
(defmacro innit:hook:defun-and-add (hook-vars options &rest body)
  "`defun' a hook function (which will run BODY) and add it to HOOK-VARS.

HOOK-VARS should be an unquoted hook symbol or a list of such. The hook function
created will be added to all of these hook variables.

OPTIONS is a plist of optional vars:
  :name      - Hook function will be named:
                 (concat innit:hook:func/name:prefix '<hook-name>')
               - '<hook-name>':
                 - If `:name' is a string, that string.
                 - If `:name' is a symbol (e.g. the hook variable), convert to a
                   string via `symbol-name'.
                 - Remove \"-hook\" suffix if present.
                 - See: `innit:hook:func/name:string'

  :argslist  - If a list, create the hook's `defun' with this list of arguments.

  :quiet     - If non-nil, do not output the 'Running hook [...]' message.

  :squelch   - If non-nil, wrap BODY in `innit:squelch'.

  :depth     - Passed on to `add-hook' as its `depth' argument.
             - If nil, hook will be prepended.
             - If non-nil, hook will be added at the depth indicated.
             - Should be an integer in the range of [-100, 100].
               - Any non-nil symbol means 90 - see function `add-hook'.

  :transient - If non-nil, hook will delete itself after it has run once.

  :file      - Filename where your macro is called from... in case you happen
               to lose your hook and need to find it.

  :docstr    - A string to use as the defined function's docstring."
  (declare (indent 2))
  ;; Try to eval inputs (at most) once.
  (let* ((macro<innit>:hooks     (elisp:list:flatten hook-vars)) ;; Normalize into a list.
         (macro<innit>:name      (int<innit>:hook:option :name options))

         (macro<innit>:quiet     (int<innit>:hook:option :quiet options))
         (macro<innit>:squelch   (int<innit>:hook:option :squelch options))
         (macro<innit>:depth     (int<innit>:hook:option :depth options))
         (macro<innit>:transient (int<innit>:hook:option :transient options))
         (macro<innit>:arg/file  (int<innit>:hook:option :file options)) ;; Not complete yet...
         (macro<innit>:docstr    (int<innit>:hook:option :docstr options))
         ;; Do not eval:
         ;;   - input
         (macro<innit>:argslist  (or (int<innit>:hook:option :argslist options)
                                     '(&rest _)))
         ;;   - derived
         (macro<innit>:hook-name (innit:hook:func/name:string (or macro<innit>:name
                                                                  (nth 0 macro<innit>:hooks))))
         (macro<innit>:hook-fn   (intern macro<innit>:hook-name)))

    ;; Currently incompatible: `:transient' and multiple hook vars.
    ;; TODO: Do we want the function to delete itself from all hooks after it's
    ;; run from any of them for the first time? Or should it run once from each
    ;; hook?
    (when (and macro<innit>:transient
               (> (length macro<innit>:hooks) 1))
      (nub:error
         :innit
         "innit:hook:func/name:string"
       '("Couldn't be bothered figuring out how to do `:transient' /and/ "
         "multiple hooks. So now you have to...")))

    ;; Should the hook's BODY be squelched?
    (setq body
          (if (not macro<innit>:squelch)
              body
            (innit:squelch body)))

    ;; Finish figuring out file arg.
    `(let ((macro<innit>:file (when (stringp ,macro<innit>:arg/file)
                                (path:relative ,macro<innit>:arg/file
                                               user-emacs-directory))))

       ;; Create function...
       (defun ,macro<innit>:hook-fn
           ;; ...with provided args list, or default of "who cares?" args list.
           ,macro<innit>:argslist
         ,macro<innit>:docstr
         (unless ,macro<innit>:quiet
           ;; Nice info message maybe?
           (nub:out
            :innit
            :info
            ,macro<innit>:hook-name
            "Running hook `%s'%s..."
            ,macro<innit>:hook-name
            (if (not (stringp macro<innit>:file))
                ""
              (concat " from '"
                      macro<innit>:file
                      "'"))))
         ;; And run the actual hook.
         ,@body
         ;; If a transient, remove hook now that it's run once.
         (when ,macro<innit>:transient
           ;; Currently should only get here if only 1 hook var, but...
           ;; ...remove from "all" hooks, to be safe?
           (dolist (macro<innit>:hook macro<innit>:hooks)
             (remove-hook ',macro<innit>:hook #',macro<innit>:hook-fn))))

       ;; ...add the new hook function to the hook variable(s).
       (dolist (macro<innit>:hook macro<innit>:hooks)
         (add-hook ',macro<innit>:hook #',macro<innit>:hook-fn ',macro<innit>:depth)))))
;; (setq test-hook nil)
;; (makunbound 'mantle:hook:test)
;; (fmakunbound 'mantle:hook:test)
;; (innit:hook:defun-and-add test-hook nil (message "Hello there."))
;; (innit:hook:defun-and-add test-hook (:quiet t) (message "Hello there."))
;; (innit:hook:defun-and-add test-hook (:quiet t :argslist (&rest ignore)) (message "Hello there."))
;; (innit:hook:defun-and-add test-hook (:file (path:current:file) :docstr "this is a test hook") (message "Hello from a file?"))
;; test-hook
;; (run-hooks 'test-hook)
;; (setq debug-on-error t)
;; (setq test-hook nil)
;; (innit:hook:defun-and-add test-hook '(:name "jeff/mcjefferson" :file (path:current:file) (message "Hello there."))
;; test-hook
;; (run-hooks 'test-hook)


(defmacro innit:hook:defun (options &rest body)
  "`defun' a hook function (which will run BODY).

OPTIONS is a plist of optional vars:
  :name      - Hook function will be named:
                 (concat innit:hook:func/name:prefix '<hook-name>')
               - '<hook-name>':
                 - If `:name' is a string, that string.
                 - If `:name' is a symbol (e.g. the hook variable), convert to a
                   string via `symbol-name'.
                 - Remove \"-hook\" suffix if present.
                 - See: `innit:hook:func/name:string'

  :argslist  - If a list, create the hook's `defun' with this list of arguments.

  :quiet     - If non-nil, do not output the 'Running hook [...]' message.

  :squelch   - If non-nil, wrap BODY in `innit:squelch'.

  :transient - Set to the hook variable and hook will delete itself after it has
               run once.

  :file      - File path string for where your macro is called from... in case
               you happen to lose your hook and need to find it.

  :docstr    - A string to use as the defined function's docstring.

Use this over `innit:hook:defun-and-add' only in cases where you aren't
`add-hook'ing directly (e.g. for use-package's ':hook')."
  (declare (indent 1))
  ;; Try to eval inputs (at most) once.
  (let* ((macro<innit>:name      (int<innit>:hook:option :name options))
         (macro<innit>:quiet     (int<innit>:hook:option :quiet options))
         (macro<innit>:squelch   (int<innit>:hook:option :squelch options))
         (macro<innit>:transient (int<innit>:hook:option :transient options))
         (macro<innit>:arg/file  (int<innit>:hook:option :file options)) ;; Not complete yet...
         (macro<innit>:docstr    (int<innit>:hook:option :docstr options))
         ;; Do not eval:
         ;;   - input
         (macro<innit>:argslist  (or (int<innit>:hook:option :argslist options)
                                     '(&rest _)))
         ;;   - derived
         (macro<innit>:hook-name (innit:hook:func/name:string macro<innit>:name))
         (macro<innit>:hook-fn   (intern macro<innit>:hook-name)))

    ;; Should the hook's BODY be squelched?
    (setq body
          (if (not macro<innit>:squelch)
              body
            (innit:squelch body)))

    ;; Create function...
    `(let ((macro<innit>:file (when (stringp ,macro<innit>:arg/file)
                                (path:relative ,macro<innit>:arg/file
                                               user-emacs-directory))))
       (defun ,macro<innit>:hook-fn
           ;; ...with provided args list, or default of "who cares?" args list.
           ,macro<innit>:argslist
         ,macro<innit>:docstr
         (unless ,macro<innit>:quiet
           ;; Nice info message maybe?
           (nub:out
            :innit
            :info
            ,macro<innit>:hook-name
            "Running hook `%s'%s..."
            ,macro<innit>:hook-name
            (if (not (stringp macro<innit>:file))
                ""
              (concat " from '"
                      macro<innit>:file
                      "'"))))
         ;; And run the actual hook.
         ,@body
         ;; If a transient, remove hook now that it's run once.
         (when (bound-and-true-p ,macro<innit>:transient)
           (remove-hook ',macro<innit>:transient #',macro<innit>:hook-fn))))))
;; (setq test-hook nil)
;; (makunbound 'mantle:hook:test)
;; (fmakunbound 'mantle:hook:test)
;; (innit:hook:defun (:name test-hook :quiet t) (message "Hello there."))
;; (innit:hook:defun (:name "test-hook" :quiet t) (message "Hello there."))
;; (innit:hook:defun (:name (concat "test" "-" "hook") :quiet t) (message "Hello there."))
;; (innit:hook:defun (:name test-hook :quiet t :argslist (&rest ignore)) (message "Hello there."))
;; (innit:hook:defun (:name test-hook :quiet t :argslist (&rest ignore)) (message "Hello there."))
;; (innit:hook:defun (:name test-hook :file (path:current:file) :docstr "this is a test hook") (message "Hello from a file?"))
;; (add-hook 'test-hook 'mantle:hook:test)
;; test-hook
;; (run-hooks 'test-hook)
;; (setq test-hook nil)
;; (makunbound 'mantle:hook:captain)
;; (fmakunbound 'mantle:hook:captain)
;; (innit:hook:defun test-hook (:name "captain-hook" :file "here") (message "hi."))
;; (add-hook 'test-hook 'sss:hook/captain)
;; test-hook
;; (run-hooks 'test-hook)
;;
;; `macro<imp>:path/file' from `imp:use-package' is a lexical var and must work as `:file'.
;; (imp:use-package test-foo
;;   :init
;;   (innit:hook:defun
;;       (:name    "test:fooo"
;;        :file    macro<imp>:path/file
;;        :quiet   t)
;;     (message "hi: %S" macro<imp>:path/file)
;;     ))


;;------------------------------------------------------------------------------
;; Running Hooks Helpers
;;------------------------------------------------------------------------------

(defun int<innit>:hook:run (hook)
  "Run HOOK (a hook function) with better error handling.
Meant to be used with `innit:hook:run` (or at least `run-hook-wrapped')."
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

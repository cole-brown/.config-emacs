;;; uniquify.el --- Unique Buffer Names -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Cole Brown
;;
;; Author: Cole Brown <code@brown.dev>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:  2022-10-20
;; Modified: 2022-10-20
;;
;;; Commentary:
;;
;; Nice, unique buffer names base off projects.
;;
;;; Code:


(require 'cl-lib)

(imp:require :path 'path)
(imp:require :path 'git)
(imp:require :path 'buffer)


;;--------------------------------------------------------------------------------
;; Settings
;;--------------------------------------------------------------------------------

;;------------------------------
;; Customizable
;;------------------------------

(defcustom path:uniquify:directory/end-in-slash? t
  "If non-nil, add a path separator to end of names that are dirs (eg `Dired')."
  :group 'path:group
  :type  'boolean)


(defconst path:uniquify:ignore/buffer:name/rx:defaults
  '(;;---
    ;; Magit Buffers
    ;;---
    "magit"
    (optional "-"
              (one-or-more alphanumeric))
    ": "
    (one-or-more printing)))


(defcustom path:uniquify:ignore/buffer:name/rx
  (append '(and string-start)
          path:uniquify:ignore/buffer:name/rx:defaults
          '(string-end))
  "`rx' Regular Expression Sexpr for buffer names that we should _not_ mess with.

Will get `string-start' and `string-end' added to it before being compiled to a
regex string.

Add on to this variable like so:
\(customize-set-variable
 path:uniquify:ignore/buffer:name/rx
 (list 'or
       ;; Be more idempotent; use the original value:
       (list 'and (default-value 'path:uniquify:ignore/buffer:name/rx))
       ;; Add my custom buffer regexes on:
       ;; say... ignore the asterisk buffers?
       '(group
         (optional \" \")
         \"*\" ;; literal asterisk
         (one-or-more printing)
         \"*\")))"
  :group 'path:group
  :type  'sexp)


(defun path:uniquify:ignore/buffer:name/rx ()
  "Compile variable `path:uniquify:ignore/buffer:name/rx' to a regex string."
  (rx-to-string path:uniquify:ignore/buffer:name/rx :no-group))
;; path:uniquify:ignore/buffer:name/rx
;; (pp path:uniquify:ignore/buffer:name/rx)
;; (path:uniquify:ignore/buffer:name/rx)


(defcustom path:uniquify:ignore/buffer:mode/major nil
  "List of major modes that we should not mess with."
  :group 'path:group
  :type  '(repeat symbol))


(defcustom path:uniquify:directory:modes '(dired-mode cvs-mode vc-dir-mode)
  "List of modes that we should use `list-buffers-directory'.

Some buffers have no variable `buffer-file-name', but do have a
`list-buffers-directory'that describes the directory they represent (e.g.
`Dired' buffers). Use `list-buffers-directory' instead for them."
  :group 'path:group
  :type  '(repeat symbol))


;;------------------------------
;; Variables
;;------------------------------

(defvar-local path:uniquify:settings/local nil
  "Non-nil if the name of this buffer is managed by `path:uniquify'.

Value should be an alist of keys:
  - `:path/parent'      - string: absolute path to parent directory
  - `:path'             - string: absolute path to file(/directory) itself
  - `:project'          - alist:  return value of `path:project:current/alist'
  - `:name/requested'   - string: desired/proposed buffer name from Emacs
  - `:name'             - string: actual buffer name decided by us
  - `:name/propertized' - string: propertized NAME string")
(put 'path:uniquify:settings/local 'permanent-local t) ; Always a buffer-local variable.


(defun path:uniquify:settings/path/normalize (path &rest segments)
  "Normalize PATH and SEGMENTS strings into a path string."
  ;; Always use "abbreviations" in path (i.e. "~" for user's home), and default
  ;; to file path...
  (let ((path (apply #'path:abbreviate:file path segments)))
    ;; Do we actually want this to be a directory path?
    (if (and path:uniquify:directory/end-in-slash?
             (file-directory-p path))
        (path:directory path)
      path)))
;; (path:uniquify:path/normalize (path:current:file))
;; (path:uniquify:path/normalize (path:current:dir))
;; (setq path:uniquify:directory/end-in-slash? nil)
;; (setq path:uniquify:directory/end-in-slash? t)


(defun int<path>:uniquify:settings/create (path/absolute/directory
                                           name/requested)
  "Create and return settings alist from args.
NOTE: Does not set `path:uniquify:settings/local'!

NAME/REQUESTED should be a string of the buffer name that Emacs (via
 `rename-buffer') desires.

PATH/ABSOLUTE/DIRECTORY should be a string of the absolute path to the file's
parent directory."
  ;; Figure out new buffer name.
  (if-let* ((path/absolute/file (path:uniquify:settings/path/normalize path/absolute/directory name/requested))
            (project            (path:project:current/alist path/absolute/file)))
      ;; We have a project, so we can figure out a name.
      (list (cons :path/parent      path/absolute/directory)
            (cons :path             path/absolute/file)
            (cons :project          project)
            (cons :name/requested   name/requested)
            ;; And the actual name:
            (cons :name             (path:project:buffer/name project))
            (cons :name/propertized (path:project:buffer/name project :pretty)))

    ;; No project... Fallback to full path?
    (list (cons :path/parent      path/absolute/directory)
          (cons :path             path/absolute/file)
          (cons :project          project)
          (cons :name/requested   name/requested)
          ;; And the actual name:
          (cons :name             path/absolute/file)
          (cons :name/propertized path/absolute/file))))
;; (int<path>:uniquify:settings/create (path:parent (path:current:file)) (file:name (path:current:file)))


(defun path:uniquify:settings/create (path/absolute/directory
                                      name/requested)
  "Create settings alist from args.

NAME/REQUESTED should be a string of the buffer name that Emacs (via
 `rename-buffer') desires.

PATH/ABSOLUTE/DIRECTORY should be a string of the absolute path to the file's
parent directory."
  (setq path:uniquify:settings/local
        (int<path>:uniquify:settings/create path/absolute/directory
                                            name/requested)))


(defun path:uniquify:settings/clear ()
  "Clear `path:uniquify:settings/local' by setting it to nil."
  (setq path:uniquify:settings/local nil))


(defun path:uniquify:settings/get (keyword)
  "Get KEYWORD's value from the local variable `path:uniquify:settings/local'."
  (alist-get keyword path:uniquify:settings/local))


(defun path:uniquify:settings/set (keyword value)
  "Set KEYWORD to VALUE in the local variable `path:uniquify:settings/local'."
  (setf (alist-get keyword path:uniquify:settings/local)
        value))


;;--------------------------------------------------------------------------------
;; Buffers
;;--------------------------------------------------------------------------------

(defun path:uniquify:buffer/managed? (buffer)
  "Is the name of BUFFER already managed by `path:uniquify'?

Return nil/non-nil."
  (with-current-buffer buffer
    ;; If our local var is non-nil, we are managing this buffer's name.
    path:uniquify:settings/local))


(defun path:uniquify:buffer/should-manage? (buffer)
  "Should we be managing BUFFER's name?

BUFFER should be a buffer object.

Will check settings:
  - `path:uniquify:ignore/buffer:name/rx'
  - `path:uniquify:ignore/buffer:mode/major'"
  (and (not
        ;; Should we ignore due to buffer name regexes?
        (and path:uniquify:ignore/buffer:name/rx
             (string-match (path:uniquify:ignore/buffer:name/rx) (buffer-name buffer))))
       (not
        ;; Should we ignore due to buffer's mode?
        (memq major-mode path:uniquify:ignore/buffer:mode/major))))
;; (path:uniquify:buffer/should-manage? (current-buffer))


(defun path:uniquify:buffer:name/set (buffer)
  "Update BUFFER's name with data from `path:uniquify:settings/local'.

BUFFER should be a buffer object.
`path:uniquify:settings/local' should be up-to-date."
  (with-current-buffer buffer
    (let ((name (path:uniquify:settings/get :name)))
      (unless (equal name (buffer-name buffer))
        ;; Use non-nil `unique' arg in order to avoid infinite loop recursion.
        (rename-buffer name :unique)))))


(defun path:uniquify:buffer:path/absolute/directory (buffer)
  "Return path of the parent directory that BUFFER is visiting, or nil if none.

Valid for:
  1. File-visiting buffers.
  2. Buffers with major-mode in `path:uniquify:directory:modes'.
Other buffers return nil."
  (with-current-buffer buffer
    (when-let ((filename
                (or buffer-file-name
                    (if (memq major-mode path:uniquify:directory:modes)
                        list-buffers-directory))))
      (path:dir (path:parent filename)))))
;; (path:uniquify:buffer:path/absolute/directory (current-buffer))
;; buffer-file-name
;; (buffer-file-name)


;;--------------------------------------------------------------------------------
;; Commands
;;--------------------------------------------------------------------------------

(defun path:cmd:uniquify:buffer/test ()
  "Find out what we would name the current buffer if we were naming buffers."
  (interactive)
  (let* ((buffer (current-buffer))
         (name (buffer-name buffer)))
    ;; Say what we're already doing:
    (cond ((path:uniquify:buffer/managed? buffer)
           (message "Buffer is managed; name: %S"
                    (path:uniquify:settings/get :name)))

          ;; Say why we wouldn't do anything.
          ((not (path:uniquify:buffer/should-manage? buffer))
           ;; Ignored due to buffer name regexes?
           (cond ((and path:uniquify:ignore/buffer:name/rx
                       (string-match (path:uniquify:ignore/buffer:name/rx) name))
                  (message "Ignore buffer due to name regex: %S"
                           name))
                 ;; Ignored due to buffer's mode?
                 ((memq major-mode path:uniquify:ignore/buffer:mode/major)
                  (message "Ignore buffer due to name regex: %S"
                           name))
                 ;; ???
                 (t
                  (message "Ignore buffer due to... unknown circumstances???: %S"
                           name))))

          ;; Say what we would do if we were doing things.
          (t
           (let ((settings (int<path>:uniquify:settings/create (path:parent (path:current:file))
                                                               (file:name (path:current:file)))))
             (message "Would name buffer: %S"
                      (alist-get :name settings))))))
  nil)
;; (path:cmd:uniquify:buffer/test)


;;--------------------------------------------------------------------------------
;; Advice
;;--------------------------------------------------------------------------------

;;------------------------------
;; Special 'desktop.el' Hand-Holding
;;------------------------------

(defun path:uniquify:get:name/requested ()
  "Return `path:uniquify' saved `:name/requested' setting for this buffer.

Indended as alias for `uniquify-buffer-base-name'."
    ;; Return something if we have something, nil otherwise.
    (and (path:uniquify:buffer/managed? (current-buffer))
         (path:uniquify:settings/get :name/base)))


(defun path:advice:uniquify:uniquify-buffer-base-name (func &rest args)
  "Return `path:uniquify' saved `:name/requested' setting for this buffer.

Indended as `:around' advice for FUNC `rename-buffer'. Prefer returning
`uniquify-buffer-base-name' result; return ours if they have nothing.

ARGS are just for future-proofing call to `uniquify-buffer-base-name'."
  (if-let ((name/base (apply func args)))
      name/base
    (path:uniquify:get:name/requested)))


;;------------------------------
;; Buffer (Re)Naming
;;------------------------------

(defun path:uniquify:rename/create (name/requested path/absolute/directory buffer)
  "Create file-backed BUFFER's full name given the args.

NAME/REQUESTED should be a string of the buffer name that Emacs (via
 `rename-buffer') desires.

PATH/ABSOLUTE/DIRECTORY should be a string of the absolute path to the file's
parent directory.

BUFFER should be the buffer's buffer object."
  ;; Clear any old settings.
  (with-current-buffer buffer
    (path:uniquify:settings/clear))

  ;; Figure out new buffer name.
  (path:uniquify:settings/create path/absolute/directory
                                 name/requested)

  ;; Return the buffer name we created.
  (path:uniquify:settings/get :name/propertized))


(defun path:advice:uniquify:rename-buffer (func name/requested &optional unique? &rest args)
  "Uniquify file-backed buffer names with parts their path.

Indended as `:around' advice for FUNC `rename-buffer'.

NAME/REQUESTED should be a string of the buffer's intended new name.
UNIQUE should be nil/non-nil.
ARGS are just for future-proofing call to `create-file-buffer'.

Return a string of the name actually given to the buffer."
  ;; Find out what Emacs wants to call this buffer.
  (let ((name/proposed (apply func name/requested unique? args))
        (buffer (current-buffer))
        name/actual)

    ;; Tweak this buffer's (re)name?
    (if (not (or unique?
                 (path:uniquify:buffer/should-manage? buffer)))
        ;;------------------------------
        ;; Ignore Buffer
        ;;------------------------------
        (progn
          ;; Don't mess  with it; mark this buffer as not-named-by-us and leave its name alone.
          (path:uniquify:settings/clear)
          (setq name/actual name/proposed))

      ;;------------------------------
      ;; Tweak Buffer (Re)Name
      ;;------------------------------
      ;; Clear any old settings.
      (with-current-buffer buffer
        (path:uniquify:settings/clear))

      ;; Figure (& save) out /our/ name for this buffer.
      (path:uniquify:settings/create path/absolute/directory
                                     name/requested)

      ;; Actually set the buffer's new name.
      (path:uniquify:buffer:name/set buffer)
      (setq name/actual (buffer-name buffer)))

    ;;------------------------------
    ;; Return Name
    ;;------------------------------
    name/actual))


;;------------------------------
;; Buffer / File Creation
;;------------------------------

(defun path:advice:uniquify:create-file-buffer (func filepath &rest args)
  "Uniquify buffer names with parts of directory name.

Intended as `:around' advice for FUNC `create-file-buffer'.

FILEPATH should be the path to the file being created.
ARGS are just for future-proofing call to `create-file-buffer'.

Return the buffer created by `create-file-buffer'."
  ;; Get a name from `create-file-buffer'...
  ;; But mainly the rest of the stuff it does.
  (let ((name/proposed (apply func filepath args))
        (buffer        (current-buffer))
        name/actual)
    (if (not (path:uniquify:buffer/should-manage? buffer))
        ;;------------------------------
        ;; Ignore This Buffer
        ;;------------------------------
        (progn
          ;; Don't mess  with it; mark this buffer as not-named-by-us and leave its name alone.
          (path:uniquify:settings/clear)
          (setq name/actual name/proposed))

      ;;------------------------------
      ;; Tweak Buffer Name
      ;;------------------------------
      ;; Figure out a buffer name.
      (path:uniquify:settings/create (path:parent filepath) name/proposed)

      ;; Actually set the buffer's new name (& return it).
      (path:uniquify:buffer:name/set buffer)

      ;; Return the buffer name we created.
      (setq name/actual (path:uniquify:settings/get :name/propertized)))

    ;;------------------------------
    ;; Return Name
    ;;------------------------------
    name/actual))


;;--------------------------------------------------------------------------------
;; Set-Up
;;--------------------------------------------------------------------------------

(defun path:uniquify:set-up ()
  "Set up hooks and advice for `path:uniquify'."
  (advice-add 'rename-buffer :around #'path:advice:uniquify:rename-buffer)
  (advice-add 'create-file-buffer :around #'path:advice:uniquify:create-file-buffer)

  ;; Alias if `uniquify' isn't loaded (yet).
  (unless (functionp 'uniquify-buffer-base-name)
    (defalias 'uniquify-buffer-base-name 'path:uniquify:get:name/requested))

  ;; Advice if `uniquify' is loaded (now or later).
  (eval-after-load 'uniquify
    ;; ...but only advise if we're enabled (still) by then...
    (when (advice-member-p #'path:advice:uniquify:rename-buffer #'rename-buffer)
      (advice-add 'uniquify-buffer-base-name :around #'path:advice:uniquify:uniquify-buffer-base-name))))


;; TODO:emacs-29: Delete when upgraded to Emacs 29!
;; TODO:package: Or depend on compat package for this function?
(if (version< emacs-version "29")
    (unless (functionp 'function-alias-p)
      (defun function-alias-p (func &optional noerror)
        "Return nil if FUNC is not a function alias.
If FUNC is a function alias, return the function alias chain.

If the function alias chain contains loops, an error will be
signalled.  If NOERROR, the non-loop parts of the chain is returned."
        (declare (side-effect-free t))
        (let ((chain nil)
              (orig-func func))
          (nreverse
           (catch 'loop
             (while (and (symbolp func)
                         (setq func (symbol-function func))
                         (symbolp func))
               (when (or (memq func chain)
                         (eq func orig-func))
                 (if noerror
                     (throw 'loop chain)
                   (signal 'cyclic-function-indirection (list orig-func))))
               (push func chain))
             chain)))))
  (nub:warning
      :innit
      caller
    '("Emacs %s: This `if' block for `function-alias-p' can be deleted once "
      "Emacs 29+ is on all systems.")
    emacs-version))


(defun path:uniquify:tear-down ()
  "Remove `path:uniquify' from advice, etc. and reset buffer names."
  (save-current-buffer
    (let (buffers/managed)
      ;;------------------------------
      ;; Prep
      ;;------------------------------
      ;; Gather a list of buffers to revert from all buffers.
      (dolist (buffer (buffer-list))
        (set-buffer buffer)
        (when (path:uniquify:buffer/managed? (current-buffer))
          (push (cons buffer (path:uniquify:settings/get :name/requested))
                buffers/managed)))

      ;;------------------------------
      ;; Clean-Up
      ;;------------------------------
      (advice-remove 'rename-buffer             #'path:advice:uniquify:rename-buffer)
      (advice-remove 'create-file-buffer        #'path:advice:uniquify:create-file-buffer)
      ;; Delete `uniquify-buffer-base-name' if it's our alias.
      (when (and (functionp 'uniquify-buffer-base-name)
                 (memq 'path:uniquify:get:name/requested
                       (function-alias-p 'uniquify-buffer-base-name :no-error)))
        ;; Delete it with extreme predjudice.
        (fmakunbound 'uniquify-buffer-base-name)
        (makunbound 'uniquify-buffer-base-name))
      ;; Unadvise `uniquify-buffer-base-name' if it's not our alias.
      (advice-remove 'uniquify-buffer-base-name #'path:advice:uniquify:uniquify-buffer-base-name)

      ;; Revert all the buffers we renamed to their original/desired name.
      (dolist (buffer buffers/managed)
        (set-buffer (car buffer))
        (rename-buffer (cdr buffer) t))))

  nil)


;;------------------------------------------------------------------------------
;; `Uniquify': Project Path Function for `uniquify-buffer-name-style'
;;------------------------------------------------------------------------------
;;
;; NOTE: Failed attempt to use `uniquify' with a function as `uniquify-buffer-name-style'...
;; But `base' and `extra-strings' just didn't give us enough info about the file.
;;
;; (defun path:project:uniquify (base extra-strings)
;;   "`uniquify-buffer-name-style' function for unique-by-project-path buffer names.
;;
;; BASE should be a string.
;; EXTRA-STRINGS should be a string.
;;
;; Return a string that `uniquify' should use to name the buffer.
;;
;; NOTE: Cannot use a lot of very useful functions here, as the buffer is likely
;; nascent and doesn't have things like variable `buffer-file-name' or `major-mode'
;; defined yet."
;;   (cond
;;    ;;------------------------------
;;    ;; Special Modes
;;    ;;------------------------------
;;    ;; NOTE: Cannot use e.g. `major-mode' as it's not been set yet... :|
;;
;;    ;; Magit already does a lot to uniquify its buffer names.
;;    ((string-match-p (rx-to-string '(sequence string-start
;;                                              "magit"
;;                                              (optional "-"
;;                                                        (one-or-more alphanumeric))
;;                                              ": "
;;                                              (one-or-more printing)
;;                                              string-end)
;;                                   :no-group)
;;                     base)
;;     ;; Just use the base name.
;;     base)
;;
;;    ;;------------------------------
;;    ;; File-Visiting Buffers
;;    ;;------------------------------
;;    ;; Do the full buffer naming shenanigans... if possible, else skip down to fallback.
;;    ((when-let* ((path (cond
;;                        ;; If we have no BASE, um... assume it's just a dir maybe? IDK.
;;                        ;; When does this come up? It's a check in the `uniquify' source code...
;;                        ;; I think it's when it's a directory? See help for `uniquify-get-proposed-name'.
;;                        ((or (not (stringp base))
;;                             (string-empty-p base))
;;                         default-directory)
;;
;;                        ;; `uniquify' has been set up to notify us about dirs? Perfect!
;;                        ((and path:uniquify:directory/end-in-slash?
;;                              (path:directory? base))
;;                         ;; Have a directory... just use `default-directory'.
;;                         default-directory)
;;
;;                        ((and path:uniquify:directory/end-in-slash?
;;                              (not (path:directory? base)))
;;                         (path:join default-directory base))
;;
;;                        ;; Not our problem; goto to the `cond' fallback.
;;                        (t
;;                         nil)))
;;                 (project      (path:project:current/alist path))
;;                 (project/root (alist-get :project/name project))
;;                 (project/path (alist-get :path         project)))
;;       ;; Return the fancy/pretty version, or just the relative path string?
;;       (concat (propertize project/root 'face 'underline)
;;               path:vc/git:rooted
;;               project/path)))
;;
;;    ;;------------------------------
;;    ;; Default/Fallback
;;    ;;------------------------------
;;    (t
;;     ;;---
;;     ;; Default
;;     ;;---
;;     ;; No simple fallback for using `uniquify' itself... So just do something
;;     ;; dumb and simple?
;;     (concat (mapconcat #'identity extra-strings "/") "/" base)
;;
;;     ;;---
;;     ;; Alternatives:
;;     ;;---
;;     ;; Do something similar to `uniquify-buffer-name-style' `post-forward'?
;;     ;;     (concat base ":" (mapconcat #'identity extra-strings "/"))
;;
;;     ;; Just use the base? IDK?
;;     ;; base
;;     )))
;; ;; (path:project:uniquify "base.txt" '("path" "to"))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :path '+uniquify)

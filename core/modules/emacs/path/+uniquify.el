;;; core/modules/emacs/path/+uniquify.el --- Project-Based Unique Buffer Names -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2022-10-20
;; Timestamp:  2023-07-28
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Unique Buffer Names
;;
;; `uniquify' only gives you a very little bit of context to use if you want to
;; try to create your own buffer uniquifying function. It does this because it
;; expects to be in control of what is unique about the buffer's name.
;;
;; `path:uniquify' uniquify off of a project / version control root directory
;; and relative path, so we have to just ignore `uniquify' and wander out on our
;; own.
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
  '(or
    ;;---
    ;; Emacs
    ;;---
    ;; Special buffers start with "*", optionally with leading space.
    (group
      (optional " ")
      "*" ;; literal asterisk
      (one-or-more printing)
      "*")

    ;;---
    ;; Magit Buffers
    ;;---
    (group
      "magit"
      (optional "-"
                (one-or-more alphanumeric))
      ": "
      (one-or-more printing))))


(defcustom path:uniquify:ignore/buffer:name/rx
  (list 'and
        'string-start
        path:uniquify:ignore/buffer:name/rx:defaults
        'string-end)
  "`rx' Regular Expression Sexpr for buffer names that we should _not_ mess with.

Will get `string-start' and `string-end' added to it before being compiled to a
regex string.

Add on to this variable like so:
\(customize-set-variable
 path:uniquify:ignore/buffer:name/rx
 (list 'and
       'string-start
       (list 'or
             ;; The defaults...
             path:uniquify:ignore/buffer:name/rx:defaults
             ;; Or my special buffers.
             '(group
               (optional \" \")
               (or \"§-\" \"§!\" \"ⓘ-\")
               (optional \" \")
               (one-or-more printing)
               (optional \" \")
               (or \"-§\" \"!§\" \"-ⓘ\"))
             buffer:regex:bookend)
       'string-end))

Use function `path:uniquify:ignore/buffer:name/rx' to see if what you created
compiles to a regex string or not."
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

Value should be an alist-tree of keywords:
  - `:path'    - alist
    - `:parent'   - string: absolute path to parent directory
    - `:filepath' - string: absolute path to file(/directory) itself
    - `:filename' - string: basename of `:filepath' (with extension)
  - `:project' - alist: return value of `path:project:current/alist'
  - `:name'    - alist
    - `:requested'          - string: desired/proposed buffer name from Emacs
    - `:buffer'             - string: actual buffer name decided by us
    - `:buffer/propertized' - string: propertized NAME string")
(put 'path:uniquify:settings/local 'permanent-local t) ; Always a buffer-local variable.
;; (get 'path:uniquify:settings/local 'permanent-local)


(defun path:uniquify:settings/path/normalize (path &rest segments)
  "Normalize PATH and SEGMENTS strings into a path string."
  ;; Always use "abbreviations" in path (i.e. "~" for user's home), and default
  ;; to file path...
  (let ((path (apply #'path:abbreviate:file path segments)))
    ;; Do we actually want this to be a directory path?
    (if (and path:uniquify:directory/end-in-slash?
             ;; Is this, like, really actually a directory? Go find out.
             (file-directory-p path))
        (path:directory path)
      path)))
;; (path:uniquify:settings/path/normalize (path:current:file))
;; (path:uniquify:settings/path/normalize (path:current:dir))
;; (setq path:uniquify:directory/end-in-slash? nil)
;; (setq path:uniquify:directory/end-in-slash? t)


(defun int<path>:uniquify:settings/create (path/absolute/directory
                                           name/requested
                                           buffer)
  "Create and return settings alist from args.
NOTE: Does not set `path:uniquify:settings/local'!

NAME/REQUESTED should be a string of the buffer name that Emacs (via
 `rename-buffer') desires.

PATH/ABSOLUTE/DIRECTORY should be a string of the absolute path to the file's
parent directory."
  (with-current-buffer buffer
    ;; Figure out new buffer name.
    (let* ((path/absolute/file (path:uniquify:settings/path/normalize path/absolute/directory
                                                                      name/requested))
           (project            (path:project:current/alist path/absolute/file))
           ;; Assuming we don't have enough info, the name will just be the path.
           (name/buffer/naked       path/absolute/file)
           (name/buffer/propertized path/absolute/file))

      ;;------------------------------
      ;; Name the buffer.
      ;;------------------------------
      ;; If we have a `project', we can figure out our name for the buffer.
      (when project
        (setq name/buffer/naked
              ;; Create the buffer name from the project settings.
              (path:project:buffer/name:propertize
               :buffer       buffer
               ;; Do not provide any text properteries in these!
               :project/name (list (alist-get :project/name project))
               :project/path (list (alist-get :path project))))

        (setq name/buffer/propertized
              ;; Create the buffer name from the project settings.
              (path:project:buffer/name:propertize
               :buffer       buffer
               ;; Do propertize these as desired.
               :project/name (list (alist-get :project/name project)
                                   'face 'underline)
               :project/path (list (alist-get :path project)))))

      ;;------------------------------
      ;; Create our settings (alist tree structure).
      ;;------------------------------
      (list (list :path
                  (cons :parent    path/absolute/directory)
                  (cons :filepath  path/absolute/file)
                  (cons :filename  (file:name path/absolute/file)))
            (cons :project         project)
            (list :name
                  ;; Name Emacs asked for.
                  (cons :requested name/requested)
                  ;; And the actual `buffer-name'.
                  (cons :buffer             name/buffer/naked)
                  (cons :buffer/propertized name/buffer/propertized)
                  ;; TODO: these?
                  ;; ;; And stuff for the modeline:
                  ;; (cons :modeline                "TODO: foo")
                  ;; (cons :modeline/propertized    "TODO: foo")
                  )))))
;; (int<path>:uniquify:settings/create (path:parent (path:current:file)) (file:name (path:current:file)) (current-buffer))


(defun path:uniquify:settings/create (path/absolute/directory
                                      name/requested
                                      buffer)
  "Create settings alist from args.

NAME/REQUESTED should be a string of the buffer name that Emacs (via
 `rename-buffer') desires.

PATH/ABSOLUTE/DIRECTORY should be a string of the absolute path to the file's
parent directory."
  (with-current-buffer buffer
    (setq path:uniquify:settings/local
          (int<path>:uniquify:settings/create path/absolute/directory
                                              name/requested
                                              buffer))))
;; (path:uniquify:settings/create (path:parent (path:current:file)) (file:name (path:current:file)) (current-buffer))
;; path:uniquify:settings/local


(defun path:uniquify:settings/clear (buffer)
  "Clear `path:uniquify:settings/local' by setting it to nil.

BUFFER should be a buffer object."
  (with-current-buffer buffer
    (setq path:uniquify:settings/local nil)))


(defun int<path>:uniquify:settings/get (keywords settings)
  "Get KEYWORDS value from the SETTINGS alist tree.

KEYWORDS should be:
  - a keyword: `:project'
  - a list of keywords: `(:path :filepath)'

SETTINGS should be `path:uniquify:settings/local' or a sub-alist thereof."
  (let ((func/name "int<path>:uniquify:settings/get"))
    (cond ((null keywords)
           (nub:error
               :innit
               func/name
             '("Cannot get value in settings; no keyword? "
               "keywords: %S, buffer: %S, settings: %S")
             keywords
             buffer
             settings))

          ;; Actually get the value.
          ((keywordp keywords)
           (alist-get keywords settings))

          ((and (listp keywords)
                (= 1 (length keywords)))
           (alist-get (car keywords) settings))

          ;; Recurse into settings a level.
          ((listp keywords)
           (let ((keyword (pop keywords)))
             (int<path>:uniquify:settings/get keywords
                                              (alist-get keyword settings))))

          ;; ???
          (t
           (nub:error
               :innit
               func/name
             '("Cannot get value in settings; don't know what to do with keywords! "
               "keywords: %S, settings: %S")
             keywords
             settings)))))


(defun path:uniquify:settings/get (keywords buffer)
  "Get KEYWORDS value from the local variable `path:uniquify:settings/local'.

BUFFER should be a buffer object.

KEYWORDS should be:
  - a keyword: `:project'
  - a list of keywords: `(:path :filepath)'"
  (with-current-buffer buffer
    (int<path>:uniquify:settings/get keywords path:uniquify:settings/local)))
;; path:uniquify:settings/local
;; (path:uniquify:settings/get '(:path :parent) (current-buffer))


(defun int<path>:uniquify:settings/set (keywords value settings)
  "Set KEYWORDS to VALUE in the local variable `path:uniquify:settings/local'.

KEYWORDS should be:
  - a keyword: `:project'
  - a list of keywords: `(:path :filepath)'

SETTINGS should be `path:uniquify:settings/local' or a sub-alist.

NOTE: Must be call in context of buffer that owns SETTINGS."
  (let ((func/name "int<path>:uniquify:settings/set"))
    (cond ((null keywords)
           (nub:error
               :innit
               func/name
             '("Cannot set value in settings; no keyword? "
               "keywords: %S, value: %S, settings: %S")
             keywords
             value
             settings))

          ;; Actually set the value.
          ((keywordp keywords)
           ;; Return this level of settings.
           (setf (alist-get keywords settings)
                 value))

          ((and (listp keywords)
                (= 1 (length keywords)))
           (setf (alist-get (car keywords) settings)
                 value))

          ;; Recurse into settings a level.
          ((listp keywords)
           (let ((keyword (pop keywords)))
             (setf (alist-get keyword settings)
                   ;; Recurse into w/ rest of keywords to update/set value.
                   (int<path>:uniquify:settings/set keywords
                                                    value
                                                    (alist-get keyword settings)))

             ))

          ;; ???
          (t
           (nub:error
               :innit
               func/name
             '("Cannot set value in settings; don't know what to do with keywords! "
               "keywords: %S, value: %S, settings: %S")
             keywords
             value
             settings)))

    ;; Return this level of settings.
    settings))
;; path:uniquify:settings/local


(defun path:uniquify:settings/set (keywords value buffer)
  "Set KEYWORDS to VALUE in the local variable `path:uniquify:settings/local'.

BUFFER should be a buffer object.

KEYWORDS should be:
  - a keyword: `:project'
  - a list of keywords: `(:path :filepath)'"
  (with-current-buffer buffer
    (int<path>:uniquify:settings/set keywords value path:uniquify:settings/local)))
;; (path:uniquify:settings/create (path:parent (path:current:file)) (file:name (path:current:file)) (current-buffer))
;; (pp path:uniquify:settings/local)
;; (path:uniquify:settings/get '(:path :filename) (current-buffer))
;; (path:uniquify:settings/set '(:path :filename) "jeff" (current-buffer))


;;--------------------------------------------------------------------------------
;; Buffers
;;--------------------------------------------------------------------------------

(defun path:uniquify:buffer/managed? (buffer)
  "Is the name of BUFFER already managed by `path:uniquify'?

Return nil/non-nil."
  (with-current-buffer buffer
    ;; If our local var is non-nil, we are managing this buffer's name.
    path:uniquify:settings/local))
;; (path:uniquify:buffer/managed? (current-buffer))


(defun path:uniquify:buffer/should-manage? (buffer)
  "Should we be managing BUFFER's name?

BUFFER should be a buffer object.

Will check settings:
  - `path:uniquify:ignore/buffer:name/rx'
  - `path:uniquify:ignore/buffer:mode/major'"
  ;; Our settings are of the "ignore this?" variety, so there's not a small number of nots.
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
    (let ((func/name "path:uniquify:buffer:name/set")
          (name (path:uniquify:settings/get '(:name :buffer) buffer)))
      ;;------------------------------
      ;; Error Checks
      ;;------------------------------
      (cond ((null name)
             ;; Error for now so we can work on hunting this down.
             ;; Ideally no erroring? Maybe?
             (nub:error
                 :innit
                 func/name
               '(:line:each
                 "Cannot name this buffer; no name in settings?!"
                 "`%s': %S")
               (symbol-name 'path:uniquify:settings/local)
               path:uniquify:settings/local))
            ((not (stringp name))
             (nub:error
                 :innit
                 func/name
               '(:line:each
                 "Cannot name this buffer; no name in settings?!"
                 "`%s': %S")
               (symbol-name 'path:uniquify:settings/local)
               path:uniquify:settings/local))
            ;;------------------------------
            ;; Buffer Naming (Or Not)
            ;;------------------------------
            ((string= name (buffer-name buffer))
             ;; Nothing to do; name already set?
             nil)
            (t
             ;; Use non-nil UNIQUE arg in order to avoid infinite loop recursion
             ;; due to our advising of `rename-buffer'.
             (rename-buffer name :unique))))))


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
                    (path:uniquify:settings/get '(:name :buffer) buffer)))

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
                      (int<path>:uniquify:settings/get '(:name :buffer) settings))))))
  nil)
;; (path:cmd:uniquify:buffer/test)


;;--------------------------------------------------------------------------------
;; Advice
;;--------------------------------------------------------------------------------

;;------------------------------
;; Special 'desktop.el' Hand-Holding
;;------------------------------

(defun path:uniquify:get:name/requested ()
  "Return `path:uniquify' saved `:name :requested' setting for this buffer.

Indended as alias for `uniquify-buffer-base-name'."
    ;; Return something if we have something, nil otherwise.
    (and (path:uniquify:buffer/managed? (current-buffer))
         (path:uniquify:settings/get '(:name :requested) (current-buffer))))


(defun path:advice:uniquify:uniquify-buffer-base-name (func &rest args)
  "Return `path:uniquify' saved `:name :requested' setting for this buffer.

Indended as `:around' advice for FUNC `rename-buffer'. Prefer returning
`uniquify-buffer-base-name' result; return ours if they have nothing.

ARGS are just for future-proofing call to `uniquify-buffer-base-name'."
  (if-let ((name/base (apply func args)))
      name/base
    (path:uniquify:get:name/requested)))


;;------------------------------
;; Buffer (Re)Naming
;;------------------------------


(defun path:advice:uniquify:rename-buffer (func name/requested &optional unique? &rest args)
  "Uniquify file-backed buffer names with parts their path.

Indended as `:around' advice for FUNC `rename-buffer'.

NAME/REQUESTED should be a string of the buffer's intended new name.
UNIQUE should be nil/non-nil.
ARGS are just for future-proofing call to `rename-buffer'.

Return a string of the name actually given to the buffer."
  ;; Find out what Emacs wants to call this buffer.
  (let ((name/proposed (apply func name/requested unique? args))
        (buffer (current-buffer))
        name/actual)

    ;; Tweak this buffer's (re)name?
    (if (and (not unique?)
             (path:uniquify:buffer/should-manage? buffer))
        (progn
          ;;------------------------------
          ;; Tweak Buffer (Re)Name
          ;;------------------------------
          ;; Clear any old settings.
          (path:uniquify:settings/clear buffer)

          ;; Figure (& save) out /our/ name for this buffer.
          (path:uniquify:settings/create (path:parent (path:current:file))
                                         name/proposed
                                         buffer)

          ;; Actually set the buffer's new name.
          (path:uniquify:buffer:name/set buffer)
          (setq name/actual (buffer-name buffer)))

      ;;------------------------------
      ;; Ignore Buffer
      ;;------------------------------
      ;; Don't mess  with it; mark this buffer as not-named-by-us and leave its name alone.
      (path:uniquify:settings/clear buffer)
      (setq name/actual name/proposed))

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
  (let ((buffer (apply func filepath args)))
    (if (path:uniquify:buffer/should-manage? buffer)
        ;;------------------------------
        ;; Tweak Buffer Name
        ;;------------------------------
        (progn
          ;; Figure out a buffer name.
          (path:uniquify:settings/create (path:parent filepath)
                                         (file:name filepath)
                                         buffer)

          ;; Actually set the buffer's new name (& return it).
          (path:uniquify:buffer:name/set buffer))

      ;;------------------------------
      ;; Ignore This Buffer
      ;;------------------------------
      ;; Don't mess  with it; mark this buffer as not-named-by-us and leave its name alone.
      (path:uniquify:settings/clear buffer))

    ;;------------------------------
    ;; Return Buffer Object
    ;;------------------------------
    buffer))


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
;; (path:uniquify:set-up)


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
        (with-current-buffer buffer
          (when (path:uniquify:buffer/managed? buffer)
            (push (cons buffer (path:uniquify:settings/get '(:name :requested) buffer))
                  buffers/managed))))

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
      (dolist (managed buffers/managed)
        (with-current-buffer (car managed)
          (rename-buffer (cdr managed) t)))

      ;; Clear settings in separate step in case we're having bugs.
      (dolist (managed buffers/managed)
        (path:uniquify:settings/clear (car managed)))))

  nil)
;; (path:uniquify:tear-down)


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :path '+uniquify)

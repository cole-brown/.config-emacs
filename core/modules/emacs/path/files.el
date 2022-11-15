;;; -*- lexical-binding: t; -*-

;;-------------------------------------spy--------------------------------------
;;--                             File Functions                               --
;;----------------------------/mnt/hello/there.txt------------------------------

(require 'cl-lib)


(imp:require :path 'path)


;;------------------------------------------------------------------------------
;; File Name
;;------------------------------------------------------------------------------

(defun file:name (path &rest segment)
  "Return the file name (with extension) from PATH & SEGMENT(s).

Example:
  (file:name \"/foo\" \"bar.tar.gz\")
  (file:name \"/foo/bar.tar.gz\")
  (file:name \"c:/foo/bar.tar.gz\")
    -> \"bar.tar.gz\""
  (file-name-nondirectory (apply #'path:join path segment)))
;; (file:name "/foo" "bar.tar.gz")


(defun file:name:base (path &rest segment)
  "Return the file name (sans extension) from PATH & SEGMENT(s).

Only removes one extension if there are multiple.

Examples:
  (file:name \"/foo\" \"bar.baz\")
  (file:name \"/foo/bar.baz\")
  (file:name \"c:/foo/bar.baz\")
    -> \"bar\"

  (file:name \"/foo\" \"bar.tar.gz\")
  (file:name \"/foo/bar.tar.gz\")
  (file:name \"c:/foo/bar.tar.gz\")
    -> \"bar.tar\""
  (file-name-sans-extension (file:name path segment)))
;; (file:name "c:/foo/bar.baz")


;;------------------------------------------------------------------------------
;; Filters / Ignore Regexes
;;------------------------------------------------------------------------------

(defun files:ignore/regex (rx/path/partial &optional ignore)
  "Create a regex for ignoring RX/PATH/PARTIAL.

RX/PATH/PARTIAL should be a regex string. It can be either absolute or relative.
If relative, it can start with or without a path separator. Both Unix and
Windows type paths are supported.

IGNORE can be:
  - nil - No extra options.
  - :dir - Also ignore a trailing directory separator character.
  - :children - Like `:dir', and also ignore all of its children."
  (let* ((dir-sep '(or "/" "\\"))
         ;; Root of file system can be a few things, depending...
         (root (list 'or
                     (list 'and 'alphabetic ":") ;; Windows drive.
                     "~" ;; Home directory.
                     "/"))) ;; Unix root.
    (concat
     ;; First RX: the optional leading-up-to-`rx/path/partial' stuff.
     (eval `(rx string-start
                ;; Allow path to start at root, or with a directory separator.
                (optional (or ,root ,dir-sep))
                ;; Allow optional parent directories in path.
                (zero-or-more
                 (and (one-or-more printing)
                      ,dir-sep))))
     ;; Second RX: The Input.
     ;;   - Our path we're actually looking for.
     rx/path/partial
     ;; Third RX: The optional ending stuff.
     (eval `(rx
             ;; No path after our match - we want to match the end of the path.
             ;; Do, however, allow for optional trailing directory separator.
             ,(cond ((eq ignore :dir)
                     (list 'zero-or-one dir-sep))
                    ;; Path allowed after our match - ignore the dir and all its children.
                    ((eq ignore :children)
                     (list
                      ;; Allow path after the `rx/path/partial' dir.
                      'zero-or-more
                      (list 'and
                            dir-sep
                            '(one-or-more printing))
                      ;; Allow the optional trailing directory separator.
                      (list 'optional dir-sep)))
                    ;; Nothing is allowed after our match.
                    (t
                     ""))
             string-end)))))
;; (files:ignore/regex ".git" :children)
;; (files:ignore/regex "init.el")


(defun files:ignore/string (string/path/partial &optional ignore)
  "Create a regex for ignoring STRING/PATH/PARTIAL.

STRING/PATH/PARTIAL should NOT be a regex string. It can be either absolute or relative.
If relative, it can start with or without a path separator. Both Unix and
Windows type paths are supported.

IGNORE can be:
  - nil - No extra options.
  - :dir - Also ignore a trailing directory separator character.
  - :children - Like `:dir', and also ignore all of its children."
  (files:ignore/regex (regexp-quote string/path/partial) ignore))
;; (files:ignore/string ".git" :children)


(defun files:ignore? (path ignores)
  "Return non-nil if PATH should be ignored based on IGNORES (list of regex strings)."
  (declare (pure t) (side-effect-free t))

  (let ((ignores ignores) ;; Give ourself an `ignores' to modify.
        path/rx/ignore
        regex)
    (while (and (not path/rx/ignore)
                ignores)
      (setq regex (car ignores)
            ignores (cdr ignores))
       (when (string-match-p regex path)
        (setq path/rx/ignore t)))

    ;; Return whether to ignore or not.
    path/rx/ignore))
;; (files:ignore? "~/mnt/d/somewhere/.git" (list (files:ignore/string ".git" :children)))
;; Doesn't care about "." or "..":
;; (files:ignore? "~/mnt/d/somewhere/." (list (files:ignore/string ".git" :children)))
;; Not a regex, but still works:
;; (files:ignore? "d:/somewhere/modules/spy/buffer/init.el" (list "init.el"))
;; "Regex" version of above:
;; (files:ignore? "d:/somewhere/modules/spy/buffer/init.el" (list (files:ignore/string "init.el")))
;; (files:ignore? "~/mnt/d/somewhere/init.el" (list (files:ignore/string "init.el")))



;;------------------------------------------------------------------------------
;; Files in directory
;;------------------------------------------------------------------------------

(defun files:in-directory (dir &optional recursive types ignores)
  "Get all files in directory DIR (never returns \".\" or \"..\").

If RECURSIVE is non-nil, looks in DIR and all children directories.

If TYPES is nil, all file types are vaild for returning.
TYPES can also be a list with these valid members:
  - :dir - allow directories
  - :file - allow files
  - :link - symbolic links
Any file type matching a member of TYPES will be returned if not ignored.

If IGNORES is non-nil, it should be a list of regex strings to use. Any absolute
path matching any of the filters will not be included in return values."
  (let ((dir/root dir)
        dir/current
        dirs/to-process
        paths/return)
    ;; Add root dir to processing list and output (if desired) now.
    (push dir/root dirs/to-process)
    (when (or (null types)
              (memq :dir types))
      (push dir/root paths/return))

    ;; Get/process one dir at a time from our processing list.
    (while dirs/to-process
      (setq dir/current (pop dirs/to-process))

      (dolist (entry (directory-files-and-attributes dir/current 'full))
        (let ((path (car entry))
              (attrs (cdr entry)))
          ;; Force ignore "." and ".." to avoid infinite loop.
          (cond ((string-suffix-p "." path)
                 nil)

                ;; Ignore anything that matches one of our ignore regex.
                ((and ignores
                      (files:ignore? path ignores))
                 nil)

                ;; A directory?
                ((eq (file-attribute-type attrs) t) ;; t == directory
                 ;; Save if TYPES allows.
                 (when (or (not types)
                           (memq :dir types))
                   (push path paths/return))
                 ;; If RECURSIVE, add to list to process.
                 (when recursive
                   (push path dirs/to-process)))

                ;; A file?
                ((eq (file-attribute-type attrs) nil) ;; nil == file
                 ;; Save if TYPES allows.
                 (when (or (not types)
                           (memq :file types))
                   (push path paths/return)))

                ;; A symlink?
                ((stringp (file-attribute-type attrs)) ;; string == symlink
                 ;; Save if TYPES allows.
                 (when (or (not types)
                           (memq :link types))
                   (push path paths/return)))

                ;; Default case - ehm... dunno how you got here.
                (t
                 (error "files:in-directory: Unsupported file-attribute-type ('%S') for path: %s"
                        (file-attribute-type attrs)
                        path))))))

    ;; Return the collected paths.
    paths/return))
;; (files:in-directory "..")
;; (files:in-directory ".." t)
;; (files:in-directory ".." t '(:dir))
;; (files:in-directory ".." t '(:dir :file))
;; (files:in-directory ".." t nil (list (files:ignore/string "init.el")))


;;------------------------------------------------------------------------------
;; File Operations
;;------------------------------------------------------------------------------

(defun file:update-files (&rest files)
  "Ensure FILES are updated in `recentf', `magit' and `save-place'.

Proudly nicked from Doom's \"core/autoload/files.el\"."
  (let (toplevels)
    (dolist (file files)
      (when (featurep 'vc)
        (vc-file-clearprops file)
        (when-let (buffer (get-file-buffer file))
          (with-current-buffer buffer
            (vc-refresh-state))))

      (when (featurep 'magit)
        (when-let (default-directory (magit-toplevel (file-name-directory file)))
          (cl-pushnew default-directory toplevels)))

      (unless (file-readable-p file)
        (when (bound-and-true-p recentf-mode)
          (recentf-remove-if-non-kept file))

        (when (and (bound-and-true-p projectile-mode)
                   (path:project?)
                   (projectile-file-cached-p file (path:project:root)))
          (projectile-purge-file-from-cache file))))

    (dolist (default-directory toplevels)
      (magit-refresh))

    (when (bound-and-true-p save-place-mode)
      (save-place-forget-unreadable-files))))


;;;###autoload
(defun file:cmd:copy/this-buffer-file (new-path &optional force-p)
  "Copy current buffer's file to NEW-PATH.

If FORCE-P, overwrite the destination file if it exists, without confirmation.

Proudly nicked from Doom's \"core/autoload/files.el\"."
  (interactive
   (list (read-file-name "Copy file to: ")
         current-prefix-arg))

  (unless (and buffer-file-name (file-exists-p buffer-file-name))
    (user-error "Buffer is not visiting any file"))

  (let ((old-path (buffer-file-name (buffer-base-buffer)))
        (new-path (expand-file-name new-path)))
    (make-directory (file-name-directory new-path) 't)
    (copy-file old-path new-path (or force-p 1))
    (file:update-files old-path new-path)
    (message "File copied to %S" (abbreviate-file-name new-path))))


;;;###autoload
(defun file:cmd:delete (&optional path force-p)
  "Delete PATH, kill its buffers and expunge it from vc/magit cache.

If PATH is not specified, default to the current buffer's file.

If FORCE-P, delete without confirmation.

Proudly nicked from Doom's \"core/autoload/files.el\"."
  (interactive (list (buffer-file-name (buffer-base-buffer))
                     current-prefix-arg))

  (let* ((path (or path (buffer-file-name (buffer-base-buffer))))
         (short-path (abbreviate-file-name path)))
    ;;------------------------------
    ;; Error Checks
    ;;------------------------------
    (unless (and path (file-exists-p path))
      (user-error "Buffer is not visiting any file"))
    (unless (file-exists-p path)
      (error "File doesn't exist: %s" path))
    (unless (or force-p (y-or-n-p (format "Really delete %S?" short-path)))
      (user-error "Aborted"))

    ;;------------------------------
    ;; Delete File
    ;;------------------------------
    (let ((buf (current-buffer)))
      (unwind-protect
          ;; Delete file?
          (progn (delete-file path :trash) t)

        ;; `delete-file' above failed. Do we need to do any clean up?
        (if (file-exists-p path)
            ;; Still exists so just complain.
            (error "Failed to delete %S" short-path)
          ;; File doesn't exist, so... ¯\_(ツ)_/¯
          (buffer:cmd:kill buf :dont-save)
          (file:update-files path)
          (message "Deleted %S" short-path))))))


;; TODO: A `file:delete` function.

;; TODO: Should I have file ops for `path:` where it makes sense?
;; (defalias 'path:cmd:delete 'file:cmd:delete)


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :path 'files)

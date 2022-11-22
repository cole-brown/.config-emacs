;;; core/modules/emacs/path/buffer.el --- Buffer-Related Path Functions -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <code@brown.dev>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2020-12-04
;; Modified:   2022-11-22
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Buffer-Related Path Functions
;;
;;; Code:


(require 'cl-lib)
(require 'dired)

(imp:require :path 'path)
(imp:require :path 'files)


;;------------------------------------------------------------------------------
;; Copy Buffer File/Dir Name Functions
;;------------------------------------------------------------------------------

(cl-defun path:buffer:copy (&key parent? relative?)
  "Copy the buffer's current path to `kill-ring'.

If in a file buffer, copy the file's path.

If in Dired buffer, copy the file/dir cursor is on, or marked files.

If a buffer is not file and not Dired, copy value of `default-directory' (which
is usually the \"current\" dir when that buffer was created).

Optional keys:
  - `:parent?' (PARENT?)
    - If non-nil, copy the buffer's parent directory path instead of the
      buffer's own path.
  - `:relative?' (RELATIVE?)
    - If non-nil, copy the buffer's path relative to the project root instead of
      the absolute path.

Return a string: either a path name or a newline separated list of path names.

Originally `xah-file-path' from
http://xahlee.info/emacs/emacs/emacs_copy_file_path.html"
  ;;------------------------------
  ;; Find Path(s)
  ;;------------------------------
  (let ((paths
         ;; In dired mode? Check for what exactly to return.
         (cond ((string-equal major-mode 'dired-mode)
                (let ((result (dired-get-marked-files)))
                  (if (equal (length result) 0)
                      default-directory
                    result)))

               ;; Not a Dired buffer: Return the buffer's file name, if it has one.
               ((buffer-file-name))

               ;; Fallback: `default-directory'
               (t
                (expand-file-name default-directory))))
        ;; Need to normalize `paths' first.
        root)

    ;; Normalize to a list if we only got one path.
    (unless (listp paths)
      (setq paths (list paths)))

    ;; Now we can figure out project root.
    (when relative?
      (setq root (path:project:root (nth 0 paths)))
      (unless root
        (error "path:buffer:copy: No project root for path! Cannot determine relative path(s) for: %s" paths)))

    ;;------------------------------
    ;; What Part of Path(s) to Copy?
    ;;------------------------------
    (when relative?
      ;; Swap `paths' out for relative paths.
      (let (paths/rel)
        (dolist (path paths)
          (push (path:canonicalize:relative path
                                            (path:project:root path))
                paths/rel))
        (setq paths paths/rel)))

    (when parent?
      ;; Swap `paths' out for their parents.
      (let (paths/parent)
        (dolist (path paths)
          (push (path:parent path)
                paths/parent))
        (setq paths paths/parent)))

    ;;------------------------------
    ;; Copy Path(s)
    ;;------------------------------
    (let ((paths/len (length paths)))
      (if (= paths/len 1)
          (message "%s%sPath Copied: 「%s」"
                   (if relative? "Relative " "")
                   (if parent?   "Parent "   "")
                   (nth 0 paths))
        (message "%d %s%sPaths Copied: 「%s, ...」"
                 (format "%d " paths/len)
                 (if relative? "Relative " "")
                 (if parent?   "Parent "   "")
                 (nth 0 paths)))

      (kill-new (mapconcat 'identity paths "\n")))))


(defun path:cmd:buffer:copy:absolute (&optional parent?)
  "Copy the buffer's current path to `kill-ring'.

If in a file buffer, copy the file's path.

If in Dired buffer, copy the file/dir cursor is on, or marked files.

If a buffer is not file and not Dired, copy value of `default-directory' (which
is usually the \"current\" dir when that buffer was created).

If PARENT? is non-nil (`universal-argument' is called first), copy the buffer's
parent directory path instead of the buffer's own path.

Return a path string."
  (interactive "P")
  (path:buffer:copy :parent? parent?))


(defun path:cmd:buffer:copy:relative (&optional parent?)
  "Copy the buffer's current path to `kill-ring'.

If in a file buffer, copy the file's path.

If in Dired buffer, copy the file/dir cursor is on, or marked files.

If a buffer is not file and not Dired, copy value of `default-directory' (which
is usually the \"current\" dir when that buffer was created).

If PARENT? is non-nil (`universal-argument' is called first), copy the buffer's
parent directory path instead of the buffer's own path.

Return a path string."
  (interactive "P")
  (path:buffer:copy :parent?   parent?
                    :relative? t))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :path 'buffer)

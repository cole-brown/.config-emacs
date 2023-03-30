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
(require 'project)

(imp:require :path 'path)
(imp:require :path 'files)


;;--------------------------------------------------------------------------------
;; Buffer Path
;;--------------------------------------------------------------------------------

(defun path:buffer (&optional buffer)
  "Get buffer's file path, if any.

BUFFER should be a buffer object, a buffer name string, or nil for
`current-buffer'.

Return nil if no backing file.

Some buffers have trouble knowing where they came from.

Example 1: An indirect buffer will be called \"foo.el<2>\", be visiting
\"foo.el\", but `buffer-file-truename` will be nil, `buffer-file-name` will
return nil... I mean... WTF?

Example 2: `dired' buffers... Sigh."
  (let ((buffer (if buffer
                    (get-buffer buffer)
                  (current-buffer))))
    (with-current-buffer buffer
      (cond (buffer-file-truename) ;; Is it already all figured out for us?

            ;; `dired' buffer? Use it's dir because... why does it not have a buffer file name?
            ;; TODO: Why would the original version use `string-equal' instead of `eq'?!
            ;;  > (string-equal major-mode 'dired-mode)
            ((eq major-mode 'dired-mode)
             default-directory)

            ;; Not a Dired buffer: Return the buffer's file name, if it has one.
            ((buffer-file-name (or (buffer-base-buffer buffer) ; indirect buffer?
                                   buffer)))

            ;; No wild guessing allowed.
            (t
             nil)))))
;; (path:buffer)


;;--------------------------------------------------------------------------------
;; Buffer Path Relative to Project
;;--------------------------------------------------------------------------------

(defun path:buffer:project (&optional buffer)
  "Get BUFFER's filepath as a relative path starting at project root directory.

BUFFER should be a buffer object, a buffer name string, or nil for
`current-buffer'.

If in a project, return relative path starting with project's root directory.
Example:
  (path:buffer:project \"~/.config/project-root/path/to/file.txt\")
    -> \"project-root/path/to/file.txt\"

If not in a project, return nil.
Example:
  (path:buffer:project \"~/.config/no-root/path/to/file.txt\")
    -> nil

If not a file-backed buffer, return nil."
  (when-let* ((path/buffer      (path:buffer buffer)) ;; Does buffer have a file and what is its actual name?
              (path/project     (cdr-safe (project-current)))
              (path/relative-to (path:parent (path:canonicalize:dir path/project))))
    ;; Ok; have the pieces. What's the relative path now?
    (path:canonicalize:relative path/file path/relative-to)))
;; (path:buffer:project)


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

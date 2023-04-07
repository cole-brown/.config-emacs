;;; git.el --- Version Control & Paths -*- lexical-binding: t; -*-
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
;;  Version Control & Paths
;;
;;; Code:


(imp:require :path 'path)


;; TODO: how to autoload / lazy require magit for these functions?


;;------------------------------------------------------------------------------
;; Require `magit'
;;------------------------------------------------------------------------------

(defun int<path>:vc/git:magit ()
  "Ensure `magit' is loaded.

TODO: Is there a better way to do this? For, like... Only having this part of
the path lib require `magit'."
  (unless (featurep 'magit)
    (require 'magit)))


;;------------------------------------------------------------------------------
;; Branch Name
;;------------------------------------------------------------------------------

(defun path:vc/git:branch (path &optional require-magit?)
  "Get name of current branch, or else current head.

PATH should be an absolute path string.

If REQUIRE-MAGIT? is nil, do not load `magit' if not found; just return nil.

Returned string will be either:
  - Name of branch.
  - First seven of hash."
  ;; Should we require `magit'?
  (unless (featurep 'magit)
    (when require-magit?
      (require 'magit)))

  ;; Can we even try to figure out a branch name?
  (when (featurep 'magit)
    ;; TODO: Does this work in a brand new git repo?
    (let* (;; (path (downcase path)) ; Don't downcase blindly... some file systems are case-sensitive.
           (default-directory (path:parent path))
           (headish (magit-headish))
           (branch (magit-name-branch headish)))
      (or branch headish))))
;; (path:vc/git:branch (path:buffer))
;; (path:vc/git:branch (path:buffer) t)


;;------------------------------------------------------------------------------
;; Git: Root Directory
;;------------------------------------------------------------------------------

;;;###autoload
(defun path:vc/git:root/path (&optional path)
  "Return canonicalized path to the Git repository that PATH is in.

If PATH is not in a git repo, return nil.

If PATH is not supplied, use `default-directory'."
  (int<path>:vc/git:magit)
  (path:canonicalize:file (magit-toplevel path)))
;; (path:vc/git:root/path)


;;;###autoload
(defun path:vc/git:root/name (&optional path)
  "Return directory name of the Git repository that PATH is in.

If PATH is not in a git repo, return nil.

If PATH is not supplied, use `default-directory'."
  (int<path>:vc/git:magit)
  (when-let* ((git:root/abs (path:vc/git:root/path path))
              (git:root/dir (file:name git:root/abs)))
    git:root/dir))
;; (path:vc/git:root/name)


;;------------------------------------------------------------------------------
;; Git: Relative Paths
;;------------------------------------------------------------------------------

;;;###autoload
(defun path:vc/git:relative (&optional path pretty?)
  "Return PATH relative to the root of its Git repo.

If PATH is not in a git repo, return nil.

If PATH is in a git repo, return:
  - If PRETTY? is non-nil:
    \"｢git:<repo-dir-name>｣/relative/path/to/file.el\"
  - If PRETTY? is nil:
    \"relative/path/to/file.el\""
  (int<path>:vc/git:magit)
  (when-let* ((git:root/abs (path:vc/git:root/path path))
              ;; We are in a git repository, so get the relative path.
              (git:path/rel (path:canonicalize:relative path git:root/abs)))
    ;; Return the fancy/pretty version, or just the relative path string?
    (if pretty?
        (concat "｢git:"
                (path:vc/git:root/name path)
                "｣/"
                git:path/rel)
      git:path/rel)))
;; (path:vc/git:relative (path:current:file))
;; (path:vc/git:relative (path:current:file) :pretty)


;;--------------------------------------------------------------------------------
;; Project
;;--------------------------------------------------------------------------------

(defun path:project:current/alist (path)
  "Get an alist about PATH's filepath relative to the current project root.

PATH should be an absolute path string.

If in a project, return an alist:
  '((:project/type . symbol)                 ; e.g. `projectile'
    (:project/name . \"root-dir-name\")      ; e.g. \".emacs.d\"
    (:path         . \"path/to/buffer.el\")) ; relative to `:project/name' root

If PATH is not in the `current-project' path, return nil."
  (when-let* ((path/project (cdr-safe (project-current)))
              (path/relative (path:canonicalize:relative path path/project))
              ;; If we don't end up with a relative path, we're not in the
              ;; current project and we have no idea what's going on, so...
              ;; return nil.
              (path-is-relative? (path:relative? path/relative)))
    (list (cons :project/type (car-safe (project-current)))
          (cons :project/name (dir:name path/project))
          (cons :path         path/relative))))
;; (path:project:current/alist (path:buffer))
;; (path:project:current/alist "/tmp")


(defun path:project:uniquify (base extra-strings)
  "`uniquify-buffer-name-style' function for unique-by-project-path buffer names.

BASE should be a string.
EXTRA-STRINGS should be a string.

Return a string that `uniquify' should use to name the buffer."
  ;; Detect `dired', etc paths and do not append the base name?
  (if-let* ((path (cond
                   ;; If we have no BASE, um... assume it's just a dir maybe? IDK.
                   ;; When does this come up? It's a check in the `uniquify' source code...
                   ;; I think it's when it's a directory? See help for `uniquify-get-proposed-name'.
                   ((or (not (stringp base))
                        (string-empty-p base))
                    default-directory)

                   ;; `uniquify' has been set up to notify us about dirs? Perfect!
                   ((and uniquify-trailing-separator-p
                         (path:directory? base))
                    ;; Have a directory... just use `default-directory'.
                    default-directory)

                   ((and uniquify-trailing-separator-p
                         (not (path:directory? base)))
                    (path:join default-directory base))

                   ;; ???
                   (t
                    "/TODO/this/path/???")))
            (project      (path:project:current/alist path))
            (project/root (alist-get :project/name project))
            (project/path (alist-get :path         project)))
      (let ((project/branch (path:vc/git:branch path)))
        ;; Return the fancy/pretty version, or just the relative path string?
        (concat (propertize project/root 'face 'underline)
                ;; (if project/branch
                ;;     (concat "⎇(" project/branch ")")
                ;;   nil)
                ;; Git uses ":/" as "the root of this git repo.
                ":/"
                project/path))

    ;; No simple fallback for using `uniquify' itself... So just do something
    ;; dumb and simple?
    (concat (mapconcat #'identity extra-strings "/") "/" base)))
;; (path:project:uniquify "base.txt" '("path" "to"))


;; (defun path:buffer:project (&optional buffer pretty?)
;;   "Return BUFFER's filepath relative to the project root.

;; If not in a project, return nil.
;; If not a file-backed buffer, return nil.

;; If in a project, return:
;;   - If PRETTY? is non-nil:
;;     \"｢<project-name>｣:/relative/path/to/file.el\"
;;   - If PRETTY? is nil:
;;     \"<project-name>/relative/path/to/file.el\""
;;   (when-let* ((alist (path:buffer:project/alist buffer))
;;               (root (alist-get :project/name alist))
;;               (path (alist-get :path alist)))
;;     ;; Return the fancy/pretty version, or just the relative path string?
;;     (concat (if pretty? "｢" "")
;;             root
;;             ;; Git uses ":/" as "the root of this git repo.
;;             (if pretty? "｣:" "")
;;             "/"
;;             path)))
;; ;; (path:buffer:project)
;; ;; (path:buffer:project nil :pretty)
;; ;; "｢emacs-sn004｣:/core/modules/emacs/path/buffer.el"
;; ;; emacs-sn004:/core/modules/emacs/path/buffer.el"


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :path 'git)

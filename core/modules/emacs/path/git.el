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


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :path 'git)

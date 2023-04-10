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


;;--------------------------------------------------------------------------------
;; Pretty Repo Paths
;;--------------------------------------------------------------------------------

(defcustom path:vc/git:rooted ":/"
  "String to use to indicate a path is relative to the project/repository root.

Default is \":/\". Why \":/\"? It's 'git pathspec' speak for \"the root of the
working tree\" aka the repository's root.

See Git's Pathspec:
https://git-scm.com/docs/gitglossary#Documentation/gitglossary.txt-aiddefpathspecapathspec"
  :group 'path:group
  :type '(string))


(defun path:vc/git:rooted (project-name &rest relative-path)
  "Return a path rooted at the PROJECT-NAME/repository.

PROJECT-NAME should be a string of the project's name aka the name of the
directory that has the '.git' folder.

RELATIVE-PATH should be string(s) of the path relative to the PROJECT directory.

Return a string of PROJECT-NAME + \":/\" + RELATIVE-PATH

Why \":/\"? It's 'git pathspec' speak for \"the root of the working tree\" aka
the repository's root.

See Git's Pathspec:
https://git-scm.com/docs/gitglossary#Documentation/gitglossary.txt-aiddefpathspecapathspec"
  (concat project-name
          path:vc/git:rooted
          (apply #'path:join relative-path)))
;; (path:vc/git:rooted "foo" "path/to/bar")


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


;;------------------------------------------------------------------------------
;; Uniquify
;;------------------------------------------------------------------------------

(defun path:project:uniquify (base extra-strings)
  "`uniquify-buffer-name-style' function for unique-by-project-path buffer names.

BASE should be a string.
EXTRA-STRINGS should be a string.

Return a string that `uniquify' should use to name the buffer.

NOTE: Cannot use a lot of very useful functions here, as the buffer is likely
nascent and doesn't have things like variable `buffer-file-name' or `major-mode'
defined yet."
  (cond
   ;;------------------------------
   ;; Special Modes
   ;;------------------------------
   ;; NOTE: Cannot use e.g. `major-mode' as it's not been set yet... :|

   ;; Magit already does a lot to uniquify its buffer names.
   ((string-match-p (rx-to-string '(sequence string-start
                                             "magit"
                                             (optional "-"
                                                       (one-or-more alphanumeric))
                                             ": "
                                             (one-or-more printing)
                                             string-end)
                                  :no-group)
                    base)
    ;; Just use the base name.
    base)

   ;;------------------------------
   ;; File-Visiting Buffers
   ;;------------------------------
   ;; Do the full buffer naming shenanigans... if possible, else skip down to fallback.
   ((when-let* ((path (cond
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

                       ;; Not our problem; goto to the `cond' fallback.
                       (t
                        nil)))
                (project      (path:project:current/alist path))
                (project/root (alist-get :project/name project))
                (project/path (alist-get :path         project)))
      ;; Return the fancy/pretty version, or just the relative path string?
      (concat (propertize project/root 'face 'underline)
              path:vc/git:rooted
              project/path)))

   ;;------------------------------
   ;; Default/Fallback
   ;;------------------------------
   (t
    ;;---
    ;; Default
    ;;---
    ;; No simple fallback for using `uniquify' itself... So just do something
    ;; dumb and simple?
    (concat (mapconcat #'identity extra-strings "/") "/" base)

    ;;---
    ;; Alternatives:
    ;;---
    ;; Do something similar to `uniquify-buffer-name-style' `post-forward'?
    ;;     (concat base ":" (mapconcat #'identity extra-strings "/"))

    ;; Just use the base? IDK?
    ;; base
    )))
;; (path:project:uniquify "base.txt" '("path" "to"))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :path 'git)

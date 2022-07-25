;;; tools/autogit/api.el -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2022-02-14
;; Modified:   2022-07-25
;; URL:        https://github.com/cole-brown/.config-emacs
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Commands for:
;;   - Getting general status for certain other git repos.
;;     + E.g. get status for your notes repo(s) and your code repo(s) in one go.
;;
;;; Code:


(imp:require :path)


;;------------------------------------------------------------------------------
;; Repositories
;;------------------------------------------------------------------------------

(defun autogit:repo? (path)
  "Return non-nil if PATH is a git repository."
  (and (path:exists? path :dir)
       (path:exists? (path:join path ".git"))))


(defun autogit:repos:list (root)
  "Get all repo directories that are (direct) children of ROOT path.

Returns list of absolute paths or nil."
  ;;------------------------------
  ;; Error Checking
  ;;------------------------------
  (unless (stringp root)
    (error "autogit:repos:list: ROOT must be a string! Got: %S"
           root))

  (let ((path:root (path:canonicalize:dir root)))
    (unless (path:exists? path:root)
      (error "autogit:repos:list: ROOT does not exist! %s"
             path:root))
    (unless (path:exists? path:root :dir)
      (error "autogit:repos:list: ROOT is not a directory! %s"
             path:root))

    ;;------------------------------
    ;; Find Git Repos.
    ;;------------------------------
    ;; Is the root itself a repo?
    (if (autogit:repo? path:root)
        (list path:root)
      ;; Nope; check its direct children.
      (let (repos)
        (dolist (path:dir (path:children path:root :absolute :dir))
          (when (autogit:repo? path:dir)
            (push path:dir repos)))
        ;; Return absolute path to any repos found.
        repos))))
;; (autogit:repos:list "~/.config")


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :autogit 'api)

;;; autogit-api.el --- Autogit API Functions -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2022-02-14
;; Timestamp:  2023-06-29
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Autogit API (non-interactive but interesting to end users) Functions
;;
;;; Code:


(require 'autogit-path)


;;------------------------------------------------------------------------------
;; Repositories
;;------------------------------------------------------------------------------

;;;###autoload
(defun autogit:repo? (path)
  "Return non-nil if PATH is a git repository."
  (and (int<autogit>:path:exists? path :dir)
       (int<autogit>:path:exists? (int<autogit>:path:join path ".git"))))


;;;###autoload
(defun autogit:repos:list (root)
  "Get all repo directories that are (direct) children of ROOT path.

Returns list of absolute paths or nil."
  ;;------------------------------
  ;; Error Checking
  ;;------------------------------
  (unless (stringp root)
    (error "autogit:repos:list: ROOT must be a string! Got: %S"
           root))

  (let ((path:root (int<autogit>:path:canonicalize:dir root)))
    (unless (int<autogit>:path:exists? path:root)
      (error "autogit:repos:list: ROOT does not exist! %s"
             path:root))
    (unless (int<autogit>:path:exists? path:root :dir)
      (error "autogit:repos:list: ROOT is not a directory! %s"
             path:root))

    ;;------------------------------
    ;; Find Git Repos.
    ;;------------------------------
    ;; Is the root itself a repo?
    (if (autogit:repo? path:root)
        (list int<autogit>:path:root)
      ;; Nope; check its direct children.
      (let (repos)
        (dolist (path:dir (int<autogit>:path:children path:root :absolute :dir))
          (when (autogit:repo? path:dir)
            (push path:dir repos)))
        ;; Return absolute path to any repos found.
        repos))))
;; (autogit:repos:list "~/.config")


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'autogit-api)
;;; autogit-api.el ends here

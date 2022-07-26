;;; tools/autogit/path.el --- Path Functions -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2020-08-28
;; Modified:   2022-07-26
;; URL:        https://github.com/cole-brown/.config-emacs
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Path Functions
;;
;;; Code:


;; TODO: How to get this lazy loaded?
;;   - Take the require out of the funcs when figured out.
;; (require 'magit)

(imp:require :path)


;;------------------------------------------------------------------------------
;; Paths & Files
;;------------------------------------------------------------------------------

(defun int<autogit>:path:join (root &rest path)
  "Return absolute (file) path to ROOT + PATH.

Given a git ROOT, and a PATH of e.g. ('path/to' 'dir' 'with-file' 'file.txt'),
will return full /file/ path in platform-agnostic manner."
  (if (imp:feature? :path)
      (apply #'path:absolute:file root path)
    (concat (file-name-as-directory (expand-file-name "" root))
            (directory-file-name (mapconcat #'file-name-as-directory path "")))))
;; (int<autogit>:path:join (car autogit:repos:path/commit) "foo")


(defun int<autogit>:path:changes:rel->abs (path-abs alist/changes)
  "Convert relative paths to absolute.

Converts all of ALIST/CHANGES' relative paths to absolute paths given
a PATH-ABS inside of the git repository."
  (let* ((default-directory path-abs)
         (git-root (magit-toplevel))
         results)
    ;; Have alist of staged, unstaged, etc files.
    (dolist (entry alist/changes results)
      ;; Convert to absolute paths.
      (push (cons (car entry)
                  (mapcar (lambda (x) (int<autogit>:path:join git-root x))
                          (cdr entry)))
            results))))
;; (int<autogit>:path:changes:rel->abs default-directory (int<autogit>:changes:in-repo default-directory))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :autogit 'path)

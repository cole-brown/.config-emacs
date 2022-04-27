;;; package.el --- Now we're the Postal Service -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2022-04-26
;; Modified:   2022-04-26
;; URL:        https://github.com/cole-brown/.config-emacs
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;  Now we're the Postal Service
;;
;;; Code:


(imp:require :innit 'debug)
(require 'package)


;;------------------------------------------------------------------------------
;; Package Archives
;;------------------------------------------------------------------------------

;; Save common package archives so we don't have to go scrounging them up again.
(defvar innit:package:archives:common
  '(;; Milkypostman's Emacs Lisp Package Archive
    (:melpa . ("melpa" . "https://melpa.org/packages/"))
    ;; For more & newer Org-Mode packages.
    (:org . ("org"   . "https://orgmode.org/elpa/"))

    ;;---
    ;; The Default
    ;;---
    ;; Emacs Lisp Package Archive
    (:default . ("gnu"   . "https://elpa.gnu.org/packages/")))
  "Alist of Package Archive keyword to cons cells  for `package-archives' alist.")


(defcustom innit:package:archives:enabled
  '(:melpa :org :default)
  "Ordered list of package archives to enable in `innit:package:init'."
  :group 'innit:group
  :type  '(restricted-sexp :match-alternatives (keywordp)))


;;------------------------------------------------------------------------------
;; Package Path
;;------------------------------------------------------------------------------
;; Modify `package' path: want a top-level dir that can hold the "elpa"
;; `package' dir, any local packages, `straight.el' repos, etc...

(defcustom innit:path:packages (path:join user-emacs-directory "packages")
  "Top-level directory for packages. ELPA, others will have subdirectories."
  :group 'innit:group
  :type  'string)


(defcustom innit:path:packages:elpa (path:join innit:path:packages "elpa")
  "Directory for ELPA/packages.el packages."
  :group 'innit:group
  :type  'string)


;; Can add more for e.g. straight.el, git subtrees, git submodules... whatever.
;; (defcustom innit:path:packages:manual (path:join innit:path:packages "manual")
;;   "Directory for ELPA/packages.el packages."
;;   :group 'innit:group
;;   :type 'string)


;;------------------------------------------------------------------------------
;; Package Early-Init
;;------------------------------------------------------------------------------

(defun innit:package:archive (keyword-or-cons)
  "Return a package name/URL cons.

If KEYWORD-OR-CONS is a keyword, returns value from
`innit:package:archives:common'.

If KEYWORD-OR-CONS is a string/string cons, returns it as-is.

Else error."
  (cond ((keywordp keyword-or-cons)
         (alist-get keyword-or-cons innit:package:archives:common))
        ((and (consp keyword-or-cons)
              (stringp (car keyword-or-cons))
              (stringp (cdr keyword-or-cons)))
         keyword-or-cons)
        (t
         (error "innit:package:archive: Expected keyword or cons, got: %S"
                keyword-or-cons))))


(defun innit:package:init ()
  "Set-up Emacs 'package.el'.

`package-initialize' is called between \"early-init.el\" and \"init.el\", so
this needs to be called by the end of \"early-init.el\"."
  ;;---
  ;; Set enabled archives.
  ;;---
  (when innit:package:archives:enabled
    (customize-set-variable 'package-archives
                            (mapcar #'innit:package:archive
                                    innit:package:archives:enabled)))

  ;;---
  ;; Set path packages are saved to.
  ;;---
  (when innit:path:packages:elpa
    (customize-set-variable 'package-user-dir innit:path:packages:elpa))
  )


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :innit 'package)

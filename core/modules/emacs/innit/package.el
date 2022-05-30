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

;; TODO: Here or move to a 'no-littering' file?
(defcustom innit:path:var (path:join user-emacs-directory "var")
  "Directory for persistent data files."
  :group 'innit:group
  :type  'string)


;; TODO: Here or move to a 'no-littering' file?
(defcustom innit:path:etc (path:join user-emacs-directory "config")
  "Directory for persistent config files."
  :group 'innit:group
  :type  'string)


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


(defcustom innit:path:packages:gpg (path:join innit:path:var "packages" "gpg")
  "Directory for gpg keys for e.g. `package-gnupghome-dir'."
  :group 'innit:group
  :type  'string)


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


(defun innit:package:init/paths ()
  "Set 'package.el' paths.

Some (`package-user-dir') get overwritten when 'package.el' loads, so this must
be called after 'package.el' loads.

Can be called earlier too, if you want..."
  ;;---
  ;; Set path packages are saved to.
  ;;---
  (when innit:path:packages:elpa
    ;; `package-user-dir' doesn't get set via `customize-set-variable'?!
    ;;   (customize-set-variable 'package-user-dir innit:path:packages:elpa)
    ;; So use `setq' instead:
    (setq package-user-dir innit:path:packages:elpa))
  ;; NOTE: Also, this gets wiped out when 'package.el' is loaded? So... Set it
  ;; as much as needed to force Emacs to behave?

  (when innit:path:packages:gpg
    (customize-set-variable 'package-gnupghome-dir innit:path:packages:gpg)))


(defun innit:package:init/early ()
  "Prepare Emacs 'package.el' for the `package-initialize' step.

`package-initialize' is called between \"early-init.el\" and \"init.el\", so
this needs to be called during \"early-init.el\"."
  (nub:debug
      :innit
      "innit:package:init/early"
      '(:innit :package :early)
    "'package.el' early init...")

  ;;---
  ;; Set enabled package archives.
  ;;---
  (when innit:package:archives:enabled
    (customize-set-variable 'package-archives
                            (mapcar #'innit:package:archive
                                    innit:package:archives:enabled)))

  ;;---
  ;; Set path packages are saved to.
  ;;---
  (innit:package:init/paths))


(defun innit:package:init/standard ()
  "Initialize Emacs 'package.el' after the `package-initialize' step.

1. Download package metadata if needed.
2. Install `use-package' if needed.
3. Require `use-package'.

`package-initialize' is called between \"early-init.el\" and \"init.el\", so
this needs to be called during \"init.el\"."
  (let ((func/name "innit:package:init/standard")
        (func/tags '(:innit :package :standard)))
    (nub:debug
        :innit
        func/name
        func/tags
      "'package.el' standard init...")

    ;;------------------------------
    ;; `package.el'
    ;;------------------------------
    ;; Set 'package.el' paths again as 'package.el' overwrites them when it loads...
    (innit:package:init/paths)

    ;; Update packages list if we are on a new install.
    (unless (or (package-installed-p 'use-package)
                package-archive-contents)
      (nub:debug
          :innit
          func/name
          func/tags
        "Update packages list...")
      (package-refresh-contents))

    ;;------------------------------
    ;; `use-package'
    ;;------------------------------
    ;;---
    ;; Install:
    ;;---
    (unless (package-installed-p 'use-package)
      (nub:debug
          :innit
          func/name
          func/tags
        "Install `use-package'...")
      (package-install 'use-package))

    (require 'use-package)

    ;;---
    ;; Global Settings:
    ;;---
    ;; Automatically install package if not found.
    ;;   https://github.com/jwiegley/use-package#package-installation
    ;; NOTE: Does not keep anything up-to-date. For that you would use package
    ;; `auto-package-update' or something similar.
    (customize-set-variable 'use-package-always-ensure t)

    ;;---
    ;; Debugging Settings:
    ;;---
    (setq use-package-compute-statistics    innit:debug?
          use-package-verbose               innit:debug?
          use-package-minimum-reported-time (if innit:debug? 0 0.1)
          use-package-expand-minimally      innit:interactive?)))



;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :innit 'package)

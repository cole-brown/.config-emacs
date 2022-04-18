;;; core/boot/00-early/80-package.el -*- lexical-binding: t; -*-


;;------------------------------------------------------------------------------
;; Package Set-Up
;;------------------------------------------------------------------------------
;; `package-initialize` is called between "early-init.el" and "init.el", so we
;; need to be all set-up and ready by the end of "early-init.el".


(require 'package)

;; Don't set this to `nil' - we want Emacs to call `package-initialize' for us
;; between "early-init.el" and "init.el".
;; (customize-set-variable 'package-enable-at-startup nil)


;;------------------------------
;; Archives
;;------------------------------
;; Add "melpa" and "org" to the available package archives.
(customize-set-variable 'package-archives
                        '(("melpa" . "https://melpa.org/packages/")
                          ("org"   . "https://orgmode.org/elpa/")
                          ("gnu"   . "https://elpa.gnu.org/packages/")))


;;------------------------------
;; Package Path
;;------------------------------
;; Modify `package' path: want a top-level dir that can hold the "elpa"
;; `package' dir, any local packages, `straight.el' repos, etc...

(defconst innit:path:packages (path:join user-emacs-directory "packages")
  "Top-level directory for packages. ELPA, others will have subdirectories.")


(defcustom innit:path:packages:elpa (path:join innit:path:packages "elpa")
  "Directory for ELPA/packages.el packages."
  :group 'innit:group
  :type 'string)


;; Can add more for e.g. straight.el, git subtrees, git submodules... whatever.
;; (defcustom innit:path:packages:manual (path:join innit:path:packages "manual")
;;   "Directory for ELPA/packages.el packages."
;;   :group 'innit:group
;;   :type 'string)


;;---
;; Set ELPA path.
;;---
(customize-set-variable 'package-user-dir innit:path:packages:elpa)

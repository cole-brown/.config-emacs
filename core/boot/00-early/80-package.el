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
;; (customize-set-variable 'package-user-dir (path:join user-emacs-directory
;;                                                      "packages"
;;                                                      "elpa"))


;;; 00-bootstrap.el --- Super early stuff requried for "all" of init. -*- lexical-binding: t; -*-
;;
;; Author: Cole Brown <code@brown.dev>
;; URL:    https://github.com/cole-brown/.config-emacs
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;; Code:


;;------------------------------------------------------------------------------
;; Required Packages & Modules
;;------------------------------------------------------------------------------
;; Load a few "must exist ASAP" things that we'll use for the rest of Emacs
;; start-up.


;;------------------------------
;; `use-package'
;;------------------------------
;; https://github.com/jwiegley/use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;;---
;; Use-Package Global Settings:
;;---

;; Don't force ensure - let packages lazily auto-load as they think they're needed.
;; Can always override on a per-package basis.
;; (customize-set-variable 'use-package-always-ensure t)

;;---
;; Use-Package & Debugging
;;---

(setq use-package-compute-statistics    innit:debug?
      use-package-verbose               innit:debug?
      use-package-minimum-reported-time (if innit:debug? 0 0.1)
      use-package-expand-minimally      innit:interactive?)


;;------------------------------
;; `imp'
;;------------------------------
(load (path:join innit:path:core/modules "imp/init.el"))

;; From here on, `imp' should be used instead of `load', `require', etc.
;; And `use-package' should be used for any packages.


;;------------------------------
;; `no-littering'
;;------------------------------
(use-package no-littering
  ;;--------------------
  :custom
  ;;--------------------
  ;; Stick with default "etc"/"var" directory names.
  ;; (no-littering-etc-directory (expand-file-name "config/" user-emacs-directory))
  ;; (no-littering-var-directory (expand-file-name "data/"   user-emacs-directory))


  ;;--------------------
  :config
  ;;--------------------
  ;; Suggested settings: https://github.com/emacscollective/no-littering#suggested-settings

  ;; `recentf' should ignore the files in the `no-littering' dirs.
  (add-to-list 'recentf-exclude no-littering-etc-directory)
  (add-to-list 'recentf-exclude no-littering-var-directory)

  ;; Auto-saves should go in the `no-littering' directory.
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

  ;; We don't want a "custom.el" at all, but if we did, it should be in the
  ;; `no-littering' etc dir.
  ;; (setq custom-file (no-littering-expand-etc-file-name "custom.el"))

  ;; Native Compliation (Emacs 28+):
  (when (fboundp 'startup-redirect-eln-cache)
    (startup-redirect-eln-cache
     (convert-standard-filename
      (no-littering-expand-var-file-name "eln-cache/")))))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :dot-emacs 'core 'boot '10-init 'bootstrap)

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
;; `package.el' & `use-package'
;;------------------------------
;; https://github.com/jwiegley/use-package

;; Finish `package.el' init; install & require `use-package'.
(innit:package:init/normal)


;;------------------------------
;; `no-littering'
;;------------------------------
(use-package no-littering
  ;;--------------------
  :custom
  ;;--------------------
  ;; Set the `no-littering' directory paths.
  (no-littering-etc-directory innit:path:etc)
  (no-littering-var-directory innit:path:var)


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

  ;; TODO: An innit function to set this or not.
  ;; TODO: Or just `(when some-variable ...)'
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

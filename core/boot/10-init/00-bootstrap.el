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
;; Load Paths
;;------------------------------------------------------------------------------

;; TODO: Do I want to add anything to the load-path directly, or just use `imp'
;; et al?
;; (add-to-list 'load-path (path:join user-emacs-directory "..."))


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
;; Keep the `user-emacs-directory' clean by changing where Emacs & packages
;; store their data. Move it from various & sundry places in and under
;; `user-emacs-directory' to be in one of two `user-emacs-directory'
;; sub-directories:
;;   - `no-littering-etc-directory'
;;   - `no-littering-var-directory'
(imp:use-package no-littering
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
  (imp:eval:after recentf
    (add-to-list 'recentf-exclude no-littering-etc-directory)
    (add-to-list 'recentf-exclude no-littering-var-directory))

  ;; Auto-saves should go in the `no-littering' directory.
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

  ;; TODO: An innit function to set this or not.
  ;; TODO: Or just `(when some-variable ...)'
  ;; TODO:   - One for the settings.el file to adjust?
  ;; We don't want a "custom.el" at all, but if we did, it should be in the
  ;; `no-littering' etc dir.
  ;; (setq custom-file (no-littering-expand-etc-file-name "custom.el"))

  ;; Native Compliation (Emacs 28+):
  (when (fboundp 'startup-redirect-eln-cache)
    (startup-redirect-eln-cache
     (convert-standard-filename
      (no-littering-expand-var-file-name "eln-cache/")))))


;;------------------------------
;; Garbage Collector Magic Hacks (`gcmh')
;;------------------------------
;; https://github.com/emacsmirror/gcmh


;; NOTE: WARNING!
;;   If you do not want `gcmh', be sure to set this back to something reasonable!
;;   It was set to `most-positive-fixnum' during early-init in
;;   "core/boot/00-early/00-bootstrap.el"!
;; (setq gc-cons-threshold 800000) ;; Emacs' default value


;; The GC introduces annoying pauses and stuttering into our Emacs experience,
;; so we use `gcmh' to stave off the GC while we're using Emacs, and provoke it
;; when it's idle. However, if the idle delay is too long, we run the risk of
;; runaway memory usage in busy sessions. If it's too low, then we may as well
;; not be using `gcmh' at all.
(imp:use-package gcmh

  ;;--------------------
  :custom
  ;;--------------------
  (gcmh-idle-delay 'auto)  ; default is 15s
  (gcmh-auto-idle-delay-factor 10)
  ;; GCMH default:  1 MB
  ;; Doom default: 16 MB
  ;; I have 32 or 64 GB of ram, depending on computer, so higher?
  (gcmh-high-cons-threshold (* 32 1024 1024))

  ;;--------------------
  :config
  ;;--------------------
  (gcmh-mode 1))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :dot-emacs 'core 'boot '10-init 'bootstrap)

;;; 20-packages.el --- Core Packages -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <code@brown.dev>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2022-11-17
;; Modified:   2022-11-17
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Core Packages
;;
;;; Code:


;;------------------------------------------------------------------------------
;; General Keybinds Package
;;------------------------------------------------------------------------------
;;              The Very Definition of a Modern Keybind General
;;                       ------------------------------
;;
;; https://github.com/noctuid/general.el
;;
;; Used by `evil', `evil-collection', and us to create keybinds a bit
;; nicer/neater than standard Emacs.
;;
;; Adds a `use-package' keyword, `:general', for creating General keybinds.

(imp:use-package general
  :demand t ;; Always load.

  ;;--------------------
  :init
  ;;--------------------

  ;; We want SPC as a leader key, probably. So do this. It just affects what
  ;; keybinds are overridden by the `override' keymap functionality that
  ;; `general' provides.
  ;;
  ;; https://github.com/emacs-evil/evil-collection#making-spc-work-similarly-to-spacemacs
  ;;
  ;; NOTE: `evil-collection' binds over SPC in many packages. To use SPC as a
  ;; leader key with `general', first set these override states:
  (setq general-override-states '(insert
                                  emacs
                                  hybrid
                                  normal
                                  visual
                                  motion
                                  operator
                                  replace))

  ;; Define some leader keys & definers for `:mantle' to use:


  ;; ;;--------------------
  ;; :config
  ;; ;;--------------------

  ;; Keybinds configured in "mantle/config/keybinds/" files, or nearer to
  ;; (ideally in) their `use-package'.
  )


;;------------------------------------------------------------------------------
;; Core `:keybinds' Module
;;------------------------------------------------------------------------------

(imp:load :feature  '(:keybind)
          :path     (imp:path:join innit:path:core/module
                                   "input"
                                   "keybind")
          :filename "init.el")



;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :core 'boot 'init 'packages)

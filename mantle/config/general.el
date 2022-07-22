;;; mantle/config/general.el --- General Keybinds -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2022-07-13
;; Modified:   2022-07-13
;; URL:        https://github.com/cole-brown/.config-emacs
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; General Keybinds
;; https://github.com/noctuid/general.el
;;
;; Used by `evil', `evil-collection', and us to create keybinds a bit
;; nicer/neater than standard Emacs.
;;
;; Adds a `use-package' keyword, `:general', for creating General keybinds.
;;
;;; Code:


;;------------------------------------------------------------------------------
;; The Very Definition of a Modern Keybind General
;;------------------------------------------------------------------------------

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


  ;; ;;--------------------
  ;; :config
  ;; ;;--------------------
  ;;
  ;; The big/general leader keys & definers defined in:
  ;;   "mantle/config/keybinds/leaders.el"
  ;;
  ;; Keybinds configured in "mantle/config/keybinds/" files, or nearer to
  ;; (ideally in) their `use-package'.
  )


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'config 'user 'general)

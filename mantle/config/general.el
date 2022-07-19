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
  ;; Depending on the 'SPC' leader function defined:
  ;; :after  (hydra)
  :after  (pretty-hydra)

  ;;--------------------
  :init
  ;;--------------------

  ;; https://github.com/emacs-evil/evil-collection#making-spc-work-similarly-to-spacemacs
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


  ;;--------------------
  :config
  ;;--------------------

  ;; TODO: This here or in some keybinds file?
  ;; https://github.com/emacs-evil/evil-collection#making-spc-work-similarly-to-spacemacs
  ;; NOTE: `evil-collection' binds over 'SPC' in many packages. To use 'SPC' as
  ;; a leader key with `general', second define your leader entry function:
  (general-define-key
   :states '(normal visual motion)
   :keymaps 'override
   ;; TODO: An actual 'SPC' leader hydra or function.
   "SPC" #'ignore))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'config 'user 'general)

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

  ;;------------------------------
  :init
  ;;------------------------------

  ;; TODO-meow: Do I need anything similar for `meow'?
  (when (imp:flag? :keybinds +evil)
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
                                    replace)))

  ;; ;;------------------------------
  ;; :config
  ;; ;;------------------------------

  ;; Keybinds configured in "mantle/config/keybinds/" files, or nearer to
  ;; (ideally in) their `use-package'.

  ;; Leaders, etc configured in "mantle/init/keybinds/" files, or nearer to
  ;; their `use-package' for specialty leaders.
  )


;;------------------------------------------------------------------------------
;; Keybind Menus
;;------------------------------------------------------------------------------
;; Need `transient' around early so it can be used to create keybind menus.

;; Examples: https://github.com/positron-solutions/transient-showcase
;; Manual:   https://magit.vc/manual/transient.html
;; Repo:     https://github.com/magit/transient
(imp:use-package transient
  ;; ...look `use-package'... Just because I added keybinds does /not/ mean you should break my init. :|
  ;; Just always load this, kay?
  :demand t
  
  ;;------------------------------
  :bind
  ;;------------------------------
  ;; Make "C-g" quit all instead of one, and "g" quit one transient.
  ;; TODO-meow: How to actually make that happen? These make "g" and "C-g", but
  ;; "f f C-g" still only backs out of one. Do I need to restart emacs or something?
  (:map transient-base-map
   ("g" . transient-quit-one)
   ("C-g" . transient-quit-all)))

;; TODO: Move hydra, pretty-hydra here?


;;------------------------------------------------------------------------------
;; Core Keybinds
;;------------------------------------------------------------------------------

;; Trying to avoid having any keybinds set by core.
;; Let's see how it goes.


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :core 'boot 'init 'packages)

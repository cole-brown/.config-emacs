;;; mantle/config/common.el --- General Development Environment Stuff -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2022-08-05
;; Timestamp:  2023-06-15
;; URL:        https://github.com/cole-brown/.config-emacs
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;  General Development Environment Stuff
;;
;;; Code:


(imp:require :jerky)


;;--------------------------------------------------------------------------------
;; Keybinds : Meow
;;--------------------------------------------------------------------------------

(imp:use-package emacs
  :when  (imp:flag? :keybinds +meow)
  :after (:and meow
               (:keybinds user general meow))

  ;;------------------------------
  :init
  ;;------------------------------

  ;; Create the Dev-Env/DevOps/etc keybinds menu in `general' for `meow'."
  (keybind:leader/global:def
    :infix (keybind:infix "d")                                    ; "dev-env"? "devops"?
    "" '(nil :which-key "Dev-Env, DevOps & Version Control..."))) ; infix title


;;------------------------------------------------------------------------------
;; Tabs
;;------------------------------------------------------------------------------

(jerky:set 'tab 'width 'code 'normal
           :namespace :default
           :value 4
           :docstr "Default/normal tab width is 4 spaces.")

(jerky:set 'tab 'width 'code 'short
           :namespace :default
           :value 2
           :docstr "Short tab width is 2 spaces.")


;; Use spaces instead of tabs.
(innit:customize-set-variable indent-tabs-mode nil)


;;------------------------------------------------------------------------------
;; Color Codes
;;------------------------------------------------------------------------------

;; Set the background color of color strings to the color they represent.
;; e.g.:
;;   - #ff1100
;;   - #abc
;;   - rgb(100, 100, 100)
;;   - YellowGreen
;;   - etc
;; https://github.com/emacsmirror/rainbow-mode/blob/master/rainbow-mode.el
;; https://elpa.gnu.org/packages/rainbow-mode.html
(imp:use-package rainbow-mode

  ;;------------------------------
  :init
  ;;------------------------------

  ;;---
  ;; Create a hook for enabling this minor mode.
  ;;---
  ;; Creates a func called `mantle:hook:rainbow-mode/enable'.
  ;;   (innit:hook:func/name:symbol "rainbow-mode/enable" nil)
  (innit:hook:defun
      (:name "rainbow-mode/enable"
       :docstr "Enable `rainbow-mode' (colorize color codes) for this buffer.")
    (rainbow-mode +1))

  ;;------------------------------
  :hook
  ;;------------------------------

  ((org-mode-hook    . mantle:hook:rainbow-mode/enable)
   (csharp-mode-hook . mantle:hook:rainbow-mode/enable)
   (css-mode-hook    . mantle:hook:rainbow-mode/enable)
   (php-mode-hook    . mantle:hook:rainbow-mode/enable)
   (html-mode-hook   . mantle:hook:rainbow-mode/enable)))


;;------------------------------------------------------------------------------
;; Diff
;;------------------------------------------------------------------------------

;;------------------------------
;; Ediff
;;------------------------------

(imp:use-package ediff

  ;;------------------------------
  :init
  ;;------------------------------

  (defvar mantle:user:ediff:window/cache nil
    "Cache of window config so it can be restored after quitting `ediff'.")

  (innit:hook:defun
      (:name   "ediff:window/save"
       :docstr "Save window config so it can be restored after quitting `ediff'.")
    (setq mantle:user:ediff:window/cache (current-window-configuration)))

  (innit:hook:defun
      (:name   "ediff:window/restore"
       :docstr "Restore a saved window config after quitting `ediff'."
       ;; NOTE: The `ediff:window/restore' hook needs the `:depth' argument!
       :depth  'append)
    (when (window-configuration-p mantle:user:ediff:window/cache)
      (set-window-configuration mantle:user:ediff:window/cache))
    (setq mantle:user:ediff:window/cache nil))


  ;;------------------------------
  :hook
  ;;------------------------------
  ;; Note: Use `innit:cmd:hook:func/name' to insert the func names created via the `innit:hook:defun' `:name' field.
  ((ediff-before-setup-hook             . mantle:hook:ediff:window/save)
   ((ediff-quit-hook ediff-suspend-hook) . mantle:hook:ediff:window/restore))


  ;;------------------------------
  :custom
  ;;------------------------------

  ;; Ignore whitespace.
  (ediff-diff-options "-w")

  ;; 'Unified'(?) instead of 'copied' context? IDK...
  (ediff-custom-diff-options "-u")

  ;; Always one frame.
  (ediff-window-setup-function 'ediff-setup-windows-plain)

  ;; Side-by-side instead of default top/bottom split.
  (ediff-split-window-function 'split-window-horizontally))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'config 'user 'dev-env 'common)

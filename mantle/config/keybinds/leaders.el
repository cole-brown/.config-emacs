;;; mantle/config/keybinds/leaders.el --- Who has two thumbs? This key. -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2022-07-21
;; Modified:   2022-07-21
;; URL:        https://github.com/cole-brown/.config-emacs
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Leader keys and their definer functions created here.
;;
;; To prevent your global leader keybindings from ever being overridden (e.g. an
;; evil package may bind "SPC"), use ~:keymaps 'override~:
;;   (my-leader-def
;;     :states 'normal
;;     :keymaps 'override
;;     "a" 'org-agenda)
;;
;; or the compact version:
;;   (my-leader-def 'normal 'override
;;     "a" 'org-agenda)
;;
;; For local leaders, use ~:keymaps 'local~
;;
;; NOTE: By default, evil keybindings made with ~:keymaps 'override~ will override
;; even those made with ~:keymaps 'local~.
;;
;; https://github.com/noctuid/general.el#override-keymaps-and-buffer-local-keybindings
;;
;; For example, to ruin your `evil-next-line' keybind (assuming you use evil):
;;   (general-create-definer jay-def
;;     :prefix "j"
;;     :keymaps 'override)
;;   (general-def
;;    :states '(normal visual motion)
;;    "j" (lambda () (interactive) (message "hi")))
;;
;; Now "j j" prints "hi" to the *Messages* buffer (& minibuffer).
;;
;;; Code:


;; TODO: Make a `:keybind' module?


;;------------------------------------------------------------------------------
;; Global Leader
;;------------------------------------------------------------------------------
;; https://github.com/noctuid/general.el#evil-examples

(defconst keybind:leader/global:key "SPC"
  "`kbd' type string to use as the primary keybinds leader key.

Add keybinds to the leader using function `keybind:leader/global'.")


;; This creates the macro `keybind:leader/global:def', which just calls
;; `general-def' with the arguments supplied here, which can be overridden by
;; callers.
(general-create-definer keybind:leader/global:def
  :prefix keybind:leader/global:key
  :states '(normal visual motion)
  :keymaps 'override)


;;------------------------------------------------------------------------------
;; Local Leader
;;------------------------------------------------------------------------------
;; https://github.com/noctuid/general.el#evil-examples

(defconst keybind:leader/local:key "SPC m"
  "`kbd' type string to use as the primary keybinds leader key.

Add keybinds to the leader using function `keybind:leader/local'.")


(general-create-definer keybind:leader/local:def
  :prefix keybind:leader/local:key
  :states '(normal visual motion)
  :keymaps 'override)


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'config 'user 'keybinds 'leaders)

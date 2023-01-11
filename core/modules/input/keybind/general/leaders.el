;;; core/modules/input/keybind/general/leaders.el --- Who has two thumbs? This key. -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2022-07-21
;; Modified:   2022-11-17
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


(imp:require :keybind 'general 'constants)


;;------------------------------------------------------------------------------
;; Leaderless
;;------------------------------------------------------------------------------
;; https://github.com/noctuid/general.el#evil-examples

;; This creates the macro `keybind:leader/global:def', which just calls
;; `general-def' with the arguments supplied here, which can be overridden by
;; callers.
(general-create-definer keybind:global:def
  :states  keybind:leader/global:states
  :keymaps keybind:leader/global:keymaps)


;;------------------------------------------------------------------------------
;; Global Leader
;;------------------------------------------------------------------------------
;; https://github.com/noctuid/general.el#evil-examples

;; This creates the macro `keybind:leader/global:def', which just calls
;; `general-def' with the arguments supplied here, which can be overridden by
;; callers.
(general-create-definer keybind:leader/global:def
  :prefix  keybind:leader/global:prefix
  :states  keybind:leader/global:states
  :keymaps keybind:leader/global:keymaps)


;; Give it its title.
(keybind:leader/global:def
  ;; Unbind the prefix and give it a title for which-key.
  "" '(nil :which-key "The Doyen of Keybind Leaders"))


;;------------------------------------------------------------------------------
;; Local Leader
;;------------------------------------------------------------------------------
;; https://github.com/noctuid/general.el#evil-examples

;; This creates the macro `keybind:leader/global:def', which just calls
;; `general-def' with the arguments supplied here, which can be overridden by
;; callers.
(general-create-definer keybind:leader/local:def
  :prefix  keybind:leader/local:prefix
  :states  keybind:leader/local:states
  :keymaps keybind:leader/local:keymaps)


;; Give it its title.
(keybind:leader/local:def
  ;; Unbind the prefix and give it a title for which-key.
  "" '(nil :which-key "Local Mode Leader"))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :keybind 'general 'leaders)

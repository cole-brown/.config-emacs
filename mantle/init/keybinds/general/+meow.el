;;; mantle/init/keybinds/+general-meow.el --- The General in Your Keybind Army -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2023-03-24
;; Modified:   2023-03-24
;; URL:        https://github.com/cole-brown/.config-emacs
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; The General in Your Keybind Army
;;
;;; Code:


(imp:use-package general
  :when  (imp:flag? :keybinds +meow)
  :after meow
  :demand t ;; Always load, if when/after/etc are valid.

  ;;------------------------------
  :init
  ;;------------------------------

  ;;--------------------------------------------------------------------------------
  ;; Aliases
  ;;--------------------------------------------------------------------------------
  ;; Meow's 5 states: `normal', `insert', `motion', `keypad', and `beacon'.

  ;; Short name aliases for meow state/mode keymaps. These will take over the
  ;; pre-existing evil aliases if conflicted.
  (push '((n normal) . meow-normal-state-keymap) general-keymap-aliases)
  ;; (general--unalias 'normal)
  ;; (general--unalias 'n)
  (push '((b beacon) . meow-beacon-state-keymap) general-keymap-aliases)
  (push '((i insert) . meow-insert-state-keymap) general-keymap-aliases)
  (push '((k keypad) . meow-keypad-state-keymap) general-keymap-aliases)
  (push '((m motion) . meow-motion-state-keymap) general-keymap-aliases)

  ;; NOTE: Cannot alias multiple states? This doesn't work:
  ;; (push '(command . (meow-normal-state-keymap meow-motion-state-keymap)) general-keymap-aliases)


  ;;------------------------------------------------------------------------------
  ;; Leader Keys
  ;;------------------------------------------------------------------------------

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


  ;;------------------------------------------------------------------------------
  ;; Leaderless / Global / Override
  ;;------------------------------------------------------------------------------
  ;; https://github.com/noctuid/general.el#evil-examples

  ;; This creates the macro `keybind:leader/global:def', which just calls
  ;; `general-def' with the arguments supplied here, which can be overridden by
  ;; callers.
  (general-create-definer keybind:global:def
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
    ;; Make sure not to steal insert mode's `self-insert' " " keybind!
    :keymaps '(meow-normal-state-keymap meow-motion-state-keymap))


  ;; Steal "SPC" for my own leader, give it its title, and I guess Meow's leader
  ;; can live inside "SPC"...
  (keybind:leader/global:def
    ;; Unbind the prefix and give it a title for which-key.
    "" '(nil :which-key "The Doyen of Keybind Leaders")

    ;; Usually bound as:
    ;;   `meow-motion-state-keymap SPC'
    ;;   `meow-normal-state-keymap SPC'
    ;; Rebind to be "SPC SPC":
    "SPC" (list #'meow-keypad :which-key "Meow Leader"))


  ;;------------------------------------------------------------------------------
  ;; Local Leader
  ;;------------------------------------------------------------------------------
  ;; https://github.com/noctuid/general.el#evil-examples

  ;; This creates the macro `keybind:leader/global:def', which just calls
  ;; `general-def' with the arguments supplied here, which can be overridden by
  ;; callers.
  (general-create-definer keybind:leader/local:def
    :prefix  keybind:leader/local:prefix
    :keymaps keybind:leader/local:keymaps)


  ;; ;; Give it its title.
  ;; (keybind:leader/local:def
  ;;   ;; Unbind the prefix and give it a title for which-key.
  ;;   "" '(nil :which-key "Local Mode Leader"))


  ;;------------------------------------------------------------------------------
  ;; Feature for Definers, Etc.
  ;;------------------------------------------------------------------------------
  ;; TODO: Have imp provide all of everything to Emacs?
  ;;       - That is, replace `imp:provide' with `imp:provide:with-emacs' in imp.
  (imp:provide:with-emacs :keybinds 'user 'general 'meow)
  )


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'init 'keybinds 'general '+meow)

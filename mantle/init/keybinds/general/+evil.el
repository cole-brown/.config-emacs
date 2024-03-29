;;; mantle/init/keybinds/general/+evil.el --- The `evil' General in My Keybind Army -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2022-07-21
;; Timestamp:  2023-06-27
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; The `evil' General in My Keybind Army
;;
;; Init the `evil' stuff in `general'.
;;
;; Create some definers to use in `use-package' during config.
;;
;;; Code:


(imp:use-package general
  :when  (imp:flag? :keybinds +evil)
  :after (:and magit evil evil-collection)
  :demand t ;; Always load, if when/after/etc are valid.

  ;;------------------------------
  :init
  ;;------------------------------

  ;;------------------------------------------------------------------------------
  ;; Global Leader
  ;;------------------------------------------------------------------------------
  ;; https://github.com/noctuid/general.el#evil-examples

  (defconst keybind:leader/global:states '(normal visual motion)
    "`kbd' type string to use as the primary keybinds leader key.

Add keybinds to the leader using function `keybind:leader/global'.")


  (defconst keybind:leader/local:keymaps 'local
    "Keymap to use so that (local-leader) keybinds override others.

They will always take precedence over keys bound in .

NOTE: Cannot use in `use-package' macro's `:general' sections! It doesn't get
replaced (soon enough) with whatever magic `general' does and then `override'
gets flagged as an invalid/non-existant keymap.
ADDENDUM: Is this true? Can use a definer in the `:general' section, anyways, I
believe? And my definers use these override maps?

See: https://github.com/noctuid/general.el#override-keymaps-and-buffer-local-keybindings")


  ;;------------------------------------------------------------------------------
  ;; Local Leader
  ;;------------------------------------------------------------------------------
  ;; https://github.com/noctuid/general.el#evil-examples

  (defconst keybind:leader/local:states '(normal visual motion)
    "`kbd' type string to use as the primary keybinds leader key.

Add keybinds to the leader using function `keybind:leader/local'.")


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
    :keymaps keybind:leader/local:keymaps)


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
    :prefix  (keybind:leader/local:prefix :evil)
    :states  keybind:leader/local:states
    :keymaps keybind:keymaps:override)


  ;; Give it its title.
  (keybind:leader/local:def
    ;; Unbind the prefix and give it a title for which-key.
    "" '(nil :which-key "Local Mode Leader"))


  ;;------------------------------------------------------------------------------
  ;; Feature for Definers, Etc.
  ;;------------------------------------------------------------------------------
  ;; TODO: Have imp provide all of everything to Emacs?
  ;;       - That is, replace `imp:provide' with `imp:provide:with-emacs' in imp.
  (imp:provide:with-emacs :keybinds 'general 'evil))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'init 'keybinds 'general '+evil)

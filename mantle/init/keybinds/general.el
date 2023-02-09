;;; mantle/init/keybinds/general/init.el --- The General in Your Keybind Army -*- lexical-binding: t; -*-
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
;; The General in Your Keybind Army
;;
;;; Code:


(imp:use-package general
   :demand t ;; Always load.

  ;;------------------------------
  :init
  ;;------------------------------

  ;;------------------------------------------------------------------------------
  ;; General Constants
  ;;------------------------------------------------------------------------------

  (defconst keybind:keymaps:override 'override
    "Keymap to use so that (non-local-leader) keybinds override others.

They will always take precedence over keys bound in other minor mode maps.

NOTE: Cannot use in `use-package' macro's `:general' sections! It doesn't get
replaced (soon enough) with whatever magic `general' does and then `override'
gets flagged as an invalid/non-existant keymap.

See: https://github.com/noctuid/general.el#override-keymaps-and-buffer-local-keybindings")


  ;;------------------------------------------------------------------------------
  ;; Global Leader
  ;;------------------------------------------------------------------------------
  ;; https://github.com/noctuid/general.el#evil-examples

  (defconst keybind:leader/global:prefix "SPC"
    "`kbd' type string to use as the primary keybinds leader key.

Add keybinds to the leader using function `keybind:leader/global'.")


  (defconst keybind:leader/global:states '(normal visual motion)
    "`kbd' type string to use as the primary keybinds leader key.

Add keybinds to the leader using function `keybind:leader/global'.")


  (defconst keybind:leader/global:keymaps keybind:keymaps:override
    "Magic value to use to prevent anyone else from overriding our keybind.

NOTE: Cannot use in `use-package' macro's `:general' sections! It doesn't get
replaced (soon enough) with whatever magic `general' does and then `override'
gets flagged as an invalid/non-existant keymap.

https://github.com/noctuid/general.el#override-keymaps-and-buffer-local-keybindings")


  ;;------------------------------------------------------------------------------
  ;; Local Leader
  ;;------------------------------------------------------------------------------
  ;; https://github.com/noctuid/general.el#evil-examples

  (defconst keybind:leader/local:prefix "SPC m"
    "`kbd' type string to use as the primary keybinds leader key.

Add keybinds to the leader using function `keybind:leader/local'.")


  (defconst keybind:leader/local:states '(normal visual motion)
    "`kbd' type string to use as the primary keybinds leader key.

Add keybinds to the leader using function `keybind:leader/local'.")


  (defconst keybind:leader/local:keymaps keybind:keymaps:override
    "Magic value to use to prevent anyone else from overriding our keybind.

NOTE: Cannot use in `use-package' macro's `:general' sections! It doesn't get
replaced with whatever magic `general' does and then gets flagged as an
invalid/non-existant keymap.

NOTE: `:keymaps' also has the special `local' argument, but I don't grok that at
all. Think you have to use it in, like, mode hooks or something? It's lacking in
useful examples...

https://github.com/noctuid/general.el#override-keymaps-and-buffer-local-keybindings")


  ;;------------------------------------------------------------------------------
  ;; Helpers
  ;;------------------------------------------------------------------------------

  (defun keybind:prefix (prefix &rest infix)
    "Create a leader `:prefix' string from PREFIX and INFIX.

PREFIX can be a string or one of the keywords: `:global', `:local'

INFIX must be a string.

See also function `keybind:infix'."
    (let ((prefix/keywords (list :global keybind:leader/global:prefix
                                 :local  keybind:leader/local:prefix)))
      ;;------------------------------
      ;; Validate
      ;;------------------------------
      ;; PREFIX
      (unless (or (stringp prefix)
                  (plist-member prefix/keywords prefix))
        (error "keybind:prefix: PREFIX must be a string or one of %S, got: %S"
               (seq-filter #'keywordp prefix/keywords)
               prefix))

      ;; INFIX
      (dolist (i infix)
        (unless (stringp i)
          (error "keybind:prefix: INFIX must be a string. Got %S: %S"
                 (type-of i)
                 i)))

      ;;------------------------------
      ;; Create Leader String
      ;;------------------------------
      (string-join (cons (if (plist-member prefix/keywords prefix)
                             (plist-get prefix/keywords prefix)
                           prefix)
                         infix)
                   " ")))
  ;; (keybind:prefix :global "g")
  ;; (keybind:prefix :local  "g")


  (defun keybind:infix (&rest infix)
    "Concat INFIX strings for a general `:infix' key's value.

For example:
  (keybind:leader/global:def
   :infix (keybind:infix \"i\" \"s\") ; insert -> signature
   [...]"
    (dolist (i infix)
      (unless (stringp i)
        (error "keybind:infix: INFIX must be a string. Got %S: %S"
               (type-of i)
               i)))

    (mapconcat #'identity infix " "))
  ;; (keybind:infix :i "s")
  ;; (keybind:infix "i" "s")


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
  ;; Feature for Definers, Etc.
  ;;------------------------------------------------------------------------------
  ;; TODO: do we need this? Don't think so?
  (imp:provide :keybinds 'user 'general))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'user 'init 'keybinds 'general)

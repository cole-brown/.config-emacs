;;; mantle/init/keybinds/general.el --- Initialize The General -*- lexical-binding: t; -*-
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
;; General Helpers, etc for `general' keybinds package.
;;
;; NOTE: This file is for things that are useful in evil, meow, vanilla, etc!
;; Specifics should go in the specific thing's `general' init, like
;; "./+meow.el".
;;
;;; Code:


(imp:use-package general
  :demand t ;; Always load, if when/after/etc are valid.

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
ADDENDUM: Is this true? Can use a definer in the `:general' section, anyways, I
believe? And my definers use these override maps?

See: https://github.com/noctuid/general.el#override-keymaps-and-buffer-local-keybindings")


  ;;------------------------------------------------------------------------------
  ;; Global Leader
  ;;------------------------------------------------------------------------------

  (defconst keybind:leader/global:prefix "SPC"
    "`kbd' type string to use as the primary keybinds leader key.

Add keybinds to the leader using function `keybind:leader/global'.")

  ;; TODO:meow:leader: Move this to `evil' if not usable by `meow'.
  (defconst keybind:leader/global:keymaps 'override
    "Keymap to use so that (non-local-leader) keybinds override others.

They will always take precedence over keys bound in other minor mode maps.

NOTE: Cannot use in `use-package' macro's `:general' sections! It doesn't get
replaced (soon enough) with whatever magic `general' does and then `override'
gets flagged as an invalid/non-existant keymap.
ADDENDUM: Is this true? Can use a definer in the `:general' section, anyways, I
believe? And my definers use these override maps?

See: https://github.com/noctuid/general.el#override-keymaps-and-buffer-local-keybindings")


  ;;------------------------------------------------------------------------------
  ;; Local Leader
  ;;------------------------------------------------------------------------------

  (defconst keybind:leader/local:prefix "SPC m"
    "`kbd' type string to use as the primary keybinds leader key.

Add keybinds to the leader using function `keybind:leader/local'.")


  ;; TODO:meow:leader: Move this to `evil' if not usable by `meow'.
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
  )



;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'init 'keybinds 'general 'general)

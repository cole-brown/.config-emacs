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


(require 'cl-lib)


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

  (defconst int<keybind>:leader/local:prefix '((:evil . "SPC m")
                                               (:meow . ((:emacs    . "C-x M-l") ; -> "SPC l"
                                                         ;; (:emacs . "C-x M-m") ; -> "SPC c m m"
                                                         (:meow     . "l")
                                                         (:personal . "l"))))
    "`kbd' type string to use as the primary keybinds leader key.

Use function `keybind:leader/local:prefix' to get the correct prefix.")


  (defun keybind:leader/local:prefix (&rest args)
    "`kbd' type string to use as the primary keybinds leader key.

Add keybinds to the leader using function `keybind:leader/local' or
`keybind:meow:leader/local:bind-keys`"
    (let ((prefix int<keybind>:leader/local:prefix))
      (dolist (arg args)
        (message "arg: %S -> %S" arg (alist-get arg prefix))
        (setq prefix (alist-get arg prefix)))
      ;; Failure?
      (unless (stringp prefix)
        (nub:error
            :innit
            "keybind:leader/local:prefix"
          "Did not find a (single) prefix string for arg '%S' in %S."
          args
          int<keybind>:leader/local:prefix))
      ;; Success; return the prefix.
      prefix))
  ;; (keybind:leader/local:prefix :evil)
  ;; (keybind:leader/local:prefix :meow)
  ;; (keybind:leader/local:prefix :meow :emacs)


  ;;------------------------------------------------------------------------------
  ;; Helpers
  ;;------------------------------------------------------------------------------


  (defun int<keybind>:keymaps/normalize (args)
    "Normalize ARGS to a list of keymaps.

Convert `global' and `:global' to `nil' for adding to the global keymap."
    (cl-flet ((normalize (keymap) ; Convert pretty 'global' keymap into its actual symbol.
                         (if (memq keymap '(:global global))
                             nil ; this is the global keymap
                           keymap)))
    (cond ((null args)
           ;; Got nothing, which we assume to mean "the global keymap", so
           ;; here's a list with that in it:
           '(nil))

          ((listp args)
           (seq-map #'normalize args))

          ((symbolp args)
           (list (normalize args)))

          (t
           (nub:error
               :innit
               "keybind:prefix"
             "Not sure what to do about local prefix...")))))
  ;; (int<keybind>:keymaps/normalize 'global)
  ;; (int<keybind>:keymaps/normalize '(global :global nil))
  ;; (int<keybind>:keymaps/normalize '(emacs-lisp-mode-map lisp-interaction-mode-map))


  (defun keybind:prefix (prefix &rest infix)
    "Create a leader `:prefix' string from PREFIX and INFIX.

PREFIX can be a string or one of the keywords: `:global', `:local'

INFIX must be a string.

See also function `keybind:infix'."
    (let ((prefix/keywords (list :global keybind:leader/global:prefix
                                 :local  (cond ((imp:flag? :keybinds +meow)
                                                (concat "SPC " (keybind:leader/local:prefix :meow :personal)))
                                               ((imp:flag? :keybinds +evil)
                                                (keybind:leader/local:prefix :evil))
                                               (t
                                                (nub:error
                                                    :innit
                                                    "keybind:prefix"
                                                  "Not sure what to do about local prefix..."))))))
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

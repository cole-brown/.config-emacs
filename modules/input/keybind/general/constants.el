;;; modules/keybind/general/constants.el --- Leader keys and other constants. -*- lexical-binding: t; -*-
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
;; Leader keys and other constants.
;;
;;; Code:


;;------------------------------------------------------------------------------
;; General Constants
;;------------------------------------------------------------------------------

(defconst keybind:override:keymaps 'override
  "`kbd' type string to use as the primary keybinds leader key.")


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


(defconst keybind:leader/global:keymaps keybind:override:keymaps
  "`kbd' type string to use as the primary keybinds leader key.

Add keybinds to the leader using function `keybind:leader/global'.")


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


(defconst keybind:leader/local:keymaps keybind:override:keymaps
  "`kbd' type string to use as the primary keybinds leader key.

Add keybinds to the leader using function `keybind:leader/local'.")


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
   :infix (keybind:infix \"i\" \"s\") ;; insert -> signature
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
;; The End.
;;------------------------------------------------------------------------------
(imp:provide:with-emacs :keybind 'general 'constants)

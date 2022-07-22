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
;; Global Leader
;;------------------------------------------------------------------------------
;; https://github.com/noctuid/general.el#evil-examples

(defconst keybind:leader/global:prefix "SPC"
  "`kbd' type string to use as the primary keybinds leader key.

Add keybinds to the leader using function `keybind:leader/global'.")


(defconst keybind:leader/global:states '(normal visual motion)
  "`kbd' type string to use as the primary keybinds leader key.

Add keybinds to the leader using function `keybind:leader/global'.")


(defconst keybind:leader/global:keymaps 'override
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


(defconst keybind:leader/local:keymaps 'override
  "`kbd' type string to use as the primary keybinds leader key.

Add keybinds to the leader using function `keybind:leader/local'.")


;;------------------------------------------------------------------------------
;; Helpers
;;------------------------------------------------------------------------------

(defun keybind:leader (prefix/root prefix/key)
  "Create a leader `:prefix' string from PREFIX/ROOT and PREFIX/KEY.

PREFIX/ROOT can be a string or one of the keywords: `:global', `:local'

PREFIX/KEY must be a string."
  (let ((prefix/keywords (list :global keybind:leader/global:prefix
                               :local  keybind:leader/local:prefix)))
    (cond ((not (stringp prefix/key))
           (error "keybind:leader: PREFIX/KEY must be a string, got: %S"
                  prefix/key))
          ((and (not (stringp prefix/root))
                (not (plist-member prefix/keywords prefix/root)))
           (error "keybind:leader: PREFIX/ROOT must be a string or one of %S, got: %S"
                  (seq-filter #'keywordp prefix/keywords)
                  prefix/root))
          ((plist-member prefix/keywords prefix/root)
             (string-join (list (plist-get prefix/keywords prefix/root)
                                prefix/key)
                          " "))
          (t
             (string-join (list prefix/root
                                prefix/key)
                          " ")))))
;; (keybind:leader :global "g")
;; (keybind:leader :local  "g")


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide:with-emacs :keybind 'general 'constants)

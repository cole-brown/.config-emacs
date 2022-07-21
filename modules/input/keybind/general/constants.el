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

(defconst keybind:leader/global:key "SPC"
  "`kbd' type string to use as the primary keybinds leader key.

Add keybinds to the leader using function `keybind:leader/global'.")


;;------------------------------------------------------------------------------
;; Local Leader
;;------------------------------------------------------------------------------
;; https://github.com/noctuid/general.el#evil-examples

(defconst keybind:leader/local:key "SPC m"
  "`kbd' type string to use as the primary keybinds leader key.

Add keybinds to the leader using function `keybind:leader/local'.")


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide:with-emacs :keybind 'general 'constants)

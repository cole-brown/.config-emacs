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
;;  Who has two thumbs? This key.
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Global Leader
;;------------------------------------------------------------------------------
;; https://github.com/noctuid/general.el#evil-examples

(defconst mantle:user:keybinds:leader/global:key "SPC"
  "`kbd' type string to use as the primary keybinds leader key.

Add keybinds to the leader using function `mantle:user:keybinds:leader/global'.")


(general-create-definer mantle:user:keybinds:leader/global
  :prefix mantle:user:keybinds:leader/global:key)


;;------------------------------------------------------------------------------
;; Local Leader
;;------------------------------------------------------------------------------
;; https://github.com/noctuid/general.el#evil-examples

(defconst mantle:user:keybinds:leader/local:key "SPC m"
  "`kbd' type string to use as the primary keybinds leader key.

Add keybinds to the leader using function `mantle:user:keybinds:leader/local'.")


(general-create-definer mantle:user:keybinds:leader/local
  :prefix mantle:user:keybinds:leader/local:key)


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'config 'user 'keybinds 'leaders)

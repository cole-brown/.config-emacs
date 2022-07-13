;;; mantle/config/undo-tree.el --- undo-tree configuration -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2022-07-13
;; Modified:   2022-07-13
;; URL:        https://github.com/cole-brown/.config-emacs
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Configure Undo and its Tree.
;;
;; NOTE: Prereq for `evil'!
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Undo-Tree
;;------------------------------------------------------------------------------

(imp:use-package undo-tree
  :demand t ;; Always load.

  ;;--------------------
  :init
  ;;--------------------

  ;; Declare that we'll have `undo-tree' available as the `evil' undo feature.
  ;; https://github.com/emacs-evil/evil#dependencies
  (imp:flag :evil +undo-tree)

  ;;--------------------
  :config
  ;;--------------------

  (global-undo-tree-mode +1))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'config 'user 'undo-tree)

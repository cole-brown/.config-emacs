;;; mantle/config/meow.el --- 'Less is More' Modal Editing -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2022-07-12
;; Modified:   2023-02-09
;; URL:        https://github.com/cole-brown/.config-emacs
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Meow: 'Less is More' Modal Editing
;;
;; https://github.com/meow-edit/meow
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Meow
;;------------------------------------------------------------------------------

;; Code: https://github.com/meow-edit/meow
;; Docs: https://github.com/meow-edit/meow#documents
(imp:use-package meow
  :when  (imp:flag? :keybinds +meow)

  ;;------------------------------
  :custom
  ;;------------------------------

  ;; Don't aggregate all actions while in insert state into a single undo action.
  ;; Act like normal Emacs instead.
  (meow-cheatsheet-layout (cond ((imp:flag? :keyboard +dvorak)
                                 meow-cheatsheet-layout-dvorak)
                                ((imp:flag? :keyboard +qwerty)
                                 meow-cheatsheet-layout-qwerty)
                                (t
                                 meow-cheatsheet-layout)))

  ;;------------------------------
  :config
  ;;------------------------------
  (meow-mode +1))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'init 'keybinds '+meow)

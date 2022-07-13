;;; mantle/config/evil.el -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2022-07-12
;; Modified:   2022-07-12
;; URL:        https://github.com/cole-brown/.config-emacs
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;  Configure Evil and Friends.
;;
;;; Code:


;;---------------------------Emacs Vim Input Layer -----------------------------
;;--                +1.5 Evilness and +7.0 Keybind Confusion                  --
;;---------------------------(I'm still learning.)------------------------------

;; TODO: Is this needed?
;; (imp:provide :input 'keyboard 'evil)


;;------------------------------------------------------------------------------
;; Undo-Tree
;;------------------------------------------------------------------------------

;; TODO: Move elsewhere and change evil's use-package to figure out what undo to use.
;; https://github.com/emacs-evil/evil#dependencies
(imp:use-package evil
  ;;--------------------
  :config
  ;;--------------------

  (global-undo-tree-mode +1))


;;------------------------------------------------------------------------------
;; Evil
;;------------------------------------------------------------------------------

(imp:use-package evil
  ;; https://github.com/emacs-evil/evil#dependencies
  ;; :preface
  ;; (setq evil-undo-system (cond ((imp:flag? :emacs +undo-tree) 'undo-tree)
  ;;                              ((imp:flag? :emacs +undo-fu) 'undo-fu)
  ;;                              ((> emacs-major-version 27) 'undo-redo)))

  ;; :after (:any undo-tree)

  ;;--------------------
  :init
  ;;--------------------

  ;;--------------------
  :hook
  ;;--------------------

  ;;--------------------
  :custom
  ;;--------------------

  ;;--------------------
  :config
  ;;--------------------



  )


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'config 'user 'evil)

;;; mantle/config/keybinds/init.el --- Set up custom keybinds. -*- lexical-binding: t; -*-
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
;;  Set up custom keybinds.
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Keybinds Dependencies
;;------------------------------------------------------------------------------

(let ((path/here (imp:path:current:dir/relative :mantle)))
  ;; Just assume all keybinds in here need `general'. Most of them will.
  (imp:eval:after (:keybind general ready)
   ;; TODO: fix indentation?

   ;;------------------------------
   ;; Set up local & global leaders' common infix menus.
   ;;------------------------------
   (imp:load :feature  '(:mantle config user keybinds infixes)
             :path     path/here
             :filename "infixes")

   ;; TODO: Do we have an evil vs non-evil check for whether to load Emacs or
   ;; Evil keybind files?


   ;;------------------------------
   ;; Emacs Keybinds
   ;;------------------------------


   ;;------------------------------
   ;; Evil Keybinds
   ;;------------------------------
   (imp:eval:after (:and evil evil-collection)

    (imp:load :feature  '(:mantle config user keybinds signature)
              :path     path/here
              :filename "signature"))))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'config 'user 'keybinds)

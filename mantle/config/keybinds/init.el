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
   ;; Prereqs
   ;;------------------------------
   ;; Set up local & global leaders' common infix menus.
   (imp:load :feature  '(:mantle config user keybinds infixes)
             :path     path/here
             :filename "infixes")

   ;; TODO: Do we have an evil vs non-evil check for whether to load Emacs or
   ;;       Evil keybind files?
   ;; TODO: What're them flags I have and are they useful here?

   ;;------------------------------
   ;; Common or Smart Keybinds
   ;;------------------------------
   ;; Keybinds that don't care about Emacs/Evil, or can figure out which kind to create.

   (imp:load :feature  '(:mantle config user keybinds signature)
             :path     path/here
             :filename "signature")

   ;;------------------------------
   ;; Emacs Keybinds
   ;;------------------------------
   ;; TODO: Do these need to be after anything?
   ;; TODO:   - No `evil' to load...
   ;; TODO:   - `general' is already loaded...
   ;; (imp:eval:after TODO-something-or-other-maybe?
   ;;
   ;;  (imp:load :feature  '(:mantle config user keybinds emacs)
   ;;            :path     (imp:path:join path/here "emacs")
   ;;            :filename "init"))

   ;;------------------------------
   ;; Evil Keybinds
   ;;------------------------------
   (imp:eval:after (:and evil evil-collection)

    (imp:load :feature  '(:mantle config user keybinds evil)
              :path     (imp:path:join path/here "evil")
              :filename "init"))))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'config 'user 'keybinds)

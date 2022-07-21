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
;; Just assume all keybinds in here need `general'. Most of them will.
(imp:eval:after general


  ;;----------------------------------------------------------------------------
  ;; Keybind Leaders & Definer Functions
  ;;----------------------------------------------------------------------------

  (imp:load :feature  '(:mantle config user keybinds leaders)
            :path     (imp:path:current:dir/relative :mantle)
            :filename "leaders.el"))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'config 'user 'keybinds)

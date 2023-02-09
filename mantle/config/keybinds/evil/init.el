;;; mantle/config/keybinds/evil/init.el --- Set up custom evil keybinds. -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2022-11-08
;; Modified:   2022-11-08
;; URL:        https://github.com/cole-brown/.config-emacs
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;  Set up custom evil keybinds.
;;
;;; Code:

;; TODO-meow: meow version of this?

;;------------------------------------------------------------------------------
;; Eeeeevil Keybinds!!!
;;------------------------------------------------------------------------------
;; ...of evil!

;; NOTE: We're already called after evil, general, etc are ready.
;; e.g.:
;;   (imp:eval:after (:and (:keybinds user general)
;;                         general
;;                         evil
;;                         evil-collection))
;;   <stuff in this file>)


;;------------------------------
;; Keybinds for Evil Commands
;;------------------------------
(imp:load :feature  '(:mantle config user keybinds evil evil)
          :path     (imp:path:current:dir/relative :mantle)
          :filename "evil")


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'config 'user 'keybinds '+evil)

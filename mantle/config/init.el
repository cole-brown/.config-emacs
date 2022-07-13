;;; config/init.el --- Configure User Stuff -*- lexical-binding: t; -*-
;;
;; Author: Cole Brown <code@brown.dev>
;; URL:    https://github.com/cole-brown/.config-emacs
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;  Configure User Stuff in This Order
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Config: Input
;;------------------------------------------------------------------------------

(imp:load :feature  '(:mantle config user general)
          :path     (imp:path:current:dir/relative :mantle)
          :filename "general")

;; Sets `imp:flag', which is used by `evil' config to figure out which optional
;; undo system to use.
(imp:load :feature  '(:mantle config user undo-tree)
          :path     (imp:path:current:dir/relative :mantle)
          :filename "undo-tree")


;; Set up `evil' et al. Do early in config in case we want to tweak a lot along
;; the way.
(imp:load :feature  '(:mantle config user evil)
          :path     (imp:path:current:dir/relative :mantle)
          :filename "evil")


;;------------------------------------------------------------------------------
;; Config: Secrets (Consts, Vars, Etc.)
;;------------------------------------------------------------------------------

(imp:load :feature  '(:mantle config user secret)
          :path     (imp:path:current:dir/relative :mantle)
          :filename "secret")


;;------------------------------------------------------------------------------
;; Config: Modes
;;------------------------------------------------------------------------------

(imp:load :feature  '(:mantle config user org-mode)
          :path     (imp:path:current:dir/relative :mantle)
          :filename "org-mode")


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'config 'user)

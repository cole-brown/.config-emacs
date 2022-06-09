;;; init/init.el --- Initialize User Stuff -*- lexical-binding: t; -*-
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
;;  Initialize User Stuff
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Run User's Init Files in This Order
;;------------------------------------------------------------------------------

;;------------------------------
;; Init: Modules
;;------------------------------
(imp:load :feature  '(:mode org)
          :path     (imp:path:join innit:path:module "mode" "org")
          :filename "init") ; Needed by ':mantle/theme/init'.


;;------------------------------
;; Init: Theme
;;------------------------------
(imp:load :feature  '(:mantle theme init)
          :path     innit:theme:path
          :filename "init")


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'init 'user)

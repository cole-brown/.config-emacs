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
;;  Configure User Stuff
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Run User's Config Files in This Order
;;------------------------------------------------------------------------------

(let ((path/dir (imp:path:current:dir)))

  (imp:load :feature  '(:mantle config org-mode)
            :path     path/dir
            :filename "org-mode"))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'config 'user)

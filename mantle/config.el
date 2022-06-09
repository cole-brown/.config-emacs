;;; config.el --- User-Level Emacs Configuration -*- lexical-binding: t; -*-
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
;; User-Level Emacs Configuration
;;
;;; Code:


;;------------------------------------------------------------------------------
;; User Configs: Run in This Order
;;------------------------------------------------------------------------------

(imp:load :feature  '(:mantle config)
          :path     (imp:path:join innit:path:mantle "config")
          :filename "init")


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'init)

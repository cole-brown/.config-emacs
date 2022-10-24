;;; 50-mantle.el --- Hand init over to user. -*- lexical-binding: t; -*-
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
;; Hand init over to user.
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Configure: 'mantle/' (User's Config)
;;------------------------------------------------------------------------------

(imp:load :feature  '(:mantle config)
          :filename "config")


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :core 'boot 'config 'mantle)

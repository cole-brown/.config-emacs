;;; core/boot/20-config/50-mantle.el --- Hand init over to user. -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2022-03-25
;; Timestamp:  2023-06-29
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Hand init over to user for the bulk of their Emacs set-up.
;;
;; More precisely, hand the 'config' stage over to 'mantle/config'.
;;
;; We get back init briefly for the 'finalize' stage.
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Configure: 'mantle/' (User's Config)
;;------------------------------------------------------------------------------

(imp:load :feature  '(:mantle lower config)
          :filename "config")


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :core 'boot 'config 'mantle)

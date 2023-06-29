;;; core/boot/10-init/80-mantle.el --- Load the mantle (user init) entry file. -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2022-03-28
;; Timestamp:  2023-06-29
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Load the mantle (user init) entry file.
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Initialize: 'mantle/' (User's Init)
;;------------------------------------------------------------------------------

(imp:load :feature  '(:mantle lower init)
          :filename "init")


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :core 'boot 'init 'mantle)

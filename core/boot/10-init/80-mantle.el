;;; core/boot/10-init/80-mantle.el --- Load the mantle (user init) entry file. -*- lexical-binding: t; -*-
;;
;; Author: Cole Brown <code@brown.dev>
;; URL:    https://github.com/cole-brown/.config-emacs
;;
;;; Commentary:
;;;
;;; Load the mantle (user init) entry file.
;;;
;;; Code:


;;------------------------------------------------------------------------------
;; Initialize: 'mantle/' (User's Init)
;;------------------------------------------------------------------------------

(imp:load :feature  '(:mantle init)
          :filename "init")


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :core 'boot 'init 'mantle)

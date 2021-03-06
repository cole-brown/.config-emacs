;;; 20-load.el --- Set up loads. -*- lexical-binding: t; -*-
;;
;; Author: Cole Brown <code@brown.dev>
;; URL:    https://github.com/cole-brown/.config-emacs
;;
;;; Commentary:
;;; Code:


;;------------------------------------------------------------------------------
;; Initialize: 'mantle/' (User's Init)
;;------------------------------------------------------------------------------

(imp:load :feature  '(:mantle init)
          :filename "init")


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :core 'boot '10-init 'load)

;;; settings.el --- Settings for Innit -*- lexical-binding: t; -*-
;;
;; Author: Cole Brown <code@brown.dev>
;; URL:    https://github.com/cole-brown/.config-emacs
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;; Code:



;;------------------------------------------------------------------------------
;; Debug
;;------------------------------------------------------------------------------

;; You should use the Emacs `--debug-init' flag on the command line...
;; (setq init-file-debug nil)
;; (setq debug-on-error nil)


;;------------------------------------------------------------------------------
;; Init Output
;;------------------------------------------------------------------------------

;; Allow messages during innit for debugging.
(setq innit:display:messages  t
      innit:display:load-file t)


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :settings)

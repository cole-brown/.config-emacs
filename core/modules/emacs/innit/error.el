;;; error.el --- Errors during init. -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2022-04-26
;; Modified:   2022-10-25
;; URL:        https://github.com/cole-brown/.config-emacs
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;  Errors during init.
;;
;;; Code:


;; TODO: Actually use these errors during start-up.

;;------------------------------------------------------------------------------
;; Custom Error Types
;;------------------------------------------------------------------------------

;; TODO: Trim down to just what I'm using?
(define-error 'innit:error          "Error in Innit Emacs core")
(define-error 'innit:error:hook     "Error in an Innit startup hook"  'innit:error)
(define-error 'innit:error:autoload "Error in Innit's autoloads file" 'innit:error)
(define-error 'innit:error:module   "Error in an Innit module"        'innit:error)
(define-error 'innit:error:private  "Error in private config"         'innit:error)
(define-error 'innit:error:package  "Error with packages"             'innit:error)


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide:with-emacs :innit 'error)

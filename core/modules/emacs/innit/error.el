;;; error.el --- Errors during init. -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Cole Brown
;;
;; Author: Cole Brown <code@brown.dev>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created: April 26, 2022
;; Modified: April 26, 2022
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/work/error
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
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

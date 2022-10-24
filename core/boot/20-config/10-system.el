;;; 10-system.el --- System/Host Configuration -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <code@brown.dev>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2022-10-24
;; Modified:   2022-10-24
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;; Code:


(imp:require :system 'multiplexer 'dlv)


;;------------------------------------------------------------------------------
;; This System's Domain DLVs
;;------------------------------------------------------------------------------

;; Set DLV system domain for all registered functions & args lists.
(system:multiplexer:dlv:domain/all)


;;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :core 'boot 'config 'system)

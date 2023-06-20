;;; core/boot/20-config/00-init.el --- Normal Init: System/Host Configuration -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2022-10-24
;; Timestamp:  2023-06-20
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Configure system/host things.
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

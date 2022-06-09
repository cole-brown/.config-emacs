;;; 00-config.el --- Configure packages & stuff -*- lexical-binding: t; -*-
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


(imp:require :system 'multiplexer 'dlv)


;;------------------------------------------------------------------------------
;; Configure: Pre-Mantle (Before User's Config)
;;------------------------------------------------------------------------------

;;------------------------------
;; This System's Domain DLVs
;;------------------------------
;; Set DLV system domain for all registered functions & args lists.
(system:multiplexer:dlv:domain/all)


;;------------------------------------------------------------------------------
;; Configure: 'mantle/' (User's Config)
;;------------------------------------------------------------------------------

(imp:load :feature  '(:mantle config)
          :filename "config")


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :core 'boot '20-config 'config)

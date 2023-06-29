;;; mantle/init/identity.el --- Leeloo Dallas, Multi-Pass -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2020-11-10
;; Timestamp:  2023-06-29
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Leelo Dallas, Multi-Pass
;;
;; My Identity stuff. Mostly done in secret repo, actually.
;;
;;; Code:


(imp:require :tools 'signature)


;;------------------------------------------------------------------------------
;;                               !! - NOTE - !!
;;------------------------------------------------------------------------------
;; I am setting this stuff in my systems/secrets repo.


;; ;;------------------------------------------------------------------------------
;; ;; Hello, my name is...
;; ;;------------------------------------------------------------------------------
;;
;; ;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; ;; clients, file templates and snippets.
;; (setq user-full-name    "Cole Brown"
;;       user-mail-address "code@brown.dev")
;;
;;
;; ;;------------------------------------------------------------------------------
;; ;; Signatures & Marks
;; ;;------------------------------------------------------------------------------
;;
;; (signature:create "♫" user-full-name)
;;
;;
;; ;;------------------------------------------------------------------------------
;; ;; Emails for Signatures
;; ;;------------------------------------------------------------------------------
;;
;; (signature:set 'id 'email
;;                 :namespace :work
;;                 :value "jeff.jefferson@example.com"
;;                 :docstr "Work Email")
;;
;; (signature:set 'id 'email
;;                 :namespace :home
;;                 :value "jeff@example.com"
;;                 :docstr "Home Email")
;;
;; (signature:set 'id 'email
;;                 :namespace :default
;;                 :value "code@example.com"
;;                 :docstr "Open Source Coding Email")



;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'init 'identity)

;;; config/identity.el -*- lexical-binding: t; -*-


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
(imp:provide :mantle 'init 'user 'identity)

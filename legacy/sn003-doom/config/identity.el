;;; config/identity.el -*- lexical-binding: t; -*-


(imp:provide :modules 'spy 'io 'signature)


;;------------------------------------------------------------------------------
;; Hello, my name is...
;;------------------------------------------------------------------------------

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Cole Brown"
      user-mail-address "code@brown.dev")


;;------------------------------------------------------------------------------
;; Signatures & Marks
;;------------------------------------------------------------------------------

(spy:signature/create "♫" user-full-name)

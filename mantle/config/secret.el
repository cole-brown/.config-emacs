;;; config/secrets.el -*- lexical-binding: t; -*-


(imp:require :system 'secret)


;;---------------------------Configure Our Secrets------------------------------
;;--               You're not allowed to know what they are.                  --
;;---------------------------------(shhh...)------------------------------------

;; Configure our secrets if we have any.
(system:secret:config :secret 'config)


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'config 'user 'secret)

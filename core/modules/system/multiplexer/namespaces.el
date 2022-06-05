;;; system/multiplexer/namespaces.el -*- lexical-binding: t; -*-

;;---------------------Jerky Namespaces for the System(s)-----------------------
;;--                           Default Namespaces                             --
;;------------------------------------------------------------------------------


(imp:require :jerky)


;;------------------------------------------------------------------------------
;; Create Namespaces
;;------------------------------------------------------------------------------

;;------------------------------
;; Work Namespace
;;------------------------------
;; Default fallback is ok.
(jerky:namespace:create :work
                        :title "Namespace for the Job"
                        :docstr "Work/Job-related variables.")


;;------------------------------
;; Home Namespace
;;------------------------------
;; Default fallback is ok.
(jerky:namespace:create :home
                        :title "Namespace for House & Home"
                        :docstr "Homework, Side-Projects, Personal Notes, etc...")


;;------------------------------
;; Secret Namespace
;;------------------------------
;; No fallback allowed.
(jerky:namespace:create :secret
                        :title "Namespace for the Confidential"
                        :docstr "You've seen this, so... Now I have to kill you, I guess?"
                        :fallbacks jerky:namespace/no-fallback)


;;------------------------------------------------------------------------------
;; Initialize Namespaces
;;------------------------------------------------------------------------------

;;------------------------------
;; Set the Default System Namespace
;;------------------------------
;; - Can be overwritten during system multiplex init if a different default
;;   namespace is desired.
;; - No fallback allowed.
(jerky:set 'namespace 'system
           :default
           :title "Default namespace for the System/Computer"
           :docstr "Default Namespace - no fallback."
           :fallbacks jerky:namespace/no-fallback)


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :system 'multiplexer 'namespaces)

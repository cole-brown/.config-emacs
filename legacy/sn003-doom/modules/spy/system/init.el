;; -*- mode: emacs-lisp; lexical-binding: t -*-


;;------------------Init & Config Help for Multiple Systems.--------------------
;;--                     What computer is this anyways?                       --
;;--------------------------(probably the wrong one)----------------------------

(spy/require :spy 'jerky)


;;------------------------------------------------------------------------------
;; Constants & Variables
;;------------------------------------------------------------------------------

;; Default fallback is ok.
(jerky/namespace/create :work ;; System Namespace
           :title "Namespace for the Job"
           :docstr "Work/Job-related variables.")

;; Default fallback is ok.
(jerky/namespace/create :home ;; System Namespace
           :title "Namespace for House & Home"
           :docstr "Homework, Side-Projects, Personal Notes, etc...")

;; Default fallback is ok.
(jerky/namespace/create :secret ;; System Namespace
           :title "Namespace for the Confidential"
           :docstr "You've seen this, so... Now I have to kill you, I guess?"
           :fallbacks :jerky/namespace/no-fallback)


;;------------------------------------------------------------------------------
;; Multiple systems (computers) able to use this same Doom Config.
;;------------------------------------------------------------------------------

;; Always load `multiplex' unless specifically removed.
(unless (featurep! -multiplex)
   (load! "+multiplex"))


;;------------------------------------------------------------------------------
;; Config/Load Helpers.
;;------------------------------------------------------------------------------

;; Always load `config' unless specifically removed.
(unless (featurep! -config)
   (load! "+config"))

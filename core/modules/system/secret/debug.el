;;; system/secret/debug.el --- Debugging/error help. -*- lexical-binding: t; -*-


(imp:require :nub)


;;------------------------------------------------------------------------------
;; Initialization
;;------------------------------------------------------------------------------

(defun int<system/secret>:nub:init ()
  "Initialize nub user & settings for secret."
  ;; Defaults for all the settings.
  (nub:vars:init :system/secret))



;;------------------------------------------------------------------------------
;; Debugging Toggle
;;------------------------------------------------------------------------------

(defun system:secret:debug:toggle ()
  "Toggle debugging for dlv."
  (interactive)
  (nub:debug:toggle :system/secret))


;;------------------------------------------------------------------------------
;; Debugging Functions
;;------------------------------------------------------------------------------

;; Just use:
;;   - `nub:debug'
;;   - `nub:debug:func/start'
;;   - `nub:debug:func/end'


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :system 'secret 'debug)

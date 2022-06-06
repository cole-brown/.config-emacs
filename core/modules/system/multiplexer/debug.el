;;; system/multiplexer/debug.el --- Debugging/error help. -*- lexical-binding: t; -*-


(imp:require :nub)


;;------------------------------------------------------------------------------
;; Initialization
;;------------------------------------------------------------------------------

(defun int<system/multiplexer>:nub:init ()
  "Initialize nub user & settings for multiplexer."
  ;; Defaults for all the settings except for the levels/enabled setting.
  (nub:vars:init :system/multiplexer))



;;------------------------------------------------------------------------------
;; Debugging Toggle
;;------------------------------------------------------------------------------

(defun system:multiplexer:debug:toggle ()
  "Toggle debugging for dlv."
  (interactive)
  (nub:debug:toggle :system/multiplexer))


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
(imp:provide :system 'multiplexer 'debug)

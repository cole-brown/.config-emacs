;;; emacs/dlv/debug.el --- Debugging functionality for `dlv'. -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Debugging functionality for `dlv'.
;;
;;; Code:


(imp:require :nub)


;;------------------------------------------------------------------------------
;; Initialization
;;------------------------------------------------------------------------------

(defun int<dlv>:debug:init ()
  "Initialize dlv debugging."
  ;; Defaults for all the settings except for the levels/enabled setting.
  (nub:vars:init :dlv
                 nil
                 nil
                 (list (cons :error t)
                       (cons :warn  t)
                       (cons :info  t)
                       (cons :debug (imp:flag? :dlv +debug)))
                 nil))



;;------------------------------------------------------------------------------
;; Debugging Toggle
;;------------------------------------------------------------------------------

(defun dlv:debug:toggle ()
  "Toggle debugging for dlv."
  (interactive)
  (nub:debug:toggle :dlv))


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
(imp:provide :dlv 'debug)

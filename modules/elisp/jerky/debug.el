;;; jerky/debug.el -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Debugging functionality for jerky.
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Initialization
;;------------------------------------------------------------------------------

(defun int<jerky>:debug:init ()
  "Initialize jerky debugging."
  ;; Just use defaults for all settings.
  (nub:vars:init :jerky))


;;------------------------------------------------------------------------------
;; Debugging Toggle
;;------------------------------------------------------------------------------

(defun jerky:debug:toggle ()
  "Toggle debugging for jerky."
  (interactive)
  (nub:debug:toggle :jerky))


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
(imp:provide:with-emacs :jerky 'debug)

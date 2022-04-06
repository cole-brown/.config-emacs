;;; 10-debug.el --- Early Debugging -*- lexical-binding: t; -*-
;;
;; Author: Cole Brown <code@brown.dev>
;; URL:    https://github.com/cole-brown/.config-emacs
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;; Code:


;;------------------------------------------------------------------------------
;; Debug Variables
;;------------------------------------------------------------------------------

(defvar init:debug? (or (getenv-internal "DEBUG") init-file-debug)
  "If non-nil, flags the init as debugging / debug mode.

Use `init:debug:toggle' to toggle it. The '--debug-init' flag and setting the
DEBUG envvar will enable this at startup.")


;;------------------------------------------------------------------------------
;; Debug Functions
;;------------------------------------------------------------------------------

(defun init:debug:set (enable)
  "Set `init:debug?' to ENABLE flag & ensure Emacs debug flags match.

ENABLE should be nil/non-nil. Sets Emacs variables `init-file-debug' and
`debug-on-error' to match boolean state of `init:debug?'.

NOTE: If it needs to get any more fancy, consider a minor mode like
`doom-debug-mode'."
  ;; Set our flag to exactly whatever enable is.
  (setq init:debug? enable)
  ;; Set Emacs' flags to just t/nil.
  (setq debug-on-error  (not (null enable))
        init-file-debug debug-on-error))


(defun init:debug:toggle ()
  "Toggle `init:debug?' flag & ensure Emacs debug flags match."
  (init:debug:set (not init:debug?)))


;;------------------------------------------------------------------------------
;; The End
;;------------------------------------------------------------------------------

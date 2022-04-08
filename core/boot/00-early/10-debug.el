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

(defvar innit:debug? (or (getenv-internal "DEBUG") init-file-debug)
  "If non-nil, flags the init as debugging / debug mode.

Use `innit:debug:toggle' to toggle it. The '--debug-init' flag and setting the
DEBUG envvar will enable this at startup.")


;;------------------------------------------------------------------------------
;; Debug Functions
;;------------------------------------------------------------------------------

(defun innit:debug:set (enable)
  "Set `innit:debug?' to ENABLE flag & ensure Emacs debug flags match.

ENABLE should be nil/non-nil. Sets Emacs variables `init-file-debug' and
`debug-on-error' to match boolean state of `innit:debug?'.

NOTE: If it needs to get any more fancy, consider a minor mode like
`doom-debug-mode'."
  ;; Set our flag to exactly whatever enable is.
  (setq innit:debug? enable)
  ;; Set Emacs' flags to just t/nil.
  (setq debug-on-error  (not (null enable))
        init-file-debug debug-on-error))


(defun innit:debug:toggle ()
  "Toggle `innit:debug?' flag & ensure Emacs debug flags match."
  (innit:debug:set (not innit:debug?)))


;;------------------------------------------------------------------------------
;; The End
;;------------------------------------------------------------------------------

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

(defvar innit:debug? nil
  "If non-nil, flags the init as debugging / debug mode.

Use `innit:debug:toggle' to toggle it. The '--debug-init' flag and setting the
DEBUG envvar will enable this at startup.")


;;------------------------------------------------------------------------------
;; Debug Functions
;;------------------------------------------------------------------------------

(defun innit:debug? (&optional any)
  "Return non-nil if debugging.

If ANY is nil (default), just return value of `innit:debug?' variable.
If ANY is non-nil, return non-nil if any of these are non-nil:
  1) `innit:debug?' variable
  2) `debug-on-error' variable
  3) `init-file-debug' variable"
  (if any
      (or innit:debug?
          debug-on-error
          init-file-debug)
    innit:debug?))


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
;; Set-Up
;;------------------------------------------------------------------------------

;; Set our debug variable (`innit:debug?') and Emacs' variables based on inputs.
(cond
 ;;---
 ;; Environment Variable: DEBUG
 ;;---
 ((and (getenv-internal "DEBUG")
       (not init-file-debug)
       (not debug-on-error))
  (setq innit:debug? (getenv-internal "DEBUG"))

  ;; Also cascade into Emacs?
  (setq init-file-debug t
        debug-on-error t)

  ;; TODO: `innit' function for init/debug messages, etc?
  (message "innit: '%s': Enable `innit:debug?' from environment variable DEBUG: %S"
           ;; TODO: `innit' function for relative path of filename for init/debug messages?
           "core/00-early/10-debug.el"
           innit:debug?))

 ;;---
 ;; CLI Flag: "--debug-init"
 ;;---
 (init-file-debug
  (setq innit:debug? init-file-debug)

  ;; TODO: `innit' function for init/debug messages, etc?
  (message "innit: '%s': Enable `innit:debug?' from '--debug-init' CLI flag: %S"
           ;; TODO: `innit' function for relative path of filename for init/debug messages?
           "core/00-early/10-debug.el"
           innit:debug?))

 ;;---
 ;; Interactive Flag?
 ;;---
 ;; How did you get this set and not `init-file-debug'? Debugging some small
 ;; piece of init, maybe?
 (debug-on-error
  (setq innit:debug? debug-on-error

  ;; TODO: `innit' function for init/debug messages, etc?
  (message "innit: '%s': Enable `innit:debug?' from `debug-on-error' variable: %S"
           ;; TODO: `innit' function for relative path of filename for init/debug messages?
           "core/00-early/10-debug.el"
           innit:debug?))))


;;------------------------------------------------------------------------------
;; The End
;;------------------------------------------------------------------------------

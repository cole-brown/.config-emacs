;;; squelch.el --- Squelch Output -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2022-08-01
;; Modified:   2022-08-01
;; URL:        https://github.com/cole-brown/.config-emacs
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Squelch / Quiet / Silence / Muffle / Whatever your output.
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Silent Function Replacements
;;------------------------------------------------------------------------------

(defun innit:squelch:write-region (start end filename &optional append visit lockname mustbenew)
  "Silent version of `write-region'.

See `write-region' for details on args: START, END, FILENAME, APPEND, VISIT,
LOCKNAME, and MUSTBENEW.

Will set VISIT to `no-message' before calling `write-region' unless it is
non-nil."
  (unless visit (setq visit 'no-message))
  (funcall #'write-region start end filename append visit lockname mustbenew))


(defun innit:squelch:load (file &optional noerror nomessage nosuffix must-suffix)
  "Silent version of `load'.

See `load' for details on args: FILE, NOERROR, NOMESSAGE, NOSUFFIX, MUST-SUFFIX

Ignore NOMESSAGE and call `load' with NOMESSAGE == t instead."
  (funcall #'load file noerror t nosuffix must-suffix))


(defun innit:squelch:message (&rest _)
  "Like `message', except it will ignore all inputs and do nothing."
  ;; Take message format string and args and I don't care.
  ;; We're being silent.
  nil)


(defun innit:squelch:standard-output (&rest _)
  "A character output stream to nowhere.

`standard-output' is generally t for having `print' output to echo area. Set
`standard-output' to this function to squelch it instead."
  ;; Take output stream character and who cares.
  ;; We're being silent.
  nil)


;;------------------------------------------------------------------------------
;; Squelch API
;;------------------------------------------------------------------------------

(defmacro innit:squelch (&rest forms)
  "Run FORMS without generating any output.

This silences calls to `message', `load', `write-region' and anything that
writes to `standard-output'. In interactive sessions this won't suppress writing
to *Messages*, only inhibit output in the echo area.

Originally from Doom's `quiet!' macro."
  `(if (innit:debug? :any)
       ;; Don't squelch if any true of: `innit:debug?', `debug-on-error', `init-file-debug'
       (progn ,@forms)
     ,(if innit:interactive?
          ;; Be less bossy; allow output to buffers but not the minibuffer.
          `(let ((inhibit-message t)
                 (save-silently t))
             (prog1
                 ,@forms
               ;; And we prevent output to the minibuffer by... Clearing
               ;; anything out of the minibuffer after it's already been output
               ;; to the minibuffer. Mission Accomplished.
               (message "")))
        ;; Full powered squelching; prevent output by just redefining commonly
        ;; chatty functions.
        `(cl-letf ((standard-output               #'innit:squelch:standard-output)
                  ((symbol-function message)      #'innit:squelch:message)
                  ((symbol-function load)         #'innit:squelch:load)
                  ((symbol-function write-region) #'innit:squelch:write-region))
           ,@forms))))


(defun innit:advice:squelch (fn &rest args)
  "Generic advice function for silencing noisy functions.

Lexically set some variables to squelch output, then call FN with ARGS,
returning FN's results.

In interactive Emacs, this just inhibits messages from appearing in the
minibuffer. They are still logged to *Messages*.

In tty Emacs, messages are suppressed completely.

Usage:
  (advice-add #'undo-tree-save-history :around #'innit:advice:squelch)

Originally from Doom's `doom-shut-up-a' function."
  (innit:squelch (apply fn args)))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :innit 'squelch)

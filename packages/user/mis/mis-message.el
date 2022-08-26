;;; mis-message.el --- Message output. -*- lexical-binding: t; -*-
;;
;; Author: Cole Brown <code@brown.dev>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:  2019-10-23
;; Modified: 2022-08-08
;;
;;; Commentary:
;;
;; Mis API for messages.
;;
;;; Code:


(require 'mis-error)
(require 'mis-valid)
(require 'mis-buffer)
(require 'mis-format)
(require 'mis-parse)

(require 'mis-align)
(require 'mis-style)
(require 'mis-art)

(require 'mis-comment)


;;------------------------------------------------------------------------------
;; Output Helpers
;;------------------------------------------------------------------------------

(defun mis:print:message (buffer message &rest args)
  "Output formatted/propertized MESSAGE & ARGS to BUFFER.

NOTE: Hacks/shenanigans for outputting propertized message/args to the
*Messages* buffer, which is normally plain strings only.

BUFFER should be nil, a string, or a few different keywords/symbols.
See `int<mis>:buffer:name'.

MESSAGE should be a string that `format' understands.

ARGS should be the `format' ARGS for MESSAGE."
  (let ((func/name (list 'mis caller))
        (output    (apply #'format message args)))
    (with-current-buffer (int<mis>:buffer:get-or-create func/name buffer)
      (save-mark-and-excursion
        ;; Special Shenanigans™ Part 01: *Messages*
        ;; Set `message-log-max' to nothing and then `message' so normal stuff
        ;; happens /EXCEPT/ for `output' getting printed to *Messages*.
        (when (eq (int<mis>:buffer:type func/name buffer) :messages)
          (let ((message-log-max nil))
            (message output)))

        (goto-char (point-max))

        ;; Special Shenanigans™ Part 02: Read-Only Buffers (e.g. *Messages*)
        ;; Ignore read-only status of buffer while we output to it. Need this to
        ;; be able to actually print our output to *Messages* with its properties
        ;; intact.
        (let ((inhibit-read-only t))
          ;; TODO: Test this to make sure we don't get extra newlines. It looks correct?
          (unless (zerop (current-column))
            (insert "\n"))
          (insert output "\n"))))))


(defun mis:print:strings (caller buffer strings)
  "Output formatted/propertized MESSAGES to current buffer.

BUFFER should be a buffer or string.

MESSAGES should be a list of strings.

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)"
  (let ((func/name (list 'mis caller)))
    (with-current-buffer (int<mis>:buffer:get-or-create func/name buffer)
      (save-mark-and-excursion
        (dolist (string strings)
          ;; Special Shenanigans™ Part 01: *Messages*
          ;; Set `message-log-max' to nothing and then `message' so normal stuff
          ;; happens /EXCEPT/ for STRING getting printed to *Messages*.
          (when (eq (int<mis>:buffer:type func/name buffer) :messages)
            (let ((message-log-max nil))
              (message string)))

          (goto-char (point-max))

          ;; Special Shenanigans™ Part 02: Read-Only Buffers (e.g. *Messages*)
          ;; Ignore read-only status of buffer while we output to it. Need this to
          ;; be able to actually print our output to *Messages* with its properties
          ;; intact.
          (let ((inhibit-read-only t))
            ;; TODO: Test this to make sure we don't get extra newlines. It looks correct?
            (unless (zerop (current-column))
              (insert "\n"))
            (insert string "\n")))))))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'mis-message)
;;; mis-message.el ends here

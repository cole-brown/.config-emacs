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
(require 'mis-format)
(require 'mis-style)


;;------------------------------------------------------------------------------
;; Output Helpers
;;------------------------------------------------------------------------------

(defun int<mis>:message:propertized/messages (message &rest args)
  "Output formatted/propertized MESSAGE & ARGS to *Messages* buffer.

Act like `message' but preserve MESSAGE's string properties /in the *Messages*
buffer/.

MESSAGE should be a string that `format' understands.

ARGS should be the `format' ARGS for MESSAGE."
  (let ((output (apply #'format message args)))
    ;; Send the OUTPUT string through `message' for minibuffer
    ;; display, but set `message-log-max' to `nil' so it _will not_ be output to
    ;; *Messages* buffer.
    (let ((message-log-max nil))
      (apply 'message output))

    ;; Now disable *Messages* buffer's read-only property so we can add the
    ;; properly OUTPUT at the end manually.
    (with-current-buffer (get-buffer "*Messages*")
      (save-mark-and-excursion
        (goto-char (point-max))
        (let ((inhibit-read-only t))
          ;; TODO: Test this to make sure we don't get extra newlines. It looks correct?
          (unless (zerop (current-column))
            (insert "\n"))
          (insert output "\n"))))))


(defun int<mis>:message:propertized/buffer (buffer message &rest args)
  "Output formatted/propertized MESSAGE & ARGS to BUFFER.

BUFFER should be a buffer or string.

MESSAGE should be a string that `format' understands.

ARGS should be the `format' ARGS for MESSAGE."
  (let ((output (apply #'format message args)))
    (with-current-buffer (get-buffer-create buffer)
      (save-mark-and-excursion
        (goto-char (point-max))
        ;; TODO: Test this to make sure we don't get extra newlines. It looks correct?
        (unless (zerop (current-column))
          (insert "\n"))
        (insert output "\n"))
      output)))


;;------------------------------------------------------------------------------
;; Output API
;;------------------------------------------------------------------------------

(defun mis (&rest args)
  "Output a message built from ARGS."
  ;; TODO TODO TODO
  )


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'mis-message)
;;; mis-message.el ends here

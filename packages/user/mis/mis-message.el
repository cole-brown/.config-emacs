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

(require 'mis-align)
(require 'mis-style)
(require 'mis-art)

(require 'mis-comment)


;;------------------------------------------------------------------------------
;; Output Helpers
;;------------------------------------------------------------------------------

;; TODO: delete?
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
      (message output))

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


;; TODO: delete?
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


(defun int<mis>:message:print (caller buffer strings)
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
          (goto-char (point-max))
          ;; Special Shenanigans™ Part 01: *Messages*
          ;; Set `message-log-max' to nothing and then `message' so normal stuff
          ;; happens /EXCEPT/ for STRING getting printed to *Messages*.
          (when (eq (int<mis>:buffer:type func/name buffer) :messages)
            (let ((message-log-max nil))
              (message string)))

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
;; Output API
;;------------------------------------------------------------------------------

;; TODO: Move this to "mis.el"?
(defun mis (&rest args)
  "Output a message built from ARGS."
  (let (style
        content
        output
        buffer)
    ;;------------------------------
    ;; Parsing & Validation
    ;;------------------------------
    ;; Check for validity, duplicate styles. Also look for any options we need
    ;; to skim off (e.g. `:buffer').
    (while args
      (let ((arg (pop args)))
        (cond ((listp arg)
               ;; Think this is a Mis syntax tree? Should be anyways.
               (let ((key   (nth 0 arg))
                     (value (nth 1 arg)))
                 ;; At this point, everything should be a Mis syntax tree. Error if not.
                 (int<mis>:valid:syntax? 'mis 'arg arg)

                 (if (eq :style key)
                     ;; Save off style or error.
                     (if style
                         (int<mis>:error 'mis
                                         '("Only one Mis `:style' allowed per level in ARGS. "
                                           "have: %S, found: %S, args: %S")
                                         style
                                         value
                                         args)
                       (setq style value))

                   ;; Save off this mis content type (messages, comments, arts...) in
                   ;; key, value order so it can be used as-is in next step. The pairs as
                   ;; a whole will be in reverse order but that'll be righted in
                   ;; finalization/printing.
                   (push value content)
                   (push key content))))

              ((keywordp arg)
               (let ((key arg)
                     (value (pop arg)))
                 ;; Validate top-level Mis arg.
                 (pcase key
                   (:buffer
                    (if buffer
                        (int<mis>:error 'mis
                                        '("Only one Mis `:buffer' allowed. "
                                          "have: %S, found: %S, args: %S")
                                        buffer
                                        value
                                        args)
                      ;; Validate value later (when used)?
                      (setq buffer value)))
                   (_
                    (int<mis>:error 'mis
                                    '("Unknown/unhandled args keyword/value pair!"
                                      "unknown keyword: %S, value: %S, args: %S")
                                    key
                                    value
                                    args)))))

              (t
               (int<mis>:error 'mis
                               '("Unknown/unhandled argument!"
                                 "unknown: %S, args: %S")
                               arg
                               args)))))

    ;;------------------------------
    ;; Compile Mis syntax into strings.
    ;;------------------------------
    ;; Compile each piece of `content' in current (reverse) order & push to
    ;; `output' list. Final step will then have `output' in forwards order.
    (while content
      (let ((key (pop content))
            (value (pop content)))
        ;; Find compiler for mis type.
        ;; Pass it the type's tree and the current style.
        (pcase key
          (:format
           (push (int<mis>:compile:format 'mis
                                          value
                                          style)
                 output))

          (_
           (int<mis>:error 'mis
                           '("Unhandled mis content type: %S "
                             "values: %S")
                           key
                           value)))))

    ;;------------------------------
    ;; Print Output
    ;;------------------------------
    ;; Now that we only have output left, switch to output buffer and print.
    (int<mis>:message:print 'mis
                            buffer
                            output)))
;; (mis
;;  (mis:style :width 80)
;;  (mis:line "-"))
;; (mis
;;   (mis:style :width 80)
;;   (mis:comment (mis:line "-"))
;;   (mis:comment :align 'center "Hello there.")
;;   (mis:comment (mis:line "-")))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'mis-message)
;;; mis-message.el ends here

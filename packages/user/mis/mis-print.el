;;; mis-print.el --- Output & Printing -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <code@brown.dev>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2022-09-15
;; Modified:   2022-09-15
;;
;;; Commentary:
;;
;;  Output & Printing
;;
;;; Code:


(require 'mis-error)
(require 'mis-valid)
(require 'mis-tree-output)
(require 'mis-tree-syntax)
(require 'mis-buffer)
;; (require 'mis-string)


;;------------------------------------------------------------------------------
;; Printing
;;------------------------------------------------------------------------------

(defun int<mis>:print:output/entry (caller buffer/type entry)
  "Print Mis Output Tree ENTRY string to current buffer.

BUFFER/TYPE should be a value from `int<mis>:buffer:type'.

ENTRY should be an alist with key/value conses.
  - valid keys: `:string', `:metadata'
NOTE: ENTRY should be one of the output alists of a full Mis Output Tree.
For example, if the full Mis Output Tree is:
  '((:output ((:string . \"foo\")  (:metadata (:bar . baz)))
             ((:string . \"zort\") (:metadata (:poit . narf)))))
Then ENTRY should be, e.g.:
  '((:string . \"foo\")  (:metadata (:bar . baz)))

Caller should be in correct position of correct buffer.

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)"
  (let* ((caller   (list 'int<mis>:print:output/entry caller))
         (string   (int<mis>:output:get/string caller entry))
         (metadata (int<mis>:output:get/metadata caller entry)))
    (int<mis>:debug caller
                    "print string: %S"
                    string)
    (int<mis>:debug caller
                    "print metadata: %S"
                    metadata)

    ;;------------------------------
    ;; No-Ops
    ;;------------------------------
    ;; Null string is ok; we do nothing.
    (cond ((null string)
           (int<mis>:debug caller
                           "print nothing == do nothing. string: %S"
                           string)
           nil)

          ;;------------------------------
          ;; Errors
          ;;------------------------------
          ;; Not a string is not ok now that null is checked.
          ((not (stringp string))
           (int<mis>:error caller
                           "Output string must be a string or nil. Got %S from %S"
                           string
                           entry))

          ;;------------------------------
          ;; Print String to Buffer
          ;;------------------------------
          (t
           (int<mis>:debug caller
                           "buffer/type: %S"
                           buffer/type)

           ;;---
           ;; Special Shenanigans™ Part 01: '*Messages*' Side-Effects
           ;;---
           ;; Set `message-log-max' to nothing and then `message' so normal stuff
           ;; happens /EXCEPT/ for STRING getting printed to '*Messages*'.
           (when (eq buffer/type :messages)
             (int<mis>:debug caller
                             "`:messages' shenanigans part 01: `message' for side-effects"
                             buffer/type)
             (let ((message-log-max nil))
               (message string))

             ;;---
             ;; Special Shenanigans™ Part 02: Append-Only Buffers (e.g. '*Messages*')
             ;;---
             ;; Only ever append to the *Messages* buffer.
             ;; TODO: An `:append' metadata or something to generalize this?
             (int<mis>:debug caller
                             "`:messages' shenanigans part 02: Go to end of '*Messages*' buffer")
             (goto-char (point-max)))

           ;;---
           ;; Special Shenanigans™ Part 03: Read-Only Buffers (e.g. '*Messages*')
           ;;---
           ;; Ignore read-only status of buffer while we output to it. Need this to
           ;; be able to actually print our output to *Messages* with its properties
           ;; intact.
           ;; TODO: "ignore/inhibit read only" metadata prop?
           (int<mis>:debug caller
                           "`:messages' shenanigans part 03: force inhibit read-only? %S & %S == %S"
                           (eq buffer/type :messages)
                           buffer-read-only
                           (and (eq buffer/type :messages)
                                buffer-read-only))
           (let ((inhibit-read-only (if (and (eq buffer/type :messages)
                                             buffer-read-only)
                                        t
                                      inhibit-read-only)))
             (int<mis>:debug caller
                             "insert string...")
             (unless (zerop (current-column))
               (insert "\n"))
             ;; TODO: Test this to make sure we don't get extra newlines. It looks correct?
             ;; (insert string "\n")
             (insert string))))))


(defun int<mis>:print:output (caller metadata output)
  "Finalize and print strings in Mis OUTPUT Tree to the output buffer.

OUTPUT should be a Mis Output Tree.

METADATA should be nil or a Mis Output Tree of any top-level metadata (like the
output buffer settings).

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)"
  (let* ((caller  (list 'int<mis>:print:output caller))
         (buffer/object (int<mis>:output/metadata:find caller :buffer:object metadata))
         (buffer/name   (int<mis>:output/metadata:find caller :buffer:name   metadata))
         (buffer/type   (int<mis>:buffer:type          caller buffer/name)))

    (int<mis>:debug caller
                    "current-buffer (before): %S"
                    (current-buffer))
    (int<mis>:debug caller
                    '("print to:\n"
                      "  buffer/name:   %S\n"
                      "  buffer/type:   %S\n"
                      "  buffer/object: %S")
                    buffer/name
                    buffer/type
                    buffer/object)

    ;; Enter buffer & save mark/excursion once, then print each string.
    (with-current-buffer buffer/object
      (save-mark-and-excursion
        (int<mis>:debug caller
                        "current-buffer (printing): %S"
                        (current-buffer))
        ;; Loop on the output strings/metadatas to finalize & print.
        (dolist (entry (int<mis>:output:get/entries caller output))
          (int<mis>:print:output/entry caller
                                       buffer/type
                                       entry))))))


(defun int<mis>:print (caller metadata list/output)
  "Finalize and print strings in list of Mis Output Trees LIST/OUTPUT.

LIST/OUTPUT should be nil or a list of Mis Output Trees.

METADATA should be nil or a Mis Output Tree of any top-level metadata (like the
output buffer settings).

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)"
  (let* ((caller        (list 'int<mis>:print caller))
         (buffer/object (int<mis>:output/metadata:find caller :buffer:object metadata))
         (buffer/name   (int<mis>:output/metadata:find caller :buffer:name   metadata))
         (buffer/type   (int<mis>:buffer:type          caller buffer/name)))

    (int<mis>:debug caller
                    "metadata:    %S"
                    metadata)
    (int<mis>:debug caller
                    "list/output: %S"
                    list/output)

    (int<mis>:debug caller
                    "current-buffer (before): %S"
                    (current-buffer))
    (int<mis>:debug caller
                    '("print to:\n"
                      "  buffer/name:   %S\n"
                      "  buffer/type:   %S\n"
                      "  buffer/object: %S")
                    buffer/name
                    buffer/type
                    buffer/object)

    ;; Enter buffer & save mark/excursion once, then print each string.
    (with-current-buffer buffer/object
      (save-mark-and-excursion
        (int<mis>:debug caller
                        "current-buffer (printing): %S"
                        (current-buffer))
        ;; Loop on the list of output trees.
        (dolist (output list/output)
          ;; Loop on this output tree's entries of strings/metadatas to finalize & print.
          (dolist (entry (int<mis>:output:get/entries caller output))
            (int<mis>:print:output/entry caller
                                         buffer/type
                                         entry)))))))


;;------------------------------------------------------------------------------
;; Printing APIs
;;------------------------------------------------------------------------------

;; TODO: Use `int<mis>:print'?
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


;; TODO: Use `int<mis>:print'?
(defun mis:print:strings (caller buffer strings)
  "Output formatted/propertized MESSAGES to current buffer.

BUFFER should be a buffer or string.

STRINGS should be a list of strings.

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
(provide 'mis-print)
;;; mis-print.el ends here

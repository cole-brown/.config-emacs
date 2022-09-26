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

(defun int<mis>:print:string (caller output)
  "Print OUTPUT to current buffer.

OUTPUT should be an alist with key/value conses.
  - valid keys: `:string', `:metadata'
NOTE: OUTPUT should be one of the output alists of a full Mis Output Tree.
For example, if the full Mis Output Tree is:
  '((:output ((:string . \"foo\")  (:metadata (:bar . baz)))
             ((:string . \"zort\") (:metadata (:poit . narf)))))
Then OUTPUT should be, e.g.:
  '((:string . \"foo\")  (:metadata (:bar . baz)))

Caller should be in correct position of correct buffer.

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)"
  (let* ((caller (list 'int<mis>:print:string caller))
         (string (int<mis>:output:get/string caller output))
         (metadata (int<mis>:output:get/metadata caller output)))
    ;;------------------------------
    ;; No-Ops
    ;;------------------------------
    ;; Null string is ok; we do nothing.
    (cond ((null string)
           nil)

          ;;------------------------------
          ;; Errors
          ;;------------------------------
          ;; Not a string is not ok now that null is checked.
          ((not (stringp string))
           (int<mis>:error caller
                           "Output string must be a string or nil. Got %S from %S"
                           string
                           output))

          ;;------------------------------
          ;; Print!
          ;;------------------------------
          (t
           ;;------------------------------
           ;; TODO: Finalize String
           ;;------------------------------
           ;; TODO: string alignment and other finalization? or is that done elsewhere?

           ;;------------------------------
           ;; Print String to Buffer
           ;;------------------------------
           ;; Special Shenanigans™ Part 01: *Messages*
           ;; Set `message-log-max' to nothing and then `message' so normal stuff
           ;; happens /EXCEPT/ for STRING getting printed to *Messages*.
           (when (eq (int<mis>:buffer:type caller buffer) :messages)
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
             (insert string "\n"))))))


(defun int<mis>:print (caller output metadata)
  "Finalize and print strings in Mis OUTPUT Tree to the output buffer.

OUTPUT should be a Mis Output Tree.

METADATA should be nil or an alist of any top-level metadata (like the output
buffer settings).

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)"
  (let* ((caller  (list 'int<mis>:print caller))
         ;; buffer name string or buffer object or nil
         (buffer/meta (int<mis>:metadata:get/value caller :buffer metadata))
         ;; buffer object
         (buffer (int<mis>:buffer:get-or-create caller buffer)))

    ;; Enter buffer & save mark/excursion once, then print each string.
    (with-current-buffer buffer
      (save-mark-and-excursion
        ;; Loop on the output strings/metadatas to finalize & print.
        (dolist (output (int<mis>:output:get/entries caller tree))
          (int<mis>:print:string caller output))))))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'mis-print)
;;; mis-print.el ends here

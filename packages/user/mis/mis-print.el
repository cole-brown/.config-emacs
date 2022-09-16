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
(require 'mis-parse)
(require 'mis-buffer)
(require 'mis-string)


;;------------------------------------------------------------------------------
;; Output Trees
;;------------------------------
;; These trees are formatted as an alist with key/value cons:
;;   - key: `:output'
;;   - value:
;;     - list of alists with key/value conses
;;     - valid keys: `:string', `:metadata'
;;
;; Example
;; '((:output ((:string . "foo") (:metadata . ...))
;;            ((:string . "bar") (:metadata . ...))))
;;
;;------------------------------------------------------------------------------

(defun int<mis>:output:get/outputs (caller output)
  "Return the list-of-alists from Mis OUTPUT Tree.

Return value is a list of alists:
 '(((:string . \"foo\") (:metadata . [...]))
   ((:string . \"bar\") (:metadata . [...])))
Each alist in return value has keys: `:string' and `:metadata'

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)"
  (alist-get :output output))
;; (int<mis>:output:get/outputs 'test '((:output ((:string . "foo") (:metadata . :foo)) ((:string . "bar") (:metadata . :bar)))))


(defun int<mis>:output:get/string (caller output)
  "Return output string from OUTPUT alist.

OUTPUT should be an alist with key/value conses.
  - valid keys: `:string', `:metadata'

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)"
  (alist-get :string output))
;; (int<mis>:output:get/string 'test '((:string . "foo") (:metadata . :foo)))


(defun int<mis>:output:get/metadata (caller output)
  "Return output string from OUTPUT alist.

OUTPUT should be an alist with key/value conses.
  - valid keys: `:string', `:metadata'

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)"
  (alist-get :metadata output))
;; (int<mis>:output:get/metadata 'test '((:string . "foo") (:metadata . :foo)))


(defun int<mis>:output:create (caller string &rest metadata)
  "Create a Mis Output Tree from STRING and METADATA(s).

STRING should be nil or a string.

Each METADATA should nil or a cons of a keyword and... some value.

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)"
  (let* ((caller (list 'int<mis>:output:create caller)))
    ;;------------------------------
    ;; Error Checks
    ;;------------------------------
    (int<mis>:valid:string-or-nil? caller
                                   'string
                                   string)

    (when (and (not (null metadata))
               (not (seq-every-p (lambda (m) "Validate METADATA param."
                                   (and
                                    ;; Is a cons but is not a list?
                                    (and (listp m)
                                         (cdr m)
                                         (atom (cdr m)))
                                    ;; Key is a keyword?
                                    (keywordp (car m))))
                                 metadata)))
      (int<mis>:error caller
                      "Mis Output Metadata must be nil or a cons alist. Got %S: %S"
                      (type-of metadata)
                      metadata))

    ;;------------------------------
    ;; Create the Mis Output Tree
    ;;------------------------------
    ;; Alist of `:output' to list of alists...
    (list (cons :output
                ;; List of alists...
                (list
                 ;; Alist with `:string' & `:metadata' keys, validated values.
                 (list (cons :string string)
                       (cons :metadata metadata)))))))
;; (int<mis>:output:create 'test "foo" '(:buffer . "bar") '(:align . baz))
;; (int<mis>:valid:output? 'test 'output (int<mis>:output:create 'test "foo" '(:buffer . "bar") '(:align . baz)))


(defun int<mis>:output:append (caller output string &rest metadata)
  "Append STRING & METADATA to Mis OUTPUT Tree.

STRING should be nil or a string.

Each METADATA should nil or a cons of a keyword and... some value.

OUTPUT should be a Mis Output Tree (can be nil). It will be updated and
the updated value returned. Caller should set the return value back to the input
arg as the update is not guaranteed to be in-place.
Example:
  (setq output (int<mis>:output:append 'test
                                       output
                                       \"New output string!\"))

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)"
  (let ((caller (list 'int<mis>:output:append caller)))
    ;;------------------------------
    ;; Error Checks
    ;;------------------------------
    (int<mis>:valid:output? caller
                            'output
                            output)
    (int<mis>:valid:string-or-nil? caller
                                   'string
                                   string)

    (when (and (not (null metadata))
               (not (seq-every-p (lambda (m) "Validate METADATA param."
                                   (and
                                    ;; Is a cons but is not a list?
                                    (and (listp m)
                                         (cdr m)
                                         (atom (cdr m)))
                                    ;; Key is a keyword?
                                    (keywordp (car m))))
                                 metadata)))
      (int<mis>:error caller
                      "Mis Output Metadata must be nil or a cons alist. Got %S: %S"
                      (type-of metadata)
                      metadata))

    ;;------------------------------
    ;; Append to OUTPUT
    ;;------------------------------
    (let ((outputs (int<mis>:output:get/outputs :output output))) ;; list of alists
      ;; Update OUTPUT.
      (setf (alist-get :output output)
            ;; Add to the end of the list of alists.
            (append outputs
                    (list ;; list of...
                     (list (cons :string string) ;; alist of string & metadata
                           (cons :metadata metadata)))))
      ;; And return updated OUTPUT.
      output)))
;; (int<mis>:output:append 'test
;;                         (int<mis>:output:create 'test "foo" '(:buffer . "bar") '(:align . baz))
;;                         "zort"
;;                         (cons :poit 'narf))


(defun int<mis>:output:create-or-append (caller output string &rest metadata)
  "Create or append to the Mis OUTPUT Tree, depending on if it exists currently.

Will call either `int<mis>:output:create' or `int<mis>:output:append'.

STRING should be nil or a string.

Each METADATA should nil or a cons of a keyword and... some value.

OUTPUT should be a Mis Output Tree (can be nil). It will be updated and
the updated value returned. Caller should set the return value back to the input
arg as the update is not guaranteed to be in-place.
Example:
  (setq output (int<mis>:output:create-or-append 'test
                                                 output
                                                 \"hello there\"
                                                 '(:align . :center)))

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)"
  (apply (if output
             #'int<mis>:output:update
           #'int<mis>:output:create)
         (list 'int<mis>:output:create-or-append caller)
         output
         string
         metadata))


(defun int<mis>:output (caller string style/complete)
  "Create a Mis Output Tree from STRING & style data in SYNTAX and STYLE.

STRING should be a formatted/propertized/etc string for outputting.

STYLE/COMPLETE should be nil or a `:style' Mis Syntax Tree. It should be the
full & complete styling tree for STRING.

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)"
  (let ((caller (list 'int<mis>:output caller))
        metadata) ;; alist
    (int<mis>:debug caller
                    "string:         %S"
                    string)
    (int<mis>:debug caller
                    "style/complete: %S"
                    style/complete)

    ;; Check each styling keyword in `style/complete' to see if it's metadata-worthy.
    (dolist (kvp (int<mis>:syntax:get/value caller :style style/complete))
      (int<mis>:debug caller
                        "metadata kvp:   %S"
                        kvp)
      (when (memq (car kvp) int<mis>:keywords:metadata)
        (int<mis>:debug caller
                        "metadata kvp:   %S"
                        kvp)
        (push kvp metadata)))

    ;; Create the Mis Output Tree.
    (apply #'int<mis>:output:create
           caller
           string
           metadata)))
;; (int<mis>:output 'test "hello there" (mis:style :width 42 :align 'center))


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
        (dolist (output (int<mis>:output:get/outputs caller tree))
          (int<mis>:print:string caller output))))))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'mis-print)
;;; mis-print.el ends here

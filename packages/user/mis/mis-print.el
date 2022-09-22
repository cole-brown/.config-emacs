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
(require 'mis-compile)

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
  "Return output metadata from OUTPUT alist.

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
;; (int<mis>:output:get/metadata
;;  'test
;;  (nth 0 (int<mis>:output:get/outputs 'test
;;                                      (int<mis>:output:create 'test "this" '(:buffer . "foo") '(:align . lawful-good)))))


(defun int<mis>:output:update/metadata (caller existing new)
  "Update/overwrite EXISTING metadata with NEW metatada.

EXISTING and NEW should be nil or a Mis Output Tree.

Return updated EXISTING Mis Output Tree. Caller should save the return value as
the update is not guaranteed to be in-place.
Example:
  (setq metadata/existing (int<mis>:output:update/metadata 'example
                                                            metadata/existing
                                                            metadata/new))

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)"
  (let ((caller (list 'int<mis>:output:update/metadata)))
    (dolist (each/new new)
      (let ((key (car each/new))
            (value (cdr each/new)))
        ;; Add/overwrite NEW's key/value to EXISTING.
        (setf (alist-get key existing) value)))

    existing))
;; (int<mis>:output:update/metadata
;;  'test
;;  (int<mis>:output:get/metadata
;;   'test
;;   (nth 0 (int<mis>:output:get/outputs 'test
;;                                       (int<mis>:output:create 'test "this" '(:buffer . "foo") '(:align . lawful-good)))))
;;  (int<mis>:output:get/metadata
;;   'test
;;   (nth 0 (int<mis>:output:get/outputs 'test
;;                                       (int<mis>:output:create 'test "also this" '(:align . center))))))
;;
;; (int<mis>:output:update/metadata
;;  'test
;;  (int<mis>:output:get/metadata
;;   'test
;;   (nth 0 (int<mis>:output:get/outputs 'test
;;                                       (int<mis>:output:create 'test "this" '(:buffer . "foo") '(:align . lawful-good)))))
;;  nil)
;;
;; (int<mis>:output:update/metadata
;;  'test
;;  nil
;;  (int<mis>:output:get/metadata
;;   'test
;;   (nth 0 (int<mis>:output:get/outputs 'test
;;                                       (int<mis>:output:create 'test "also this" '(:align . center))))))


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


(defun int<mis>:output:append (caller existing new)
  "Append Mis Output Tree NEW onto Mis Output Tree EXISTING.

EXISTING and NEW should both be either nil or a Mis Output Tree.

Return updated EXISTING Mis Output Tree. Caller should set the return value back
to the input arg as the update is not guaranteed to be in-place.
Example:
  (setq output/existing (int<mis>:output:append 'example
                                                output/existing
                                                output/new))

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)"
  (let* ((caller (list 'int<mis>:output:append caller))
         ;; Make 'em backwards so we can append and un-backwards at the end.
         (existing/entries (nreverse (int<mis>:output:get/outputs caller existing))))
    (int<mis>:debug caller "existing: %S" existing)
    (int<mis>:debug caller "new:      %S" new)

    ;;------------------------------
    ;; Null Checks
    ;;------------------------------
    (cond ((null existing)
           new)

          ((null new)
           existing)

          ;; Both exist so actually append them.
          (t
           ;;------------------------------
           ;; Error Checks
           ;;------------------------------
           (int<mis>:valid:output? caller 'existing existing)
           (int<mis>:valid:output? caller 'new      new)

           ;;------------------------------
           ;; Append & Return
           ;;------------------------------
           ;; Append the new MOT's outputs to the existing one's.
           (dolist (entry/new (int<mis>:output:get/outputs caller new))
             (push entry/new existing/entries))

           ;; Recombobulate return value.
           (int<mis>:output:from-entries caller
                                         (nreverse existing/entries))))))
;; (int<mis>:output:append 'test nil '((:output ((:string . \"foo-0\" ) (:metadata (:bar:0  . baz0 ))))))
;; (int<mis>:output:append 'test '((:output ((:string . \"foo-0\" ) (:metadata (:bar:0  . baz0 ))))) nil)
;; (int<mis>:output:append 'test
;;                         '((:output
;;                            ((:string . \"foo-0\" ) (:metadata (:bar:0  . baz0 )))
;;                            ((:string . \"foo-1\" ) (:metadata (:bar:1  . baz1 )))
;;                            ((:string . \"foo-2\" ) (:metadata (:bar:2  . baz2 )))))
;;                         '((:output
;;                            ((:string . \"narf\") (:metadata (:zort . poit)))
;;                            ((:string . \"egad\") (:metadata (:troz . fiddely-posh))))))


(defun int<mis>:output:from-entries (caller entries)
  "Create a Mis Output Tree from Mis Output Tree ENTRIES.

Each entry in ENTRIES should be an alist with `:string' and `:metadata' keys:
  ((:string . \"foo\") (:metadata . [...]))

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)"
  (let ((caller (list 'int<mis>:output:from-entries caller)))
    (list (cons :output
                entries))))
;; (int<mis>:output:from-entries 'test
;;                               '(((:string . \"foo\")  (:metadata (:bar . baz)))
;;                                 ((:string . \"zort\") (:metadata (:poit . narf)))))


(defun int<mis>:output (caller string style/complete &optional metadata)
  "Create a Mis Output Tree from STRING & style data in SYNTAX and STYLE.

STRING should be a formatted/propertized/etc string for outputting.

STYLE/COMPLETE should be nil or a `:style' Mis Syntax Tree. It should be the
full & complete styling tree for STRING.

METADATA should be nil or a Mis Output Tree node's metadata alist.

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)"
  (let ((caller (list 'int<mis>:output caller)))
    (int<mis>:debug caller
                    "string:         %S"
                    string)
    (int<mis>:debug caller
                    "style/complete: %S"
                    style/complete)
    (int<mis>:debug caller
                    "metadata:       %S"
                    metadata)

    ;; Check each styling keyword in `style/complete' to see if it's metadata-worthy.
    ;; If so, (over)write to METADATA alist.
    (dolist (kvp (int<mis>:syntax:get/value caller :style style/complete))
      (int<mis>:debug caller
                        "metadata kvp:   %S"
                        kvp)
      (let ((key   (car kvp))
            (value (cdr kvp)))
        (int<mis>:debug caller
                        "metadata key:   %S"
                        key)
        (int<mis>:debug caller
                        "metadata value: %S"
                        value)
        (when (memq key int<mis>:keywords:metadata)
          (setf (alist-get key metadata) value))))

    ;; Create the Mis Output Tree.
    (apply #'int<mis>:output:create
           caller
           string
           metadata)))
;; (int<mis>:output 'test "hello there" (mis:style :width 42 :align 'center))
;; (int<mis>:output 'test "hello there" (mis:style :width 42 :align 'center) '((:width . 11) (:align . left)))


;;------------------------------------------------------------------------------
;; Finalize / Reduce
;;------------------------------------------------------------------------------

(defun int<mis>:output:reduce (caller output)
  "Reduce the Mis OUTPUT Tree into a single string/metadata MOT.

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)"
  (let* ((caller (list 'int<mis>:compile:output caller))
         entry/reduced
         string/reduced
         metadata/reduced)
    (int<mis>:debug caller
                    "output:       %S"
                    output)

    ;;------------------------------
    ;; Reduce Outputs
    ;;------------------------------
    (dolist (entry (int<mis>:output:get/outputs caller output))
      (int<mis>:debug caller
                      "entry:        %S"
                      entry)
      (let ((string/entry   (int<mis>:output:get/string caller entry))
            (metadata/entry (int<mis>:output:get/metadata caller entry)))
        (int<mis>:debug caller
                        "string/entry: %S"
                        string/entry)
        (int<mis>:debug caller
                        "meta/entry:   %S"
                        metadata/entry)
        ;; Combine strings.
        (unless (or (null string/entry)
                    (string-empty-p string/entry))
          (setq string/reduced
                (if (null string/reduced)
                    string/entry
                  (concat string/reduced string/entry)))
          (int<mis>:debug caller
                          "concat:       %S"
                          string/reduced))
        ;; Combine metadatas.
        (setq metadata/reduced
              (int<mis>:output:update/metadata caller metadata/reduced metadata/entry))
        (int<mis>:debug caller
                        "meta:        %S"
                        metadata/reduced)))

    ;;------------------------------
    ;; Return reduced as a MOT
    ;;------------------------------
    (let ((output/reduced (apply #'int<mis>:output:create
                                 caller
                                 string/reduced
                                 (nreverse metadata/reduced))))
      (int<mis>:debug caller
                      "<--output:    %S"
                      output/reduced)
      output/reduced)))
;; (int<mis>:output:reduce 'test
;;                         '((:output
;;                            ((:string . "foo-0") (:metadata (:bar:0 . baz0)))
;;                            ((:string . "foo-1") (:metadata (:bar:1 . baz1)))
;;                            ((:string . "foo-2") (:metadata (:bar:2 . baz2)))
;;                            ((:string . "narf") (:metadata (:zort . poit)))
;;                            ((:string . "egad") (:metadata (:troz . fiddely-posh))))))


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

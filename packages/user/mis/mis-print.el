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
(require 'mis-style)


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
  (let* ((caller (list 'int<mis>:output:append caller)))
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
  "Create or append to the Mis OUTPUT tree, depending on if it exists currently.

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







;;------------------------------------------------------------------------------
;; Output Metadata
;;------------------------------------------------------------------------------

(defun int<mis>:metadata:get/value  (caller key metadata &optional default)
  "Get KEY's value from Mis METADATA alist.

KEY should be:
  - a metadata keyword  - From `int<mis>:keywords:metadata`
  - a temporary keyword - Starts with `:tmp:`

SYNTAX should be a Mis Syntax Tree.

Optional DEFAULT can be a value to return if KEY is not present.

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)"
  (alist-get key metadata default))
;; (int<mis>:metadata:get/value 'test :buffer '((:buffer . "test-buffer-name")))
;; (int<mis>:metadata:get/value 'test :buffer (int<mis>:output:get/metadata 'test '((:string . "foo") (:metadata (:buffer . "test-buffer-name")))))


;;------------------------------------------------------------------------------
;; Finalizing
;;------------------------------------------------------------------------------



;;------------------------------------------------------------------------------
;; Printing
;;------------------------------------------------------------------------------

(defun int<mis>:print:string (caller output)
  "Print OUTPUT to current buffer.

OUTPUT should be an alist with key/value conses.
  - valid keys: `:string', `:metadata'

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
           ;; Finalize String
           ;;------------------------------
           ;; TODO: string alignment?

           ;;------------------------------
           ;; Print String to Buffer
           ;;------------------------------
           (dolist (string strings)
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
               (insert string "\n")))))))


(defun int<mis>:print (caller tree metadata)
  "Finalize and print strings in TREE to the output buffer.

TREE should be a Mis Output Tree.

METADATA should be nil or an alist of any top-level metadata (like output buffer
settings).

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

    (with-current-buffer (int<mis>:buffer:get-or-create caller buffer)
      (save-mark-and-excursion
        ;; Loop on the output strings/metadatas to finalize & print.
        (dolist (output (int<mis>:output:get/outputs caller tree))

          ;; TODO: Finalize string.

          ;; TODO: Print string.

          )
        ))))


;;------------------------------------------------------------------------------
;; Output API
;;------------------------------------------------------------------------------

(defun mis (&rest args)
  "Output a message built from ARGS."
  (let (styling
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
        ;; Is this non-nil and /only/ styling info? Then it's global styling for
        ;; use in all of the things here.
        (cond ((and arg
                    (int<mis>:style:exclusive? arg))
               ;; Make sure it's valid...ish.
               (int<mis>:valid:syntax? 'mis 'arg arg)

               (if styling
                   ;; There can be only one.
                   ;;
                   ;; NOTE: We could allow more than one and merge, but then
                   ;; we'd have to do collision detection...
                   (int<mis>:error 'mis
                                   '("Only one Mis `:style' allowed per level in ARGS. "
                                     "have: %S, found: %S, args: %S")
                                   styling
                                   value
                                   args)
                 ;; Save the styling.
                 (setq styling arg)))

              ((listp arg)
               ;; Think this is a Mis syntax tree? Should be anyways; error if not.
               (int<mis>:valid:syntax? 'mis 'arg arg)
               ;; Save off this mis content type (messages, comments, arts...).
               (push arg content))

              ;; Top-level arg? Example: `:buffer'
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
                   ;; TODO: Other top-level keywords?
                   (_
                    (int<mis>:error 'mis
                                    '("Unknown/unhandled args keyword/value pair!"
                                      "unknown keyword: %S, value: %S, args: %S")
                                    key
                                    value
                                    args)))))

              ;; Fallthrough: Error!
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
      (int<mis>:debug 'mis
                      "compile: %S"
                      (nth 0 content))
      ;; Get a syntax tree; turn into a string.
      (push (int<mis>:compile 'mis
                              (pop content)
                              styling)
            output))

    (int<mis>:debug 'mis
                    "compiled output: %S"
                    output)

    ;;------------------------------
    ;; Print Output
    ;;------------------------------
    ;; Now that we only have output left, switch to output buffer and print.
    (mis:print:strings 'mis
                       buffer
                       output)))
;; (mis
;;  (mis:style :width 80)
;;  (mis:line "-"))
;; (mis (mis:comment (mis:line "-")))
;; (mis
;;   (mis:style :width 80)
;;   (mis:comment (mis:line "-"))
;;   (mis:comment "Hello there.")
;;   (mis:comment (mis:line "-")))
;; TODO-mis: YOU ARE HERE:
;; (mis (mis:comment (mis:string :align 'center "Hello there.")))
;; (mis
;;   (mis:style :width 80)
;;   (mis:comment (mis:line "-")
;;                (mis:string :align 'center "Hello there.")
;;                (mis:line "-")))
;; (mis
;;   (mis:comment :width 80
;;                (mis:line "-")
;;                (mis:string :align 'center "Hello there.")
;;                (mis:line "-")))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'mis-print)
;;; mis-print.el ends here

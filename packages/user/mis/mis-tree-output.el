;;; mis-tree-output.el --- Mis Output Tree Functions -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <code@brown.dev>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2022-09-26
;; Modified:   2022-09-26
;;
;;; Commentary:
;;
;;  Mis Output Tree Functions
;;
;;; Code:


(require 'mis-error)
(require 'mis-valid)
(require 'mis-int-string)
(require 'mis-tree-syntax)


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

(defun int<mis>:output:get/entries (caller output)
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
;; (int<mis>:output:get/entries 'test '((:output ((:string . "foo") (:metadata . :foo)) ((:string . "bar") (:metadata . :bar)))))


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
;;  (nth 0 (int<mis>:output:get/entries 'test
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
;;   (nth 0 (int<mis>:output:get/entries 'test
;;                                       (int<mis>:output:create 'test "this" '(:buffer . "foo") '(:align . lawful-good)))))
;;  (int<mis>:output:get/metadata
;;   'test
;;   (nth 0 (int<mis>:output:get/entries 'test
;;                                       (int<mis>:output:create 'test "also this" '(:align . center))))))
;;
;; (int<mis>:output:update/metadata
;;  'test
;;  (int<mis>:output:get/metadata
;;   'test
;;   (nth 0 (int<mis>:output:get/entries 'test
;;                                       (int<mis>:output:create 'test "this" '(:buffer . "foo") '(:align . lawful-good)))))
;;  nil)
;;
;; (int<mis>:output:update/metadata
;;  'test
;;  nil
;;  (int<mis>:output:get/metadata
;;   'test
;;   (nth 0 (int<mis>:output:get/entries 'test
;;                                       (int<mis>:output:create 'test "also this" '(:align . center))))))


(defun int<mis>:output:create/entry (caller string &rest metadata)
  "Create a Mis Output Tree Entry from STRING and METADATA(s).

STRING should be nil or a string.

Each METADATA should nil or a cons of a keyword and... some value.

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)"
  (let* ((caller (list 'int<mis>:output:create/entry caller)))
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
    ;; Create the MOT entry.
    ;;------------------------------
    ;; Alist with `:string' & `:metadata' keys, validated values.
    (list (cons :string string)
          (cons :metadata metadata))))
;; (int<mis>:output:create/entry 'test "foo" '(:buffer . "bar") '(:align . baz) '(:width . 11))


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
                 (apply #'int<mis>:output:create/entry
                        caller
                        string
                        metadata))))))
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
         (existing/entries (nreverse (int<mis>:output:get/entries caller existing))))
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
           (dolist (entry/new (int<mis>:output:get/entries caller new))
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
;; Mis Output Tree String Functions
;;------------------------------------------------------------------------------

(defun int<mis>:output/string:affix (caller prefix postfix entry)
  "Attach PREFIX and POSTFIX to Mis Output Tree ENTRY's string.

PREFIX and POSTFIX should be strings.

ENTRY should be a Mis Output Tree Entry, which is an alist with keys:
  - `:string'
  - `:metadata'

Return a Mis Output Tree with affixed string(s).

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)"
  (let* ((caller   (list 'int<mis>:output/string:affix caller))
         (string   (int<mis>:output:get/string caller entry))
         (metadata (int<mis>:output:get/metadata caller entry))
         (align    (alist-get :align metadata))
         (width    (alist-get :width metadata)))

    ;;------------------------------
    ;; Error Checks
    ;;------------------------------
    ;; We refuse to mess with nil, etc.
    (int<mis>:valid:string? caller :string  string)
    (int<mis>:valid:string? caller 'prefix  prefix)
    (int<mis>:valid:string? caller 'postfix postfix)

    ;; If they are aligned, they've need to have been given a width.
    (when align
      (int<mis>:valid:positive-integer? caller :width width))

    ;;------------------------------
    ;; Alignment-Aware Prefix & Postfix
    ;;------------------------------
    ;; Deal with string according to metadata, then return a new MOT entry.
    (cond
     ;;---
     ;; Special Cases:
     ;;---

     ;; Empty String: Who cares what the metadata says?
     ((string-empty-p string)
      (apply #'int<mis>:output:create
             caller
             (int<mis>:string:affix prefix postfix string)
             metadata))

     ;;---
     ;; Left/Center/Right Cases:
     ;;---
     ;; Carve out space for prefix/postfix in the string based on the alignment.

     ((eq align 'left)
      (let* ((prefix/length  (length prefix))
             (postfix/length (length postfix))
             (string/length  (length string))
             (total/length   (+ prefix/length string/length postfix/length)))
        ;; No room on left; if we need to trim down, do it all from the end of the string.
        (if (> total/length width)
            (apply #'int<mis>:output:create
                   caller
                   (int<mis>:string:affix prefix
                                          postfix
                                          (substring string 0 (- width prefix/length postfix/length)))
                   metadata)
          ;; No trim necessary; have the space to just affix.
          (apply #'int<mis>:output:create
                 caller
                 (int<mis>:string:affix prefix
                                        postfix
                                        string)
                 metadata))))

     ((eq align 'center)
      (let* ((prefix/length  (length prefix))
             (postfix/length (length postfix)))
        ;; Trim down each side by the length of the string being affixed there.
        (apply #'int<mis>:output:create
               caller
               (int<mis>:string:affix prefix
                                      postfix
                                      (substring string
                                                 prefix/length
                                                 (- width postfix/length)))
               metadata)))

     ((eq align 'right)
      (let* ((prefix/length  (length prefix))
             (postfix/length (length postfix))
             (string/length  (length string))
             (total/length   (+ prefix/length string/length postfix/length)))
        ;; No room on right; if we need to trim down, do it all from the beginning of the string.
        (if (> total/length width)
            (apply #'int<mis>:output:create
                   caller
                   (int<mis>:string:affix prefix
                                          postfix
                                          (substring string
                                                     (+ prefix/length postfix/length)
                                                     width))
                   metadata)
          ;; No trim necessary; have the space to just affix.
          (apply #'int<mis>:output:create
                 caller
                 (int<mis>:string:affix prefix
                                        postfix
                                        string)
                 metadata))))

     ;;---
     ;; No alignment: Nothing special to do.
     ;;---
     (t
      (apply #'int<mis>:output:create
             caller
             (int<mis>:string:affix prefix postfix string)
             metadata)))))
;; (int<mis>:output/string:affix 'test
;;                               ";; " ""
;;                               '((:string . "    hello there     ") (:metadata (:align . center) (:width . 20))))
;; from: '((:output ((:string . "    hello there     ") (:metadata (:align . center) (:width . 20)))))
;; to:   '((:output ((:string . ";;  hello there     ") (:metadata (:align . center) (:width . 20)))))


;;------------------------------------------------------------------------------
;; Finalize
;;------------------------------------------------------------------------------

(defun int<mis>:output:finalize/lines (caller output)
  "Reduce the Mis OUTPUT Tree into a single string/metadata MOT per line.

Will split any strings with newlines up so that return value has only one line
per MOT entry string.

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)"
  (let* ((caller (list 'int<mis>:output:finalize/lines caller))
         string/line
         metadata/line
         output/line)
    (int<mis>:debug caller
                    "output:            %S"
                    output)

    ;;------------------------------
    ;; Finalize Outputs
    ;;------------------------------
    (dolist (entry (int<mis>:output:get/entries caller output))
      (int<mis>:debug caller
                      '("\n========="
                        "\n==ENTRY=="))
      (int<mis>:debug caller
                      "entry:             %S"
                      entry)
      (let* ((string/entry       (int<mis>:output:get/string caller entry))
             (metadata/entry     (int<mis>:output:get/metadata caller entry))
             (string/entry/lines (int<mis>:string:lines/split string/entry))
             (length             (length string/entry/lines)))
        (int<mis>:debug caller
                        "string/entry:      %S"
                        string/entry)
        (int<mis>:debug caller
                        "meta/entry:        %S"
                        metadata/entry)

        ;;------------------------------
        ;; Explode each entry into its lines.
        ;;------------------------------
        (dotimes (i length)
          (let ((string/entry/line   (nth i string/entry/lines))
                (metadata/entry/line metadata/entry)     ; Alias so the code is easier to read.
                (newline?            (< i (1- length)))  ; Is this the end of a line?
                ;; Merge last entry's metadata with this entry's?
                (metadata/merge?     ))
            (int<mis>:debug caller
                            '("\n-------"
                              "\n--#%02d--")
                            i)
            (int<mis>:debug caller
                            "string/entry/line: %S"
                            string/entry/line)
            (int<mis>:debug caller
                            "newline?:          %S"
                            newline?)
            (int<mis>:debug caller
                            "metadata/merge?:   %S"
                            metadata/merge?)

            (int<mis>:debug caller
                            "  - 1? %S"
                            (= i 0))
            (int<mis>:debug caller
                            "  - 2? %S"
                            (not (null string/line)))
            (int<mis>:debug caller
                            "  - 3? %S"
                            (when (not (null string/line))
                              (not (string-match-p (rx string-start
                                                       (one-or-more not-newline)
                                                       (zero-or-more "\n")
                                                       string-end)
                                                   string/line))))

            ;;---
            ;; Process metadata...
            ;;---
            ;; NOTE: Process before string so we can do string checks before it's mutated.
            (setq metadata/line
                  ;; Not the very first thing we process and this string is just a newline?
                  (cond ((and (not (null metadata/line))
                              (string-match-p (rx string-start
                                                  (zero-or-more "\n")
                                                  string-end)
                                              string/entry/line))
                         ;; Use most appropriate metadata. If the line
                         ;; accumulator string is something, use its metadata,
                         ;; otherwise use the current empty line's metadata.
                         (if (and (not (null string/line))
                                  (string-match-p (rx string-start
                                                      (one-or-more not-newline)
                                                      (zero-or-more "\n")
                                                      string-end)
                                                  string/line))
                             metadata/line
                           metadata/entry/line))

                        ;; Merge metadata together because merging strings together.
                        ((and (= i 0)
                              ;; Existing string must be something other than just empty/newline.
                              (not (null string/line))
                              (string-match-p (rx string-start
                                                  (one-or-more not-newline)
                                                  (zero-or-more "\n")
                                                  string-end)
                                              string/line))
                         (int<mis>:output:update/metadata caller metadata/line metadata/entry/line))

                        ;; Otherwise, use only the new metadata.
                        (t
                         metadata/entry/line)))
            (int<mis>:debug caller
                            "meta/line:         %S"
                            metadata/line)

            ;;---
            ;; Process this string...
            ;;---
            ;; Add back in the newline char that `int<mis>:string:lines/split` removed?
            (when newline?
              (setq string/entry/line (concat string/entry/line "\n")))

            ;; Combine strings?
            (setq string/line (if (null string/line)
                                  string/entry/line
                                (concat string/line string/entry/line)))
            (int<mis>:debug caller
                            "string/line:       %S"
                            string/line)

            ;;---
            ;; Should we start a new entry?
            ;;---
            ;; Yes if this line isn't the last (aka when it ends with a newline).
            ;; E.g. splitting:
            ;;   -  ""    -> '("")
            ;;   - "1\n"  -> '("1" "")
            ;;   - "\n2"  -> '("" "2")
            ;;   - "1\n2" -> '("1" "2")
            (when (and newline?
                       (not (string-empty-p string/line))) ; Ignore if it's just... nothing.
              ;; Add this line into our output and start anew.
              (setq output/line (int<mis>:output:append caller
                                                        output/line
                                                        (apply #'int<mis>:output:create
                                                               caller
                                                               string/line
                                                               metadata/line))
                    string/line   nil
                    metadata/line nil)
              (int<mis>:debug caller
                              "output/line: %S"
                              output/line))))))

    ;;------------------------------
    ;; Return lines as a MOT
    ;;------------------------------
    ;; Need to add the final output entry (if it's not nothing).
    (when (not (string-empty-p string/line))
      (setq output/line (int<mis>:output:append caller
                                                output/line
                                                (apply #'int<mis>:output:create
                                                       caller
                                                       string/line
                                                       metadata/line))))
    (int<mis>:debug caller
                    "<--output:      %S"
                    output/line)
    output/line))
;; (int<mis>:output:finalize/lines 'test
;;                                 '((:output
;;                                    ((:string . "foo-0") (:metadata (:bar:0 . baz0)))
;;                                    ((:string . "foo-1") (:metadata (:bar:1 . baz1)))
;;                                    ((:string . "foo-2") (:metadata (:bar:2 . baz2)))
;;                                    ((:string . "narf") (:metadata (:zort . poit)))
;;                                    ((:string . "egad") (:metadata (:troz . fiddely-posh))))))
;;
;; (int<mis>:output:finalize/lines 'test
;;                                 '((:output
;;                                    ((:string . "\nfoo-0") (:metadata (:bar:0 . baz0)))
;;                                    ((:string . "foo-1\n") (:metadata (:bar:1 . baz1)))
;;                                    ((:string . "\nfoo-2") (:metadata (:bar:2 . baz2)))
;;                                    ((:string . "\nnarf") (:metadata (:zort . poit)))
;;                                    ((:string . "zort\ntroz") (:metadata (:egad . fiddely-posh))))))


(defun int<mis>:output:finalize/block (caller output)
  "Reduce the Mis OUTPUT Tree into a single string/metadata MOT.

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)"
  (let* ((caller (list 'int<mis>:output:finalize caller))
         string/finalized
         metadata/finalized
         output/finalized)
    (int<mis>:debug caller
                    "output:         %S"
                    output)

    ;;------------------------------
    ;; Finalize Outputs
    ;;------------------------------
    (dolist (entry (int<mis>:output:get/entries caller output))
      (int<mis>:debug caller
                      "entry:          %S"
                      entry)
      (let* ((string/entry   (int<mis>:output:get/string caller entry))
             (metadata/entry (int<mis>:output:get/metadata caller entry)))
        (int<mis>:debug caller
                        "string/entry:   %S"
                        string/entry)
        (int<mis>:debug caller
                        "meta/entry:     %S"
                        metadata/entry)

        ;; Combine strings.
        (unless (or (null string/entry)
                    (string-empty-p string/entry))
          (setq string/finalized
                (if (null string/finalized)
                    string/entry
                  (concat string/finalized string/entry)))
          (int<mis>:debug caller
                          "concat:         %S"
                          string/finalized))
        ;; Combine metadatas.
        (setq metadata/finalized
              (int<mis>:output:update/metadata caller metadata/finalized metadata/entry))
        (int<mis>:debug caller
                        "meta:          %S"
                        metadata/finalized)))

    ;;------------------------------
    ;; Return finalized as a MOT
    ;;------------------------------
    (setq output/finalized (apply #'int<mis>:output:create
                                  caller
                                  string/finalized
                                  metadata/finalized))
    (int<mis>:debug caller
                    "<--output:      %S"
                    output/finalized)
    output/finalized))
;; (int<mis>:output:finalize/block 'test
;;                                 '((:output
;;                                    ((:string . "\nfoo-0") (:metadata (:bar:0 . baz0)))
;;                                    ((:string . "foo-1\n") (:metadata (:bar:1 . baz1)))
;;                                    ((:string . "\nfoo-2") (:metadata (:bar:2 . baz2)))
;;                                    ((:string . "narf") (:metadata (:zort . poit)))
;;                                    ((:string . "egad") (:metadata (:troz . fiddely-posh))))))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'mis-tree-output)
;;; mis-tree-output.el ends here

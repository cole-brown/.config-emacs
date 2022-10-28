;;; mis-valid.el --- Internals for Validation -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Cole Brown
;;
;; Author:     Cole Brown <code@brown.dev>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2019-10-23
;; Modified:   2022-10-28
;;
;;; Commentary:
;;
;;  Internals for Validation
;;
;;; Code:


(require 'mis-error)


;;------------------------------------------------------------------------------
;; Constants
;;------------------------------------------------------------------------------

;;------------------------------
;; Categories
;;------------------------------

;; User Input Categories
(defconst int<mis>:keywords:category/input
  '(:style
    :comment
    :line
    :string)
  "Valid Mis categories for parsing & validation.")


;; Mis Syntax Tree Categories
(defconst int<mis>:keywords:category/internal
  '(:style
    :comment
    :format)
  "Valid Mis categories for parsing & validation.")


(defconst int<mis>:keywords:category
  '((:style   :style)
    (:comment :comment)
    (:line    :line)
    (:string  :format))
  "Alist of cons of keyword to keyword list.

Cons: (input-category-keyword . (internal-category-keyword-0 ...))
AKA:  (input-category-keyword internal-category-keyword-0 ...)")


;;------------------------------
;; Keywords for Mis type
;;------------------------------

(defconst int<mis>:keywords:style
  '(:width
    :padding
    :align ;; Uses: `:width', `:padding'
    :indent
    :trim :trim:left :trim:right
    :newlines
    :propertize
    :face
    :weight
    :bold
    :slant
    :italic
    :foreground
    :background)
  "Valid style keywords.")


(defconst int<mis>:keywords:style/no-cascade
  '(:newlines
    :indent)
  "Style keywords that are not allowed to cascade down into childrens' styles.")


(defconst int<mis>:keywords:comment
  '(:type
    :language
    :prefix:major   ; start of first line of comment
    :prefix:minor   ; start of rest of the comment lines
    :postfix:minor  ; end of rest of the comment lines
    :postfix:major) ; end of final line of comment
  "Valid comment keywords.")


(defconst int<mis>:keywords:line
  '(:string)
  "Valid line keywords.")


(defconst int<mis>:keywords:metadata
  '(:buffer
    :output
    :align
    :width)
  "Valid metadata keywords.")


;;------------------------------
;; Keywords/Symbols for a specific Mis keyword.
;;------------------------------

(defconst int<mis>:valid:align/types
  ;; keyword / symbol
  '(:left      left
    :center    center
    :right     right)
  "Valid Mis alignment types.")


(defconst int<mis>:valid:comment/types
  ;; keyword / symbol
  '(:inline  inline
    :block   block
    :quote   quote
    :default default)
  "Valid Mis comment types.

'inline' is a standard single-line comment.
'block' is a standard multi-line comment.
'quote' is a multi-line comments for e.g. org, markdown.
'default' depends on buffer's major-mode.")


(defconst int<mis>:valid:indent/types
  ;; keyword / symbol
  '(:fixed    fixed
    :existing existing
    :auto     auto)
  "Valid Mis indent types.

See indent's styler, `int<mis>:style:indent' for what indent symbols mean.

Also valid is:
  - any positive integer
  - a string")


(defconst int<mis>:valid:output/types
  '(buffer
    string)
  "Valid Mis output types.

Keyword and symbol both allowed; use `int<mis>:valid:symbol-or-keyword?' to
validate.")


;;------------------------------------------------------------------------------
;; Keyword Validation
;;------------------------------------------------------------------------------

(defun int<mis>:valid:category/tmp? (caller check &optional valids)
  "Ensure CHECK category keyword is valid according to VALID/INPUTS list.

CHECK should be:
  - a temp keyword      - prefix `:tmp:'
                        - any valid

VALIDS should be nil (all temp keywords are valid), or a list of exactly which
temp keywords are valid.

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)

Return nil/non-nil."
  (let ((caller (list 'int<mis>:valid:category/tmp? caller)))
    (unless (keywordp check)
      (int<mis>:error caller
                      "CHECK must be a keyword. Got %S: %S"
                      (type-of check)
                      check))

    ;;------------------------------
    ;; Validate & Return
    ;;------------------------------
    ;; Validate against exact matches.
    (if (not (null valids))
        ;; It must match, exactly, something in VALID/INPUTS.
        (not (null (memq check valids)))

      ;; Validate in general.
      (let* ((check/str (symbol-name check))
             (check/len (length check/str)) ; Avoid out of range errorrs in `substring'.
             (type      (substring check/str 0 (min 5 check/len)))) ; Get enough to check for ":tmp:".
        ;; Any `:tmp:...' keyword is valid. They should have more after ":tmp:",
        ;; but whatever.
        (string= type ":tmp:")))))
;; (int<mis>:valid:category/tmp? 'test :tmp:foo nil)
;; (int<mis>:valid:category/tmp? 'test :tmp:foo '(:tmp:bar :tmp:foo))
;; (int<mis>:valid:category/tmp? 'test :tmp:foo '(:tmp:bar))


(defun int<mis>:valid:category/mis/input? (caller check &optional valids)
  "Ensure CHECK category keyword is valid according to VALIDS list.

CHECK should be:
  - an input keyword - from `int<mis>:keywords:category/input'

VALIDS should be nil (all input keywords are valid), or a list of
exactly which keywords are valid.

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)

Return nil/non-nil."
  (let ((caller (list 'int<mis>:valid:category/mis? caller)))
    (unless (keywordp check)
      (int<mis>:error caller
                      "CHECK must be a keyword. Got %S: %S"
                      (type-of check)
                      check))

    ;;------------------------------
    ;; Validate & Return
    ;;------------------------------
    (let ((existing? (not (null (memq check int<mis>:keywords:category/input)))))
      ;; Validate against exact matches if present, else against existing input keywords.
      (if (null valids)
          ;; Valid if it's an existing keyword; no more checks.
          existing?

        ;; Needs to be both an existing keyword and in the VALIDS set.
        (and existing?
             (not (null (memq check valids))))))))
;; (int<mis>:valid:category/mis/input? 'test :style)
;; (int<mis>:valid:category/mis/input? 'test :style nil)
;; (int<mis>:valid:category/mis/input? 'test :tmp:keyword nil)
;; (int<mis>:valid:category/mis/input? 'test :style '(:style :tmp:bar :tmp:foo))


(defun int<mis>:valid:category/mis/input:param? (caller check &optional valids)
  "Ensure CHECK as a function param is valid according to VALIDS list.

CHECK should be:
  - nil
    - All (existing) categories are valid.
  - an input keyword
    - Must be from `int<mis>:keywords:category/input'.
  - a list of input keywords
    - Each must be from `int<mis>:keywords:category/input'.

VALIDS should be nil (all input keywords are valid), or a list of
exactly which keywords are valid.

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)

Return non-nil or signal an error."
  (let ((caller (list 'int<mis>:valid:category/mis/input:param? caller)))
    ;; Nil is just valid.
    (cond ((null check)
           t)

          ;; Since it's not null, it has to be a keyword or a list of keywords.
          ((and (not (keywordp check))  ; keyword
                (not (and (listp check) ; list of keywords
                          (seq-every-p #'keywordp check))))
           (int<mis>:error caller
                           '("CHECK must be nil, a keyword, or a list of keywords. "
                             "Got %S: %S")
                           (type-of check)
                           check))

          ;; Is the single keyword CHECK a valid keyword?
          ((and (keywordp check)
                (not (int<mis>:valid:category/mis/input? 'mis check valids)))
           (int<mis>:error caller
                           "CHECK keyword must be a member of %S or nil. Got: %S"
                           (or valids
                               int<mis>:keywords:category/input)
                           check))

          ;; Does the list of keywords CHECK contain only valid keywords?
          ((and (listp check)
                (not (seq-every-p (lambda (each)
                                    "Is EACH keyword in CHECK valid?"
                                    (int<mis>:valid:category/mis/input? 'mis each valids))
                                  check)))
           (int<mis>:error caller
                           "Each keyword in CHECK must be a member of %S. Got: %S"
                           int<mis>:keywords:category/input
                           check))

          ;; Valid; got through all the invalid checks.
          (t
           t))))
;; (int<mis>:valid:category/mis/input:param? 'test '(:comment :style :string))


(defun int<mis>:valid:category/mis/internal? (caller check &optional valids)
  "Ensure CHECK category keyword is valid according to VALIDS list.

CHECK should be:
  - an internal keyword - from `int<mis>:keywords:category/internal'

VALIDS should be nil (all internal keywords are valid), or a list of
exactly which internal keywords are valid.

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)

Return nil/non-nil."
  (let ((caller (list 'int<mis>:valid:category/mis? caller)))
    (unless (keywordp check)
      (int<mis>:error caller
                      "CHECK must be a keyword. Got %S: %S"
                      (type-of check)
                      check))

    ;;------------------------------
    ;; Validate & Return
    ;;------------------------------
    (let ((existing? (not (null (memq check int<mis>:keywords:category/internal)))))
      ;; Validate against exact matches if present, else against existing internal keywords.
      (if (null valids)
          ;; Valid if it's an existing keyword; no more checks.
          existing?

        ;; Needs to be both an existing keyword and in the VALIDS set.
        (and existing?
             (not (null (memq check valids))))))))
;; (int<mis>:valid:category/mis/internal? 'test :style)
;; (int<mis>:valid:category/mis/internal? 'test :style nil)
;; (int<mis>:valid:category/mis/internal? 'test :style nil)
;; (int<mis>:valid:category/mis/internal? 'test :style '(:style :tmp:bar :tmp:foo))


(defun int<mis>:valid:validator (caller category keyword)
  "Determine which category KEYWORD falls under, return validator func for it.

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)

CATEGORY must be:
  - nil                - All existing categories' keywords are valid.
  - a list of keywords - Only these existing categories' keywords are valid.

Signal an error if invalid; if valid, return cons 2-tuple of:
  (category . validator-fn)"
  (let ((caller (list 'int<mis>:keyword:category caller))
        check/category
        validator/category
        validator/func)
    ;;------------------------------
    ;; Error Check Params
    ;;------------------------------
    ;; Is CATEGORY a valid type?
    (cond ((and (not (null category)) ; nil
                (not (and (listp category) ; list of keywords
                          (seq-every-p #'keywordp category))))
           (int<mis>:error caller
                           '("CATEGORY must be nil or a list of keywords. "
                             "Got %S: %S")
                           (type-of category)
                           category))

          ;; Does the list of keywords CATEGORY contain only valid keywords?
          ((and (listp category)
                (not (seq-every-p (lambda (cat)
                                    "Is every CATEGORY keyword valid?"
                                    (int<mis>:valid:category/mis/input? 'mis cat))
                                  category)))
           (int<mis>:error caller
                           "All members of CATEGORY must be members of %S. CATEGORY: %S"
                           int<mis>:keywords:category/input
                           category))

          ((not (keywordp keyword))
           (int<mis>:error caller
                           "KEYWORD must be a keyword. Got %S: %S"
                           (type-of keyword)
                           keyword))

          ;;------------------------------
          ;; Valid Keyword for Category ___?
          ;;------------------------------
          ;; 1. Need to return the valid temp/internal keyword and its validator function.
          ;; 2. Also need to validate its category against the allowed.
          ;; 3. Also need to be DRY.
          ;; So save to vars, check after.

          ((memq keyword int<mis>:keywords:style)
           ;; Set the validation/return values.
           (setq check/category     :style
                 validator/category :style
                 validator/func     #'int<mis>:valid:style/kvp?))

          ((memq keyword int<mis>:keywords:comment)
           (setq check/category     :comment
                 validator/category :comment
                 validator/func     #'int<mis>:valid:comment/kvp?))

          ((memq keyword int<mis>:keywords:line)
           ;; Lines get converted into `:format' after parsing, so they're just temporary.
           (setq check/category     :line
                 validator/category :tmp:line
                 validator/func     #'int<mis>:valid:line/kvp?))

          ;;------------------------------
          ;; Fallthrough: Error
          ;;------------------------------
          (t
           (int<mis>:error caller
                           "Keyword has no known category: %S"
                           keyword)))

    ;;------------------------------
    ;; Found Keyword; Check & Return
    ;;------------------------------
    ;; Is it a valid input category?
    (if (int<mis>:valid:category/mis/input? 'mis check/category category)
        ;; Valid; return.
        (cons validator/category validator/func)
      ;; Invalid; error.
      (int<mis>:error caller
                      "Keyword `%S' is in category `%S', which is not a valid category: %S"
                      keyword
                      validator/category
                      (or category
                          int<mis>:keywords:category/input)))))


(defun int<mis>:valid:style/kvp? (caller keyword value)
  "Determine which validation predicate to call for KEYWORD, then call it.

VALUE should be keyword's value, if it has one, or nil if not.

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)

Signal an error if invalid; return normalized value if valid."
  (let ((caller (list 'int<mis>:valid:style/kvp? caller)))
    ;; Validators will return the valid value, if it's valid.
    (pcase keyword
      ;;------------------------------
      ;; Keyword Validators
      ;;------------------------------
      (:width
       (int<mis>:valid:positive-integer? caller keyword value))

      (:padding
       (int<mis>:valid:string-or-char? caller keyword value)
       (when (and (stringp value)
                  (not (= (length value) 1)))
         (int<mis>:error 'int<mis>:align
                         "%s must be a character or a string of length 1. Got a string of length %d: %S"
                         (int<mis>:error:name keyword)
                         (length padding)
                         padding))
       ;; Must return the value if it's valid, so:
       value)

      (:align
       (int<mis>:valid:member? caller keyword value int<mis>:valid:align/types))

      (:indent
       (int<mis>:valid:indent? caller keyword value))

      ((or :trim :trim:left :trim:right)
       (int<mis>:valid:string-or-nil? caller keyword value))

      (:newlines
       (int<mis>:valid:boolean? caller keyword value))

      ;; Style is allowed to have children right now. For example, center a few
      ;; things by grouping them in a `mis:style':
      ;;   (mis:style :align 'center
      ;;              <thing-to-be-centered-1>
      ;;              <thing-to-be-centered-2>
      ;;              ...)
      (:children
       value)

      ;;------------------------------
      ;; Fallthrough / Error
      ;;------------------------------
      (_
       (int<mis>:error caller
                       "Don't know how to validate keyword %S"
                       keyword)))))


(defun int<mis>:valid:style/exclusively? (caller syntax)
  "Return non-nil if SYNTAX is _only_ styling; return nil otherwise.

SYNTAX must be nil or must:
  - Only have one branch.
  - Only have `:style' branches.
  - Only have styling keys in the `:style' branch.
    - No sub-branches (children).

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)"
  (let ((caller (list 'int<mis>:valid:style/exclusively? caller)))
    ;; No styling is valid styling.
    (cond ((null syntax)
           (int<mis>:debug caller
                           "Ok; Nil is a valid styling SYNTAX: %S"
                           syntax)
           t)

          ;; Must have valid syntax.
          ((not (int<mis>:valid:syntax? caller
                                        'syntax
                                        syntax
                                        :no-error))
           (int<mis>:debug caller
                           "Invalid mis SYNTAX! %S"
                           syntax)
           nil)

          ;; (Alist) syntax should contain only 1 element.
          ((not (eq 1 (length syntax)))
           (int<mis>:debug caller
                           "Style SYNTAX should be length 1, got %d: %S"
                           (length syntax)
                           syntax)
           nil)

          ;; Key should be `:style'...
          ((int<mis>:syntax:has caller
                                syntax
                                :style)
           (int<mis>:debug caller
                           "SYNTAX has `:style'... is `:style' only styling?: %S"
                           syntax)
           ;; Do not let `int<mis>:valid:...' signal errors and break code flow.
           ;; Non-exclusively styling trees are a-ok.
           (condition-case err
               ;; All the entries of `:style' should be styling keywords/values.
               (let ((valid-keys t))
                 (dolist (entry (int<mis>:syntax:get/value caller :style syntax))
                   (let ((key   (car entry))
                         (value (cdr entry)))
                     (unless (int<mis>:valid:member? caller key key int<mis>:keywords:style)
                       (int<mis>:debug caller
                                       "Style SYNTAX has invalid key `%S' = %S. SYNTAX: %S"
                                       key
                                       value
                                       syntax)
                       (setq valid-keys nil))))

                 (if valid-keys
                     (progn
                       (int<mis>:debug caller
                                       "Ok; SYNTAX looks like only styling: %S"
                                       syntax)
                       ;; Return yep.
                       t)

                   (int<mis>:debug caller
                                   "Failure! Style SYNTAX is not only styling: %S"
                                   syntax)
                   ;; Return nope.
                   nil))
             ;;---
             ;; Condition-Case Signal Handlers:
             ;;---
             (error
              ;; Probably a call to a `int<mis>:valid:...' function returned "no, not valid".
              ;; Therefore this is not _only_ styling keywords, so return nope.
              (int<mis>:debug caller
                              "Style SYNTAX has non-styling data, probably: %S -> %S"
                              (car err)
                              (cdr err))
              nil)))

          ;; Fallthrough: not styling so return nil.
          (t
           (int<mis>:debug caller
                           "Style SYNTAX doesn't seem to be a Mis Styling Syntax Tree? %S"
                           syntax)
           nil))))
;; (int<mis>:valid:style/exclusively? nil)
;; (int<mis>:valid:style/exclusively? (mis:style :width 80))
;; (int<mis>:valid:style/exclusively? (mis:style))
;; (int<mis>:valid:style/exclusively? (mis:style :align 'center "Hello there."))
;; Invalid:
;;   `:string' is not a styling keyword and is not in children.
;;   (int<mis>:valid:style/exclusively? '((:style (:align . center)) (:string . "hello")))


(defun int<mis>:valid:comment/kvp? (caller keyword value)
  "Determine which validation predicate to call for KEYWORD, then call it.

VALUE should be keyword's value, if it has one, or nil if not.

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)

Signal an error if invalid; return normalized value if valid."
  (let ((caller (list 'int<mis>:valid:keyword? caller)))
    ;; Validators will return the valid value, if it's valid.
    (pcase keyword
      ;;------------------------------
      ;; Keyword Validators
      ;;------------------------------
      (:type
       (int<mis>:valid:member? caller keyword value int<mis>:valid:comment/types))

      (:language
       (int<mis>:valid:string-symbol-nil? caller keyword value))

      ((or :prefix:major :postfix:major :prefix:minor :postfix:minor)
       (int<mis>:valid:string-or-nil? caller keyword value))

      ;;------------------------------
      ;; Fallthrough / Error
      ;;------------------------------
      (_
       (int<mis>:error caller
                       "Don't know how to validate keyword %S"
                       keyword)))))


(defun int<mis>:valid:line/kvp? (caller keyword value)
  "Determine which validation predicate to call for KEYWORD, then call it.

VALUE should be keyword's value, if it has one, or nil if not.

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)

Signal an error if invalid; return normalized value if valid."
  (let ((caller (list 'int<mis>:valid:line/kvp? caller)))
    ;; Validators will return the valid value, if it's valid.
    (pcase keyword
      ;;------------------------------
      ;; Keyword Validators
      ;;------------------------------
      (:width
       (int<mis>:valid:positive-integer? caller keyword value))

      (:string
       (int<mis>:valid:string? caller keyword value))

      ;;------------------------------
      ;; Fallthrough / Error
      ;;------------------------------
      (_
       (int<mis>:error caller
                       "Don't know how to validate keyword %S"
                       keyword)))))


;;------------------------------------------------------------------------------
;; Normalization Functions
;;------------------------------------------------------------------------------

(defun int<mis>:valid:normalize->symbol (caller name keyword-or-symbol)
  "Normalizes a keyword or a symbol to a symbol.
Remove ':' from the front of the keyword/symbol name if present.

NAME should be KEYWORD-OR-SYMBOL's symbol name as a symbol or string.

KEYWORD-OR-SYMBOL should be a keyword or a symbol.

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)"
  (let ((caller (list "int<mis>:valid:normalize->symbol" caller)))
    (cond ((keywordp keyword-or-symbol)
           (intern (string-remove-prefix ":" (symbol-name keyword-or-symbol))))
          ((symbolp keyword-or-symbol)
           keyword-or-symbol)
          (t
           (int<mis>:error caller
                           "%s must be a keyword or symbol. Got %S: %S"
                           (int<mis>:error:name name)
                           (type-of keyword-or-symbol)
                           keyword-or-symbol)))))
;; (int<mis>:valid:normalize->symbol 'test 'type :foo)
;; (int<mis>:valid:normalize->symbol 'test 'type 'foo)
;; (int<mis>:valid:normalize->symbol 'test 'type [])


(defun int<mis>:valid:normalize->string (caller name symbol-or-string)
  "Normalizes a keyword, symbol, or string to a string.
Remove ':' from the front of the keyword/symbol name if present.

NAME should be SYMBOL-OR-STRING's symbol name as a symbol or string.

SYMBOL-OR-STRING should be a keyword or a symbol.

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)"
  (let* ((caller (list "int<mis>:valid:normalize->symbol" caller)))
    (string-remove-prefix ":"
                          (cond ((symbolp symbol-or-string)
                                 (symbol-name symbol-or-string))
                                ((stringp symbol-or-string)
                                 symbol-or-string)
                                (t
                                 (int<mis>:error caller
                                                 "%s must be a keyword, symbol, or string. Got %S: %S"
                                                 (int<mis>:error:name name)
                                                 (type-of symbol-or-string)
                                                 symbol-or-string))))))
;; (int<mis>:valid:normalize->string 'test 'type :foo)
;; (int<mis>:valid:normalize->string 'test 'type 'foo)
;; (int<mis>:valid:normalize->string 'test 'type "foo")
;; (int<mis>:valid:normalize->string 'test 'type ":foo")
;; (int<mis>:valid:normalize->symbol 'test 'type [])


;;------------------------------------------------------------------------------
;; Validation "Predicates"
;;------------------------------------------------------------------------------
;; If valid, return the valid value.
;;
;; NOTE: nil can be a valid value, so these signal an error on invalid input.
;; Use a `condition-case' if you need to validate and failure is a valid option.

;; TODO: For all the predicates: Remove `&rest _`, replace with nothing or `&optional no-error?`.

(defun int<mis>:valid:boolean? (caller name value &rest _)
  "Return normalized t/nil boolean for VALUE.

A valid boolean input VALUE means either nil or non-nil.

NAME should be VALUE's symbol name as a symbol or string.

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)"
  ;; Convert VALUE to canonical boolean t/nil values.
  (if value
      t
    nil))


(defun int<mis>:valid:positive-integer? (caller name value &rest _)
  "Signal an error if VALUE is not a integer, else return VALUE.

NAME should be VALUE's symbol name as a symbol or string.

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)"
  (if (integerp value)
      (if (> value 0)
          value
        (int<mis>:error caller
                    "%s must be a positive integer. Got: %d"
                    (int<mis>:error:name name)
                    value))
    (int<mis>:error caller
                    "%s must be of type integer. Got %S: %S"
                    (int<mis>:error:name name)
                    (type-of value)
                    value)))


(defun int<mis>:valid:string? (caller name value &rest _)
  "Signal an error if VALUE is not a string, else return VALUE.

NAME should be VALUE's symbol name as a symbol or string.

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)"
  (if (stringp value)
      value
    (int<mis>:error caller
                    "%s must be of type string. Got %S: %S"
                    (int<mis>:error:name name)
                    (type-of value)
                    value)))


(defun int<mis>:valid:string-or-char? (caller name value &rest _)
  "Signal an error if VALUE is not a string or a character, else return VALUE.

NAME should be VALUE's symbol name as a symbol or string.

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)"
  (if (or (stringp value)
          (characterp value))
      value
    (int<mis>:error caller
                    "%s must be of type string or character. Got %S: %S"
                    (int<mis>:error:name name)
                    (type-of value)
                    value)))


(defun int<mis>:valid:string-or-nil? (caller name value &rest _)
  "Signal an error if VALUE is not a string or nil, else return VALUE.

NAME should be VALUE's symbol name as a symbol or string.

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)"
  (if (or (null value) (stringp value))
      value
    (int<mis>:error caller
                    "%s must be nil or of type string. Got %S: %S"
                    (int<mis>:error:name name)
                    (type-of value)
                    value)))


(defun int<mis>:valid:string-or-symbol? (caller name value &rest _)
  "Signal an error if VALUE is not a string or a symbol, else return VALUE.

NAME should be VALUE's symbol name as a symbol or string.

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)"
  (if (or (stringp value)
          (symbolp value))
      value
    (int<mis>:error caller
                    "%s must be of type string or symbol. Got %S: %S"
                    (int<mis>:error:name name)
                    (type-of value)
                    value)))
;; (int<mis>:valid:string-or-symbol? 'test 'input :foo)
;; (int<mis>:valid:string-or-symbol? 'test 'input "foo")


(defun int<mis>:valid:string-symbol-nil? (caller name value &rest _)
  "Signal an error if VALUE is not a string, a symbol, or nil; else return VALUE.

NAME should be VALUE's symbol name as a symbol or string.

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)"
  (if (or (stringp value)
          (symbolp value)
          (null value))
      value
    (int<mis>:error caller
                    "%s must be either a string, a symbol, or nil. Got type %S: %S"
                    (int<mis>:error:name name)
                    (type-of value)
                    value)))


(defun int<mis>:valid:string1-char-nil? (caller name value &optional no-error?)
  "Assert VALUE is a string of length 1, a character, or nil.

Return VALUE or signal an error. If NO-ERROR? is non-nil, return NO-ERROR?
instead of signaling an error.

NAME should be VALUE's symbol name as a symbol, keyword, or string.

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)"
  (cond ((and (not (null value))
              (not (stringp value))
              (not (characterp value)))
         (if no-error?
             no-error?
           (int<mis>:error caller
                           "%s must be of type string, character, or nil. Got %S: %S"
                           (int<mis>:error:name name)
                           (type-of value)
                           value)))

        ((and (stringp value)
              (not (= (length value) 1)))
         (if no-error?
             no-error?
           (int<mis>:error caller
                           '("%s must be nil, a character, or a string of length 1. "
                             "Got a string of length %S: %S")
                           (int<mis>:error:name name)
                           (length value)
                           value)))

        (t
         ;; Not invalid; return it.
         value)))


(defun int<mis>:valid:member? (caller name value valids &rest _)
  "Signal an error if VALUE is not a member of VALIDS, else return VALUE.

VALIDS should be a list of valid values for VALUE; will use `memq' to evaluate
membership.

NAME should be VALUE's symbol name as a symbol or string.

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)"
  (if (memq value valids)
      value
    (int<mis>:error caller
                    "%s must be one of: %S. Got %S: %S"
                    (int<mis>:error:name name)
                    valids
                    (type-of value)
                    value)))


(defun int<mis>:valid:symbol-or-keyword? (caller name value valid-symbols &rest _)
  "Normalize VALUE and validate against VALID-SYMBOLS.

Convert VALUE to a symbol if it's a keyword, then compare to the list of
VALID-SYMBOLS.

VALID-SYMBOLS should be a list of valid symbols; will use `memq' to evaluate
membership.

NAME should be VALUE's symbol name as a symbol or string.

For example, if you want all of these to be valid:
  '(foo :foo bar :bar baz :baz)
Then check like so:
  (let ((example-input :foo))
    (int<mis>:valid:symbol-or-keyword? 'example
                                       'example-input
                                       example-input
                                       '(foo bar baz)))

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)"
  ;; Normalize to symbol for returning.
  (let* ((caller (list "int<mis>:valid:symbol-or-keyword?" caller))
         (value/symbol (int<mis>:valid:normalize->symbol caller name value)))
    ;; Validate the valids first...
    (unless (seq-every-p (lambda (x) "Validate VALID-SYMBOLS."
                           (and (symbolp nil)
                                (not (keywordp x))))
                         valid-symbols)
      (int<mis>:error caller
                      "%s must be a list of only symbols (no keywords). Got: %S"
                      (int<mis>:error:name name)
                      valid-symbols))

    ;; Validate & return normalized value.
    (if (memq value/symbol valid-symbols)
        value/symbol
      (int<mis>:error caller
                      "%s must be one of: %S. Got %S: %S"
                      (int<mis>:error:name name)
                      valid-symbols
                      (type-of value/symbol)
                      value/symbol))))


(defun int<mis>:valid:indent? (caller name value &rest _)
  "Signal an error if VALUE is not a valid indentation.

VALUE can be:
  - `:fixed' / `fixed'
  - `:existing' / `existing'
  - `:auto' / `auto'
  - a positive integer
  - a string

NAME should be VALUE's symbol name as a symbol or string.

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)"
  (cond ((integerp value)
         ;; Ensure it's the right kind of integer & return.
         (int<mis>:valid:positive-integer? (list 'int<mis>:valid:indent? caller)
                                           name
                                           value))

        ((stringp value)
         ;; Could check for empty strings if we want, but let's try just
         ;; allowing anything for now...
         value)

        ((memq value int<mis>:valid:indent/types)
         ;; It's the right kind of symbol, so return it.
         value)

        (t
         (int<mis>:error caller
                         "%S must be an integer or one of: %S. Got %S: %S"
                         (int<mis>:error:name name)
                         int<mis>:valid:indent/types
                         (type-of value)
                         value))))


;;------------------------------------------------------------------------------
;; Mis Trees
;;------------------------------------------------------------------------------

;; TODO: Do we need to recursively check the syntax tree? Currently only check top level.
;; If we do want that, rename `int<mis>:valid:syntax?' to
;; `int<mis>:valid:syntax/alist?' or something, and make a new "is this a
;; completely valid syntax?"


(defun int<mis>:valid:syntax? (caller name syntax &optional no-error?)
  "Error if SYNTAX fails any basic sanity check for Mis Syntax Trees.

NAME should be OUTPUT's symbol name as a symbol or string.

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)

NO-ERROR? should be nil/non-nil. If non-nil, will return nil instead of
signaling an error."
  (let ((caller (list 'int<mis>:valid:syntax? caller))
        (name (int<mis>:error:name name)))
    ;; NOTE: We do not consider `nil' a valid syntax tree,
    ;; so check for explicit nil first sot we can then check for lists.
    (cond ((null syntax)
           (if no-error?
               nil
             (int<mis>:error caller
                             "%S must be... something. Got: %S"
                             name
                             syntax)))

          ;; An alist has to be a list.
          ((not (listp syntax))
           (if no-error?
               nil
             (int<mis>:error caller
                             "%S must be a list. Got %S: %S"
                             name
                             (type-of syntax)
                             syntax)))

          ;; An alist has to have only lists (or cons, which are lists).
          ((not (seq-every-p #'listp syntax))
           (if no-error?
               nil
             (int<mis>:error caller
                             "%S must be a list of lists. Got: %S"
                             name
                             syntax)))

          ;; And we require all of them to have keywords as keys.
          ((not (seq-every-p (lambda (syntax/assoc) "Ensure alist keys are keywords."
                               (keywordp (car syntax/assoc)))
                             syntax))
           (if no-error?
               nil
             (int<mis>:error caller
                             "%S must be an alist with keyword keys. Got: %S"
                             name
                             syntax)))

          ;; And... it should not be a Mis Output Tree, probably?
          ;; This just checks for first key of `:output'; full check would be `int<mis>:valid:output?'.
          ((eq :output (caar syntax))
           (if no-error?
               nil
             (int<mis>:error caller
                             "%S should not be a Mis Output Tree, but found `:output' key. %S"
                             name
                             syntax)))

          ;; Fallthrough: Failed to find a reason it's invalid so it must be valid?
          (t))))
;; (int<mis>:valid:syntax? 'test 'syntax '((:format (:formatter repeat :string "-"))))
;; (int<mis>:valid:syntax? 'test 'syntax (int<mis>:output:create 'test "foo" '(:buffer . "bar") '(:align . baz)))


(defun int<mis>:valid:output? (caller name output &optional no-error?)
  "Error if OUTPUT fails any basic sanity check for Mis Output Trees.

NAME should be OUTPUT's symbol name as a symbol or string.

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)

NO-ERROR? should be nil/non-nil. If non-nil, will return nil instead of
signaling an error."
  (let ((caller (list 'int<mis>:valid:output? caller))
        (name (int<mis>:error:name name)))
    ;; NOTE: We do not consider `nil' a valid output tree,
    ;; so check for explicit nil first sot we can then check for lists.
    (cond ((null output)
           (if no-error?
               nil
             (int<mis>:error caller
                             "%S must be... something. Got: %S"
                             name
                             output)))

          ;; NOTE: These trees are formatted as an alist with key/value cons:
          ;;   - key: `:output'
          ;;   - value:
          ;;     - list of alists with key/value conses
          ;;     - valid keys: `:string', `:metadata'

          ;; An alist has to be a list.
          ((not (listp output))
           (if no-error?
               nil
             (int<mis>:error caller
                             "%S must be a list. Got %S: %S"
                             name
                             (type-of output)
                             output)))

          ;; An alist has to have only lists (or cons, which are lists).
          ((not (seq-every-p #'listp output))
           (if no-error?
               nil
             (int<mis>:error caller
                             "%S must be a list of lists. Got: %S"
                             name
                             output)))

          ;; And we require all of them to have keywords as keys.
          ((not (seq-every-p (lambda (output/assoc) "Ensure alist keys are keywords."
                               (keywordp (car output/assoc)))
                             output))
           (if no-error?
               nil
             (int<mis>:error caller
                             "%S must be an alist with keyword keys. Got: %S"
                             name
                             output)))

          ;; Currently, the output list only has the `:output' key.
          ((or (not (eq 1 (length output)))
               (not (eq :output (caar output))))
           (if no-error?
               nil
             (int<mis>:error caller
                             "%S must be an alist with only `:output' key. Got: %S"
                             name
                             output)))

          ;; Fallthrough: Failed to find a reason it's invalid so far...
          (t
           ;; Now... Check down inside the output list...
           (let* ((output/value (alist-get :output output))
                  (output/value/name (int<mis>:error:name :output))
                  (output/value/alist/name (concat output/value/name " alist"))
                  (valid t))
             ;; Should be a list of alists.
             (if (not (listp output/value))
                 (if no-error?
                     (setq valid nil)
                   (int<mis>:error caller
                                   "%S must be a list. Got %S: %S"
                                   output/value/name
                                   (type-of output/value)
                                   output/value))
               ;; Check each alist in list.
               (dolist (output/value/alist output/value)
                 ;; Should be an alist with only keys: `:string' and `:metadata'
                 ;; So... should be list of length 2.
                 (cond ((not (listp output/value/alist))
                        (if no-error?
                            (setq valid nil)
                          (int<mis>:error caller
                                          "%S must be a list. Got %S: %S"
                                          output/value/alist/name
                                          (type-of output/value/alist)
                                          output/value/alist)))

                       ;; An alist has to have only lists (or cons, which are lists).
                       ((not (seq-every-p #'listp output/value/alist))
                        (if no-error?
                            (setq valid nil)
                          (int<mis>:error caller
                                          "%S must be a list of lists. Got: %S"
                                          output/value/alist/name
                                          output/value/alist)))

                       ;; And this alist should only have the 2 keys: `:string' and `:metadata'
                       ((or (not (eq 2 (length output/value/alist)))
                            ;; Use `assoc' in order to allow null values.
                            (not (assoc :string output/value/alist))
                            (not (assoc :string output/value/alist)))
                        (if no-error?
                            (setq valid nil)
                            nil
                          (int<mis>:error caller
                                          "%S must be an alist with only `:string' and `:metadata' key. Got: %S"
                                          name
                                          output/value/alist)))

                       ;; Fallthrough: Failed to find a reason it's invalid so it must be valid?
                       (t)))
               ;; Return the valid tree, or the "not valid and not erroring" return value of nil.
               (if valid
                   output
                 nil)))))))
;; Valid:
;;   (int<mis>:valid:output? 'test 'output '((:output ((:string . "foo") (:metadata . :foo)) ((:string . "bar") (:metadata . :bar)))))
;;   (int<mis>:valid:output? 'test 'output (int<mis>:output:create 'test "foo" '(:buffer . "bar") '(:align . baz)))
;; Not Valid:
;;   (int<mis>:valid:output? 'test 'output '((:output (:string . "foo") (:metadata . "...actual metadata here"))))
;;   (int<mis>:valid:output? 'test 'output '((:format (:formatter repeat :string "-"))))


(defun int<mis>:tree:type (caller tree)
  "Return TREE's type.

Return:
  - `:syntax' - valid Mis Syntax Tree
  - `:output' - valid Mis Output Tree
If not a valid Mis tree, signal an error.

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)"
  (let ((caller (list 'int<mis>:tree:type caller))
        (name (int<mis>:error:name 'tree)))
    (cond ((int<mis>:valid:output? caller name tree :no-error)
           :output)
          ((int<mis>:valid:syntax? caller name tree :no-error)
           :syntax)
          (t
           (int<mis>:error caller
                           "%s is not a Mis Tree of any sort. %s: %S"
                           name
                           name
                           tree)))))
;; (int<mis>:tree:type 'test (int<mis>:syntax:create 'test :style '(:width . 10) '(:align . :center)))
;; (int<mis>:tree:type 'test (int<mis>:output:create 'test "foo" '(:buffer . "bar") '(:align . baz)))
;; (int<mis>:tree:type 'test nil)


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'mis-valid)
;;; mis-valid.el ends here

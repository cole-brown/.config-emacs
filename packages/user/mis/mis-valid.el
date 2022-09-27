;;; mis-valid.el --- Internals for Validation -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Cole Brown
;;
;; Author: Cole Brown <code@brown.dev>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:  2019-10-23
;; Modified: 2022-08-15
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
    :trim :trim:left :trim:right)
  "Valid style keywords.")


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

      ;;------------------------------
      ;; Fallthrough / Error
      ;;------------------------------
      (_
       (int<mis>:error caller
                       "Don't know how to validate keyword %S"
                       keyword)))))


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
;; Validation "Predicates"
;;------------------------------------------------------------------------------
;; Signal an error on invalid input, instead of just returning nil.

(defun int<mis>:valid:normalize->symbol (name keyword-or-symbol)
  "Normalizes a keyword or a symbol to a symbol.
That is, removes ':' from the symbol name if present.

NAME should be KEYWORD-OR-SYMBOL's symbol name as a symbol or string.

KEYWORD-OR-SYMBOL should be a keyword or a symbol."
  (cond ((keywordp keyword-or-symbol)
         (make-symbol (string-remove-prefix ":" (symbol-name keyword-or-symbol))))
        ((symbolp keyword-or-symbol)
         keyword-or-symbol)
        (t
         (int<mis>:error caller
                         "%s must be a keyword or symbol. Got type %S: %S"
                         (int<mis>:error:name keyword-or-symbol)
                         value))))
;; (int<mis>:valid:normalize->symbol 'type :foo)
;; (int<mis>:valid:normalize->symbol 'type 'foo)


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

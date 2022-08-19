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


(require 'subr)
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
    :line)
  "Valid Mis categories for parsing & validation.")


;; Mis Syntax Tree Categories
(defconst int<mis>:keywords:category/internal
  '(:mis:style
    :mis:comment
    :mis:format
    :mis:message
    :mis:string)
  "Valid Mis categories for parsing & validation.")


(defconst int<mis>:keywords:category
  '((:style   :mis:style)
    (:comment :mis:comment)
    (:line    :mis:format)
    ;; ???    :mis:message
    ;; ???    :mis:string
    )
  "Alist of keyword to keyword list conses.

Cons: (input-category-keyword . (internal-category-keyword-0 ...))")


;;------------------------------
;; Keywords for Mis type
;;------------------------------

(defconst int<mis>:keywords:style
  '(:width
    :align
    :indent
    :trim :trim:left :trim:right)
  "Valid style keywords.")


(defconst int<mis>:keywords:comment
  '(:type
    :language)
  "Valid comment keywords.")


(defconst int<mis>:keywords:line
  '(:string)
  "Valid line keywords.")


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
  '(:block  block
    :inline inline
    :default default)
  "Valid Mis comment types.

'block' is a multi-line comment.
'inline' is a single-line comment.
'default' depends on buffer's major-mode.")


;;------------------------------------------------------------------------------
;; Keyword Validation
;;------------------------------------------------------------------------------

(defun int<mis>:valid:category? (caller check valid/inputs)
  "Ensure CHECK category keyword is valid according to VALID/INPUTS list.

CHECK should be:
  - an internal keyword - prefix `:mis:'
                        - from `int<mis>:keywords:category/internal'
  - a temp keyword      - prefix `:tmp:'
                        - any valid

VALID/INPUTS should be nil (all internal/temp keywords valid), or a list of
valid internal/temp keywords.

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)"
  (let ((caller (list 'int<mis>:valid:category? caller)))
    (unless (keywordp check)
      (int<mis>:error caller
                      "CHECK must be a keyword. Got %S: %S"
                      (type-of check)
                      check))

    (let* ((check/str (symbol-name check))
           (check/len (length check/str)) ; Avoid out of range errorrs in `substring'.
           (type      (substring check/str 0 (min 5 check/len)))) ; ":mis:" or ":tmp:"?

      ;;------------------------------
      ;; Validate & Return
      ;;------------------------------
      ;; Validate `:mis:...'
      (cond ((string= type ":mis:")
             (if (null valid/inputs)
                 ;; Any existing `:mis:...' keyword is valid if VALID/INPUTS is nil.
                 (not (null (memq check int<mis>:keywords:category/internal)))

               ;; Make sure the CHECK keyword belongs in one of the VALID categories.
               (let (validated?)
                 (dolist (valid/input valid/inputs)
                   ;; CHECK must be in at least one of the valid-internals-per-input keyword lists.
                   (setq validated? (or validated?
                                        (not (null (memq check (alist-get valid/input int<mis>:keywords:category)))))))
                 validated?)))

            ;; Validate `:tmp:...'
            ((string= type ":tmp:")
             ;; Any `:tmp:...' keyword is valid if VALID/INPUTS is nil.
             (if (null valid/inputs)
                 :tmp
               ;; Else, it must match, exactly, something in VALID/INPUTS.
               (not (null (memq check valid/inputs)))))

            ;; Fallthrough: Error.
            (t
             (int<mis>:error caller
                             "CHECK must start with %S or %S. Got: %S from %S"
                             "mis"
                             "tmp"
                             type
                             check/str))))))
;; (int<mis>:valid:category? 'test :mis:style nil)
;; (int<mis>:valid:category? 'test :mis:style '(:style :tmp:bar :tmp:foo))
;; (int<mis>:valid:category? 'test :tmp:foo nil)
;; (int<mis>:valid:category? 'test :tmp:foo '(:tmp:bar :tmp:foo))
;; (int<mis>:valid:category? 'test :tmp:foo '(:tmp:bar))


(defun int<mis>:valid:validator (caller category keyword)
  "Determine which category KEYWORD falls under, return validator func for it.

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)

Optional CATEGORY should be:
  - nil                - All categories are valid.
  - a keyword          - Only this category's keywords are valid
  - a list of keywords - Only these categories' keywords are valid

Signal an error if invalid; if valid, return cons 2-tuple of:
  (category . validator-fn)"
  (let ((caller (list 'int<mis>:keyword:category caller)))
    ;;------------------------------
    ;; Error Checking
    ;;------------------------------
    ;; Is CATEGORY a valid type?
    (cond ((and (not (null category)) ; nil
                (not (keywordp category)) ; keyword
                (not (and (listp category) ; list of keywords
                          (seq-every-p #'keywordp category))))
           (int<mis>:error caller
                           '("CATEGORY must be nil, a keyword, or a list of keywords. "
                             "Got %S: %S")
                           (type-of category)
                           category))

          ;; Is the single keyword CATEGORY a valid keyword?
          ((and (keywordp category)
                (not (memq category int<mis>:keywords:category/input)))
           (int<mis>:error caller
                           "CATEGORY must be a member of %S or nil. Got: %S"
                           int<mis>:keywords:category/input
                           category))

          ;; Does the list of keywords CATEGORY contain only valid keywords?
          ((and (listp category)
                (not (seq-every-p (lambda (cat)
                                    "Is every CATEGORY keyword valid?"
                                    (memq cat int<mis>:keywords:category/input))
                                  category)))
           (int<mis>:error caller
                           "All members of CATEGORY be a member of %S. CATEGORY: %S"
                           int<mis>:keywords:category/input
                           category))

          ((not (keywordp keyword))
           (int<mis>:error caller
                           "KEYWORD must be a keyword. Got %S: %S"
                           (type-of keyword)
                           keyword))

          ;;------------------------------
          ;; Actual Categories?
          ;;------------------------------
          ((memq keyword int<mis>:keywords:style)
           '(:mis:style . int<mis>:valid:style/kvp?))

          ((memq keyword int<mis>:keywords:comment)
           '(:mis:comment . int<mis>:valid:comment/kvp?))

          ((memq keyword int<mis>:keywords:line)
           ;; Lines get converted into `:mis:format' after parsing, so they're just temporary.
           '(:tmp:line . int<mis>:valid:line/kvp?))

          ;;------------------------------
          ;; Fallthrough: Error
          ;;------------------------------
          (t
           (int<mis>:error caller
                           "Keyword has no known category: %S"
                           keyword)))))


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

NAME should be VALUE's symbol name as a symbol or string.

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)"
  (let ((valids '(:fixed fixed :existing existing :auto auto )))
    (if (or (memq value valids)
            (integerp value))
        value
      (int<mis>:error caller
                      "%S must be an integer or one of: %S. Got %S: %S"
                      (int<mis>:error:name name)
                      valids
                      (type-of value)
                      value))))


;;------------------------------------------------------------------------------
;; Mis Syntax
;;------------------------------------------------------------------------------

(defun int<mis>:valid:syntax? (caller name key tree &optional no-error?)
  "Signal an error if KEY or TREE fails any basic sanity check for Mis syntax.

NAME should be a symbol name as a symbol or string for the error message.

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)

NO-ERROR? should be nil/non-nil. If non-nil, will return nil instead of
signaling an error."
  ;; TREE should be a plist.
  (cond ((not (keywordp key))
         (if no-error?
             nil
           (int<mis>:error caller
                           '("Expected %S to be a keyword. "
                             "Got %S %S for value %S")
                           (int<mis>:error:name name)
                           (type-of key)
                           key
                           tree)))
        ;; TREE's keyword should be a valid one.
        ((not (memq key int<mis>:keywords:category/internal))
         (if no-error?
             nil
           (int<mis>:error caller
                           '("Expected %S to start with a valid Mis keyword. "
                             "Got keyword %S for %S")
                           (int<mis>:error:name name)
                           key
                           tree)))
        ;; TREE should be... something. A list of something.
        ((or (not (listp tree))
             (null (car tree)))
         (if no-error?
             nil
           (int<mis>:error caller
                           '("Expected value for %S to be a list of... something. "
                             "Got %S")
                           key
                           tree)))

        (t
         ;; Valid; return something non-nil.
         :valid)))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'mis-valid)
;;; mis-valid.el ends here

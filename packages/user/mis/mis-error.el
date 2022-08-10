;;; mis-error.el --- Internal; error helpers. -*- lexical-binding: t; -*-
;;
;; Author: Cole Brown <code@brown.dev>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:  2019-10-23
;; Modified: 2022-08-08
;;
;;; Commentary:
;;
;;  Internal; error helpers.
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Error Formatting
;;------------------------------------------------------------------------------

(defun int<mis>:error:name (name)
  "Return NAME as an uppercased string.

NAME should be either:
  - a keyword
  - a string
  - a quoted symbol"
  (cond ((keywordp name)
         ;; Keywords will just be themselves, but strings. Add quotes?
         (format "`%s'" name))

        ((or (stringp name)
             (symbolp name))
         ;; Strings & symbols should be upcased, as we're expecting those to be
         ;; function parameter names. `format' handles these well given "%s"
         ;; formatting spec.
         (upcase (format "%s" name)))

        (t
         ;; `format' so that we can return /something/. Throw in some extras so
         ;; we can maybe track down what's wrong to here.
         (format "¿(%s)?" name))))


(defun int<mis>:error:caller (caller)
  "Return CALLER as a string.

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list/cons of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)"
  (cond ((listp caller)
         (let (names)
           (dolist (name caller)
             (push (int<mis>:error:caller name) names))
           (mapconcat #'identity
                      (nreverse names)
                      " <- ")))
        ((stringp caller)
         caller)
        ((memq (car-safe caller) '(quote function))
         ;; `format' handles quoted things well.
         (format "%s" caller))
        ;; Not sure? Hope `format' handles it ok?
        ;; TODO: Could error but we're in the middle of erroring...
        (t
         (format "%s" caller))))
;; (int<mis>:error:caller '(baz bar foo))


;; TODO: This is the Nub message way. Convert to the mis way of doing concat/newlines?
(defun int<mis>:error:message (message)
  "Format MESSAGE into a string.

MESSAGE should be one of:
  - a string that `format' understands
  - a list of strings
    - will be concatenated without a separator (so use spaces where needed)
  - a list of strings that starts with the `:newlines' keyword
    - will be concatenated with \"\n\"

Return cons 2-tuple of:
  - nil or an error string about MESSAGE arg
  - formatted MESSAGE string"
  ;; Can't really error, since we're in the middle of erroring, so try to stuff
  ;; our error into the returned string. But try even harder not to call
  ;; anything that will error, as we want the caller to be able to error with
  ;; this string we're building.
  (cond ((stringp message)
         (cons nil message))

        ((and (listp message)
              (seq-every-p #'stringp message))
         (cons nil
               ;; Concat, no separator.
               (apply #'concat message)))

        ((and (listp message)
              (keywordp (car message)))
         (cond ((not (eq (car message) :newlines))
                (cons (format "(mis: error message has invalid keyword: %S)"
                              (car message))
                      ;; Haven't checked MESSAGE to make sure it's valid, so
                      ;; just send it as its full list of keywords plus
                      ;; whatever.
                      (format "%S" message)))

               ((not (seq-every-p #'stringp (cdr message)))
                (cons (format "(mis: error message should be strings: %S)"
                              (cdr message))
                      ;; Haven't checked MESSAGE to make sure it's valid, so
                      ;; just send it as its full list of keywords plus
                      ;; whatever.
                      (format "%S" message)))

               (t
                (cons nil
                      ;; Concat with newlines.
                      (mapconcat #'identity
                                 (cdr message)
                                 "\n")))))

        (t
         ;; Dunno what this is.
         (cons (format "(mis: cannot handle error message: %S)"
                       message)
               (format "%S" message)))))


;;------------------------------------------------------------------------------
;; Erroring
;;------------------------------------------------------------------------------

(defun int<mis>:error (caller message &rest args)
  "Signal an error MESSAGE, formatted with ARGS, from CALLER.

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list/cons of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)

MESSAGE should be one of:
  - a string that `format' understands
  - a list of strings
    - will be concatenated without a separator (so use spaces where needed)
  - a list of strings that starts with the `:newlines' keyword
    - will be concatenated with \"\n\"

ARGS should be the `format' ARGS for MESSAGE."
  ;; Raise error with args formatted into the finalized formatting string.
  (apply #'error
         ;; Format CALLER and MESSAGE inputs.
         (let ((msg (int<mis>:error:message message)))
           (format "%s: %s%s"
                   (int<mis>:error:caller caller)
                   (if (car msg)
                       (concat (car msg) "; ")
                     "")
                   (cdr msg)))
         args))


;; TODO:
;; TODO: Move validation to its own file?
;; TODO:

(defconst int<mis>:valid:align/types
  ;; keyword / symbol
  '(:left      left
    :center    center
    :right     right)
  "Valid mis0 `:align' types.")

;;------------------------------------------------------------------------------
;; Keyword Validation
;;------------------------------------------------------------------------------

(defun int<mis>:valid:style/kvp? (caller keyword value)
  "Determine which validation predicate to call for KEYWORD, then call it.

VALUE should be keyword's value, if it has one, or nil if not.

ARGS should be nil or any args the keyword needs to be able to validate its
value using its validator function. For example, `int<mis>:valid:member?' takes
a list of valid values.

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list/cons of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)

Signal an error if invalid; return normalized value if valid."
  (let ((caller (cons 'int<mis>:valid:keyword? caller)))
    ;; Validators will return the valid value, if it's valid.
    (pcase keyword
      ;;------------------------------
      ;; Keyword Validators
      ;;------------------------------
      (:width
       (int<mis>:valid:integer? caller keyword value))

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


;;------------------------------------------------------------------------------
;; Validation "Predicates"
;;------------------------------------------------------------------------------
;; Signal an error on invalid input, instead of just returning nil.

(defun int<mis>:valid:integer? (caller name value &rest _)
  "Signal an error if VALUE is not a integer, else return VALUE.

NAME should be VALUE's symbol name as a symbol or string.

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list/cons of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)"
  (if (integerp value)
      value
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
  - a list/cons of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)"
  (if (stringp value)
      value
    (int<mis>:error caller
                    "%s must be of type string. Got %S: %S"
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
  - a list/cons of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)"
  (if (or (null value) (stringp value))
      value
    (int<mis>:error caller
                    "%s must be nil or of type string. Got %S: %S"
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
  - a list/cons of the above, most recent first
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
  - `:fixed'
  - `:existing'
  - `:auto'
  - a positive integer

NAME should be VALUE's symbol name as a symbol or string.

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list/cons of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)"
  (let ((valids '(:fixed :existing :auto)))
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
;; The End.
;;------------------------------------------------------------------------------
(provide 'mis-error)
;;; mis-error.el ends here

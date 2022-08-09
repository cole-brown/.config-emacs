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
  - a string
  - a quoted symbol"
  ;; `format' handles both expected cases well, as well as pretty much anything
  ;; else.
  (upcase (format "%s" name)))


(defun int<mis>:error:caller (caller)
  "Return CALLER as a string.

CALLER should be calling function's name. It can be either:
  - a string
  - a quoted symbol
  - or a function-quoted symbol"
  (cond ((stringp caller)
         caller)
        ((memq (car-safe caller) '(quote function))
         ;; `format' handles quoted things well.
         (format "%s" caller))
        ;; Not sure? Hope `format' handles it ok?
        ;; TODO: Could error but we're in the middle of erroring...
        (t
         (format "%s" caller))))


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


;;------------------------------------------------------------------------------
;; Erroring "Predicates"
;;------------------------------------------------------------------------------
;; Signal an error on invalid input, instead of just returning nil.

(defun int<mis>:validate:string? (caller name value)
  "Signal an error if VALUE is not a string, else return VALUE.

NAME should be VALUE's symbol name as a symbol or string.

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol"
  (if (stringp value)
      value
    (int<mis>:error caller
                    "%s must be a string. Got a %S: %S"
                    (int<mis>:error:name name)
                    (type-of value)
                    value)))


(defun int<mis>:validate:member? (caller name value valids)
  "Signal an error if VALUE is not a member of VALIDS, else return VALUE.

VALIDS should be a list of valid values for VALUE; will use `memq' to evaluate
membership.

NAME should be VALUE's symbol name as a symbol or string.

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol"
  (if (memq value valids)
      value
    (int<mis>:error caller
                    "%s must be one of: %S. Got a %S: %S"
                    (int<mis>:error:name name)
                    valids
                    (type-of value)
                    value)))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'mis-error)
;;; mis-error.el ends here

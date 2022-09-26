;;; mis-trim.el --- Trim strings -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <code@brown.dev>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2022-09-12
;; Modified:   2022-09-12
;;
;;; Commentary:
;;
;; Trim strings.
;;
;;; Code:


(require 'mis-error)
(require 'mis-valid)
(require 'mis-tree-syntax)
(require 'mis-tree-output)
(require 'mis-style)


;;------------------------------------------------------------------------------
;; Trim Styler
;;------------------------------------------------------------------------------

(defun int<mis>:style:trim (caller string _ style &optional type regex)
  "Trim REGEX of leading and/or trailing strings from STRING.

TYPE should be one of: `:trim', `:trim:left', or `:trim:right'.

REGEX should be nil or regex strings. If nil, it defaults to \"[ \\t\\n\\r]+\".

STRING should be the string to be trimmed.

STYLE should be a Mis Syntax Tree of styling, but this function doesn't care
about it, currently.

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)

Ignoring:
  1) Mis Syntax Tree parameter

Return the trimmed string."
  (let ((caller (list 'int<mis>:style:trim caller)))
    ;;------------------------------
    ;; Sanity Checks
    ;;------------------------------
    (unless (stringp string)
      (int<mis>:error caller
                      "STRING to trim must be a string. Got %S: %S"
                      (type-of string)
                      string))

    ;; TYPE is labeled `&optional' to match the expectations of the styler
    ;; registration, but it is not optional... so make sure it's present.
    ;; But it really should have been validated already so don't bother with the
    ;; whole validation again.
    (unless (keywordp type)
      (int<mis>:error caller
                      "Trim TYPE must be a keyword. Got %S: %S"
                      (type-of regex)
                      regex))

    ;; Optional regex.
    (unless (or (null regex)
                (stringp regex))
      (int<mis>:error caller
                      "Trim REGEX must be nil or a string. Got %S: %S"
                      (type-of regex)
                      regex))

    ;;------------------------------
    ;; Trimming
    ;;------------------------------
    (cond ((eq type :trim)
           (string-trim string regex regex))

          ((eq type :trim:left)
           (string-trim-left string regex))

          ((eq type :trim:right)
           (string-trim-right string regex))

          (t
           (int<mis>:error caller
                           "Unhandled string trim TYPE: %S"
                           type)))))
;; (int<mis>:style:trim 'test "   hello?   " nil :trim)
;; (int<mis>:style:trim 'test "   hello?   " nil :trim (rx (one-or-more (or " " "?"))))
;; (int<mis>:style:trim 'test "   hello?   " nil :trim:left (rx (one-or-more (or " " "?"))))
;; (int<mis>:style:trim 'test "   hello?   " nil :trim:right (rx (one-or-more (or " " "?"))))


;; Register the trim styler for all trim keywords.
(int<mis>:styler:register :trim       #'int<mis>:style:trim)
(int<mis>:styler:register :trim:left  #'int<mis>:style:trim)
(int<mis>:styler:register :trim:right #'int<mis>:style:trim)


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'mis-trim)
;;; mis-trim.el ends here

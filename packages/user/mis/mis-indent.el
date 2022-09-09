;;; mis-indent.el --- Intendation -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <code@brown.dev>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2019-10-23
;; Modified:   2022-09-09
;;
;;; Commentary:
;;
;; Indent strings.
;;
;; Valid for `:indent':
;;   - `:fixed' / `fixed'
;;   - `:existing' / `existing'
;;   - `:auto' / `auto'
;;   - a positive integer
;;   - a string
;;
;; Creating the indent string:
;;   - For the integer/symbol indent, use `:padding' or a space (" ").
;;   - For the string indent, use as-is.
;;
;;; Code:


(require 'mis-error)
(require 'mis-valid)
(require 'mis-style)
(require 'mis-format)


;;------------------------------------------------------------------------------
;; Indentation
;;------------------------------------------------------------------------------

(defun int<mis>:indent:string (string indent padding)
  "Indent STRING according to INDENT & PADDING.

INDENT should be one of:
  - any positive integer
  - a string
  - a member of `int<mis>:valid:indent/types'
    - it should be normalized to a non-keyword member

PADDING /must/ be a string of length 1 and will only be used if INDENT is an
integer.

Return indented string."
  (declare (pure t) (side-effect-free t))

  ;;------------------------------
  ;; Indent a specific amonut?
  ;;------------------------------
  (cond ((stringp indent)
         ;; Already ready; return as-is.
         indent)

        ((integerp indent)
         ;; Want to create an amount of indent, so we'll need padding...
         (make-string indent (string-to-char padding)))

        ((eq indent 'fixed)
         (int<mis>:format:repeat padding (current-column)))


        ;;------------------------------
        ;; Indent to a specific place?
        ;;------------------------------
        ;; `existing' means:
        ;;   a) the indentation is already there
        ;;   b) find out what it is
        ;;   c) don't create it
        ((eq indent 'existing)
         ;; We don't care about finding out what it is, so... just... don't indent at all.
         "")

        ;; TODO: Implement `auto' if I end up needing it.
        ;; `auto' means: "Do what `indent-according-to-mode' does, but to the Mis
        ;; String, not to the buffer's contents."
        ((eq indent 'auto)
         ;; TODO: Implement `auto' if I end up needing it.
         (int<mis>:error 'int<mis>:indent:string
                         "TODO: Implement `auto' indentation!"))))


(defun int<mis>:style:indent (caller string style &optional _ indentation)
  "Indent STRING based INDENTATION.

STRING should be the string to be indented.

STYLE should be a Mis Syntax Tree of styling.
  - Optional:
    - `:padding'
      - Must be a character or a string of length 1.
      - Used to create the indent padding string if needed.
      - If not supplied, it will default to a space (\" \").

INDENTATION should be one of:
  - any positive integer
  - a string
  - a member of `int<mis>:valid:indent/types'
    - it should be normalized to a non-keyword member

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)

Return an indented string."
  (declare (pure t) (side-effect-free t))

  (let* ((caller  (list 'int<mis>:style:indent caller))
         (padding (int<mis>:style:padding caller style " ")))

    ;;------------------------------
    ;; Error Checks
    ;;------------------------------
    ;; `padding' error checked in its getter.

    ;; These will signal an error if invalid.
    (int<mis>:valid:string? caller
                            'string
                            string)
    (int<mis>:valid:indent? caller
                            'indentation
                            indentation)

    ;;------------------------------
    ;; Indent String & Return
    ;;------------------------------
    (apply #'int<mis>:string:lines/affix
           (int<mis>:indent:string string indentation padding)
           nil
           (int<mis>:string:lines/split string))))
;; (int<mis>:style:indent 'test "hello" nil :indent "xyz: ")


;; Register the indent styler.
(int<mis>:styler:register :indent #'int<mis>:style:indent)


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'mis-indent)
;;; mis-indent.el ends here

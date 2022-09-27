;;; mis-indent.el --- Intendation -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <code@brown.dev>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2022-09-09
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
(require 'mis-tree-string)
(require 'mis-tree-syntax)
(require 'mis-tree-output)
(require 'mis-style)
(require 'mis-format)


;;------------------------------------------------------------------------------
;; Indentation Helpers
;;------------------------------------------------------------------------------

(defun int<mis>:indent:amount (indentation &optional buffer position)
  "Figure out indentation for BUFFER at POSITION according to INDENTATION.

INDENTATION should be:
  `fixed'    -> indent to `current-column'
  `existing' -> do not indent; use `current-column' as indent amount.
  `auto'     -> indent according to `indent-according-to-mode'
  an integer -> that number of spaces

BUFFER should be:
  - nil -> the `current-buffer'
  - a buffer object or a buffer name string

POSITION should be:
  - nil -> the current position in BUFFER
  - an integer or buffer marker

Return indentation amount as an integer (number of padding characters)."
  ;;------------------------------
  ;; Error Checks
  ;;------------------------------
  (when (and (not (null buffer))
             (not (stringp buffer))
             (not (bufferp buffer)))
    (int<mis>:error 'int<mis>:format:indent:amount
                    "BUFFER must be nil, a string, or a buffer object. Got a %S: %S"
                    (type-of buffer)
                    buffer))

  (when (and (not (null position))
             (not (integer-or-marker-p position)))
    (int<mis>:error 'int<mis>:format:indent:amount
                    "POSITION must be nil, an integer, or a marker. Got a %S: %S"
                    (type-of position)
                    position))

  ;; `int<mis>:valid:indent?' will signal error for invalid.
  (int<mis>:valid:indent? 'int<mis>:format:indent:amount
                          'indentation
                          indentation)

  ;; Make sure `buffer' is a buffer object now.
  (let ((buffer (if buffer
                    (get-buffer buffer)
                  (current-buffer))))

    ;;------------------------------
    ;; Determine Indentation & Return Amount
    ;;------------------------------
    (cond ((integerp indentation)
           indentation)

          ((stringp indentation)
           (length indentation))

          ((memq indentation int<mis>:valid:indent/types)
           ;; Else need to ask the buffer about indentation.
           (let (amount)
             (with-current-buffer buffer/curr
               (save-excursion
                 ;; Want to back out our changes as we're only interested in the indent
                 ;; _amount_ right now.
                 (let ((change-group (prepare-change-group)))
                   (goto-char position)
                   (beginning-of-line-text)

                   ;; `:fixed' and `:existing' have nothing to do; they're already at the
                   ;; correct column, presumably. `:auto' needs to figure out what the
                   ;; buffer's mode wants.
                   (when (eq indent :auto)
                     (indent-according-to-mode)
                     (beginning-of-line-text))

                   ;; Save indent amount.
                   (setq amount (current-column))

                   ;; Unilaterally cancel any changes made.
                   (cancel-change-group change-group))))

             ;; Done; return the indentation amount.
             amount))

          ;;------------------------------
          ;; ???
          ;;------------------------------
          (t
           (int<mis>:error 'int<mis>:indent:string
                           "Unhandled indentation %S!"
                           indentation)))))


(defun int<mis>:indent:string (string indentation padding &optional buffer position)
  "Indent STRING according to INDENTATION & PADDING.

INDENTATION should be:
  `fixed'    -> indent to `current-column'
  `existing' -> do not indent; use `current-column' as indent amount.
  `auto'     -> indent according to `indent-according-to-mode'
  an integer -> that number of spaces

PADDING /must/ be a string of length 1 and will only be used if INDENTATION is
an integer.

BUFFER should be:
  - nil -> the `current-buffer'
  - a buffer object or a buffer name string

POSITION should be:
  - nil -> the current position in BUFFER
  - an integer or buffer marker

Return indentation string."
  ;;------------------------------
  ;; Build & Return Indentation String
  ;;------------------------------
  (cond ((stringp indentation)
         ;; Already ready; return as-is.
         indentation)

        ((integerp indentation)
         ;; Want to create an amount of indentation, so we'll need padding...
         (make-string indentation (string-to-char padding)))

        ;; `existing' means:
        ;;   a) the indentation is already there
        ;;   b) don't create it
        ((eq indentation 'existing)
         ;; We don't care about finding out what it is, so... just... don't indent at all.
         "")

        ;; `fixed' and `auto' can both just use `int<mis>:indent:amount' to
        ;; figure out their indentation.
        ((eq indentation 'fixed)
         (int<mis>:format:repeat padding
                                 (int<mis>:indent:amount indentation buffer position)))

        ;;------------------------------
        ;; ???
        ;;------------------------------
        (t
         (int<mis>:error 'int<mis>:indent:string
                         "Unhandled indentation %S!"
                         indentation))))


;;------------------------------------------------------------------------------
;; Indentation Styler
;;------------------------------------------------------------------------------

(defun int<mis>:style:indent (caller string _ style &optional _ indentation)
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

Ignoring:
  1) Mis Syntax Tree parameter
  2) `:align' keyword parameter

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

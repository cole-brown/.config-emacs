;;; mis-format.el --- String Formatting for Mis -*- lexical-binding: t; -*-
;;
;; Author: Cole Brown <code@brown.dev>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:  2019-10-23
;; Modified: 2022-08-08
;;
;;; Commentary:
;;
;;  Make (That String Formatted Just) So.
;;
;;; Code:


(require 'cl-lib)

(require 'mis-error)
(require 'mis-valid)


;;------------------------------------------------------------------------------
;; Basic Formatting
;;------------------------------------------------------------------------------

(defun int<mis>:format:message (caller message &rest args)
  "Format MESSAGE & ARGS in preparation for 'mis' formatting operations.

CALLER should be calling function's name. It can be either:
  - a string
  - a quoted symbol
  - or a function-quoted symbol

MESSAGE should be a string that `format' understands.

ARGS should be the `format' ARGS for MESSAGE."
  (cond ((not (stringp message))
         (int<mis>:error caller
                         "MESSAGE must be a string; got %S: %S"
                         (type-of message)
                         message))

        ;; Ok; format and return.
        (t
         (apply #'format message args))))


;;------------------------------------------------------------------------------
;; Trim / Truncate
;;------------------------------------------------------------------------------

(cl-defun int<mis>:format:trim (caller string &key trim trim:left trim:right)
  "Trim STRING of leading and trailing strings matching TRIM-LEFT and TRIM-RIGHT.

TRIM, TRIM:LEFT, and TRIM:RIGHT should be nil or regex strings. They default to
\"[ \\t\\n\\r]+\". TRIM is for both left and right, and will raise an error if
specified with TRIM:LEFT and/or TRIM:RIGHT.

CALLER should be calling function's name. It can be either:
  - a string
  - a quoted symbol
  - or a function-quoted symbol"
  ;;------------------------------
  ;; Error Checks
  ;;------------------------------
  (cond ((and trim trim:left trim:right)
         (int<mis>:error caller
                         '("TRIM:LEFT & TRIM:RIGHT cannot be specified with TRIM. "
                           "Got TRIM: %S, TRIM:LEFT: %S, TRIM:RIGHT: %S")
                         trim
                         trim:left
                         trim:right))
        ((and trim trim:left)
         (int<mis>:error caller
                         '("TRIM:LEFT cannot be specified with TRIM. "
                           "Got TRIM: %S, TRIM:LEFT: %S")
                         trim
                         trim:left))
        ((and trim trim:right)
         (int<mis>:error caller
                         '("TRIM:RIGHT cannot be specified with TRIM. "
                           "Got TRIM: %S, TRIM:RIGHT: %S")
                         trim
                         trim:right))
        ((not (stringp string))
         (int<mis>:error caller
                         "STRING to trim must be a string. Got a %S: %S"
                         (type-of string)
                         string))

        ;;------------------------------
        ;; Trimming
        ;;------------------------------
        ;; Trim left & right differently.
        ((and trim:left
              trim:right)
         (string-trim string trim:left trim:right))

        (trim:left
         (string-trim-left string trim:left))

        (trim:right
         (string-trim-right string trim:right))

        ;; Trim both sides the same. Either both default or both with supplied
        ;; TRIM regex.
        (t
         (string-trim string trim trim))))
;; (int<mis>:format:trim "bob" "   hello?   ")
;; (int<mis>:format:trim "bob" "   hello?   " :trim (rx (one-or-more (or " " "?"))))
;; (int<mis>:format:trim "bob" "   hello?   " :trim:left (rx (one-or-more (or " " "?"))))
;; (int<mis>:format:trim "bob" "   hello?   " :trim:right (rx (one-or-more (or " " "?"))))
;; (int<mis>:format:trim "bob" "   hello?   " :trim:left (rx (one-or-more (or " " "h"))) :trim:right (rx (one-or-more (or " " "?"))))
;; (int<mis>:format:trim "bob" 'hello)


;;------------------------------------------------------------------------------
;; Indentation
;;------------------------------------------------------------------------------

(defun int<mis>:format:indent:amount (buffer position type)
  "Figure out indentation for BUFFER at POSITION according to indent TYPE.

BUFFER should be a buffer object or a buffer name string.

POSITION should be an integer or buffer marker.

TYPE can be:
  :fixed     -> indent to `current-column'
  :existing  -> do not indent; use `current-column' as indent amount.
  :auto      -> indent according to `indent-according-to-mode'
  an integer -> that number of spaces

Return indentation as an integer (number of spaces)."
  (let (buffer/curr
        indent/amount)
    ;;------------------------------
    ;; Error Checks
    ;;------------------------------
    (cond ((and (not (stringp buffer))
                (not (bufferp buffer)))
           (int<mis>:error 'int<mis>:format:indent:amount
                           "BUFFER must be a string or buffer object. Got a %S: %S"
                           (type-of buffer)
                           buffer))

          ((not (integer-or-marker-p position))
           (int<mis>:error 'int<mis>:format:indent:amount
                           "POSITION must be an integer or marker. Got a %S: %S"
                           (type-of position)
                           position))

          ;; `int<mis>:valid:indent?' will signal error for invalid.
          ((not (int<mis>:valid:indent? 'int<mis>:format:indent:amount
                                        'type
                                        type)))

          (t
           (setq buffer/curr (get-buffer buffer))
           (unless buffer/curr
             (int<mis>:error 'int<mis>:format:indent:amount
                             "Could not find BUFFER: %S"
                             buffer))))

    ;;------------------------------
    ;; Determine Indentation
    ;;------------------------------
    (if (integerp type)
        (setq indent/amount type)
      ;; Else need to ask the buffer about indentation.
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
            (setq indent/amount (current-column))

            ;; Unilaterally cancel any changes made.
            (cancel-change-group change-group)))))

    ;; Return discovered indent amount.
    indent/amount))


;;------------------------------------------------------------------------------
;; Lines
;;------------------------------------------------------------------------------

(cl-defun int<mis>:format:repeat (character length)
  "Create a line separators or what have you, like:
--------

CHARACTER should be a character or string of length 1.

LENGTH should be an integer greater than zero."
  ;;------------------------------
  ;; Errors
  ;;------------------------------
  (cond ((and (not (stringp character))
              (not (characterp character)))
         (int<mis>:error 'int<mis>:format:repeat
                         '("CHARACTER must be a string or character. "
                           "Got a %S: %S")
                         (type-of character)
                         character))

        ((not (integerp length))
         (int<mis>:error 'int<mis>:format:repeat
                         '("LENGTH must be an integer. "
                           "Got a %S: %S")
                         (type-of length)
                         length))
        ;;------------------------------
        ;; "Long" Strings
        ;;------------------------------
        ((and (stringp character)
              (not (= (length character) 1)))
         ;; Repeat multi-char string until at least a LENGTH string is created.
         (let (repeats)
           ;; `ceiling' / `float' shenanigans to ensure string created is long enough.
           (dotimes (i (ceiling (/ (float length) (length character))))
             (setq repeats (cons character repeats)))
           ;; Now maybe truncate down to length.
           (substring (apply 'concat repeats)
                      0
                      length)))

        ;;------------------------------
        ;; Chars / 1 Char Strings
        ;;------------------------------
        (t
         (make-string length
                      (if (stringp character)
                          (string-to-char character)
                        character)
                      :multibyte))))
;; (int<mis>:format:repeat "x" 5)
;; (int<mis>:format:repeat ?x 6)
;; (int<mis>:format:repeat "xx" 5)
;; (int<mis>:format:repeat "xx" 6)


;;------------------------------------------------------------------------------
;; Output Builder
;;------------------------------------------------------------------------------

(defun int<mis>:compile:format (caller ast style)
  "Format Mis syntax plist AST using Mis STYLE plist; return string.

AST should be a `:format' value plist.
Example: (:formatter repeat :string \"-\")

STYLE should be nil a `:style' value plist.
Example: (:width 80)

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)"
  (let* ((func/name  (list 'int<mis>:format:compile caller))
         (formatter  (plist-get ast :formatter)))
    ;;------------------------------
    ;; Sanity Checks
    ;;------------------------------
    (cond ((null formatter)
           (int<mis>:error func/name
                           '("AST should have some sort of `:formatter'? "
                             "Got %S from %S")
                           formatter
                           ast))

          (t
           nil))

    ;;------------------------------
    ;; Format output string.
    ;;------------------------------
    (pcase formatter
      ('repeat
       ;; Build & return the repeated string.
       (int<mis>:format:repeat (plist-get ast :string)
                               (or (plist-get style :width)
                                   fill-column)))

      (_
       (int<mis>:error func/name
                       '("Unhandled formatter case `%S'!")
                       formatter)))))
;; (int<mis>:compile:format 'test '(:formatter repeat :string "-") '(:width 80))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'mis-format)
;;; mis-format.el ends here

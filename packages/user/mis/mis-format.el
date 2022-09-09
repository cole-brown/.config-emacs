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
;; Lines
;;------------------------------------------------------------------------------

(defun int<mis>:format:repeat (character length)
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

(defun int<mis>:compile:format (caller syntax style)
  "Format Mis SYNTAX Tree using STYLE; return string.

SYNTAX should be `:format' syntax tree.
Example: '((:format (:formatter . repeat) (:string . \"-\")))

STYLE should be nil or a `:style' syntax tree.
Example: (mis:style :width 80) -> '((:style (:width . 80)))

Only STYLE will be checked for styling; style in SYNTAX is ignored.

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)"
  (let* ((caller    (list 'int<mis>:compile:format caller))
         (formatter (int<mis>:syntax:find caller
                                          syntax
                                          :format :formatter))
         (value (int<mis>:syntax:find caller
                                      syntax
                                      :format :value)))
    (int<mis>:debug caller "formatter: %S" formatter)
    (int<mis>:debug caller "value:     %S" value)

    ;;------------------------------
    ;; Sanity Checks
    ;;------------------------------
    (cond ((null formatter)
           (int<mis>:error caller
                           '("SYNTAX should have some sort of `:formatter'? "
                             "Got %S from %S")
                           formatter
                           syntax))

          (t
           nil))

    ;;------------------------------
    ;; Do we have the output string?
    ;;------------------------------
    ;; If the value is `child', we first have to go get a string from our children.
    (when (eq value 'child)
      (int<mis>:debug caller "Get string from child...")
      (int<mis>:debug caller
                      "children: %S"
                      (int<mis>:syntax:find caller
                                            syntax
                                            :format :children))
      (setq value (int<mis>:compile caller
                                    (int<mis>:syntax:find caller
                                                          syntax
                                                          :format :children)
                                    style))
      (int<mis>:debug caller "<--value:  %S" value))

    ;;------------------------------
    ;; Format output string.
    ;;------------------------------
    (pcase formatter
      ('repeat
       ;; Build & return the repeated string.
       (int<mis>:format:repeat value
                               (int<mis>:style:width caller style)))

      ('message
       (apply #'int<mis>:format:message
              caller
              value))

      ('string
       (if (stringp value)
           value
         (int<mis>:error caller
                         "`string' formatter expected string but got %S: %S"
                         (type-of value)
                         value)))

      ('char
       (if (characterp value)
           (make-string 1 value)
         (int<mis>:error caller
                         "`char' formatter expected character(/integer) but got %S: %S"
                         (type-of value)
                         value)))

      (_
       (int<mis>:error caller
                       '("Unhandled formatter case `%S'!")
                       formatter)))))
;; (int<mis>:compile:format 'test '((:format (:formatter . repeat) (:value . "-"))) '((:style (:width . 80))))
;; (int<mis>:compile:format 'test (mis:line "-") (mis:style :width 80))

(int<mis>:compiler:register :format #'int<mis>:compile:format)


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'mis-format)
;;; mis-format.el ends here

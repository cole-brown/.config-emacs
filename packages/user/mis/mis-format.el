;;; mis-format.el --- String Formatting for Mis -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <code@brown.dev>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2019-10-23
;; Timestamp:  2023-06-29
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;  Make (That String Formatted Just) So.
;;
;;; Code:


(require 'cl-lib)

(require 'mis-error)
(require 'mis-valid)
(require 'mis-tree-syntax)
(require 'mis-tree-output)


;;------------------------------------------------------------------------------
;; Basic Formatting
;;------------------------------------------------------------------------------

(defun int<mis>:format:string (caller string &rest args)
  "Format STRING & ARGS in preparation for 'mis' formatting operations.

CALLER should be calling function's name. It can be either:
  - a string
  - a quoted symbol
  - or a function-quoted symbol

STRING should be a string that `format' understands.

ARGS should be the `format' ARGS for STRING."
  (cond ((not (stringp string))
         (int<mis>:error caller
                         "STRING must be a string; got %S: %S"
                         (type-of string)
                         string))

        ;; Ok; format and return.
        (t
         (apply #'format string args))))


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
;; Compiler
;;------------------------------------------------------------------------------

(defun int<mis>:compile:format (caller syntax style)
  "Format Mis SYNTAX Tree using STYLE; return a Mis Output Tree.

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
                                      :format :value))
         value/metadata
         output/string)
    (int<mis>:debug caller "syntax:    %S" syntax)
    (int<mis>:debug caller "style:     %S" style)
    (int<mis>:debug caller "formatter: %S" formatter)
    (int<mis>:debug caller "value:     %S" value)

    ;;------------------------------
    ;; Sanity Checks
    ;;------------------------------
    (unless formatter
      (int<mis>:error caller
                      '("SYNTAX should have some sort of `:formatter'? "
                        "Got %S from %S")
                      formatter
                      syntax))

    ;;------------------------------
    ;; Process syntax enough to get an output string from it.
    ;;------------------------------
    ;; If the value is `child', we first have to go get a string from our children.
    (when (eq value 'child)
      (int<mis>:debug caller "Get string from child...")
      (int<mis>:debug caller
                      "children: %S"
                      (int<mis>:syntax:find caller
                                            syntax
                                            :format :children))
      (setq value (int<mis>:compile:children caller :format syntax style))
      (int<mis>:debug caller "<--value:  %S" value))

    ;; If value is a Mis Output Tree, need to finalize it down to a single string.
    (when (int<mis>:valid:output? caller 'value value :no-error)
      (let* ((mot (int<mis>:output:finalize/block caller value))
             (mot/entries (int<mis>:output:get/entries caller mot))
             (mot/entry/0 (nth 0 mot/entries)))
        ;; Sanity checks & extract string.
        (when (> (length mot/entries) 1)
          (int<mis>:error caller
                          '("Finalized Mis Output Tree has more than one entry?! "
                            "mot: %S")
                          mot))
        (setq value          (int<mis>:output:get/string   caller mot/entry/0)
              value/metadata (int<mis>:output:get/metadata caller mot/entry/0))))

    ;;------------------------------
    ;; Format output string.
    ;;------------------------------
    (setq output/string
          (pcase formatter
            ('repeat
             ;; Build & return the repeated string.
             (int<mis>:format:repeat value
                                     (int<mis>:style:get/width caller style nil nil)))

            ('format
             (apply #'int<mis>:format:string
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
                             formatter))))
    (int<mis>:debug caller "formatted string: %S" output/string)

    ;;------------------------------
    ;; Style formatted string & return.
    ;;------------------------------
    (let ((output/unstyled (int<mis>:output caller
                                            output/string
                                            (int<mis>:syntax:merge/style caller
                                                                         :format
                                                                         syntax
                                                                         style)
                                            value/metadata))
          output/styled)
      (int<mis>:debug caller "output/unstyled:  %S" output/unstyled)
      (setq output/styled (int<mis>:style caller
                                          output/unstyled
                                          :format
                                          syntax
                                          style))
      (int<mis>:debug caller "<--output/styled: %S" output/styled)
      output/styled)))
;; (int<mis>:compile:format 'test '((:format (:formatter . repeat) (:value . "-"))) '((:style (:width . 80))))
;; (int<mis>:compile:format 'test (mis:line "-") (mis:style :width 80))


(int<mis>:compiler:register :format #'int<mis>:compile:format)


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'mis-format)
;;; mis-format.el ends here

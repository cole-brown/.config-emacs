;;; mis-string.el --- Strings and things. -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <code@brown.dev>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2019-10-23
;; Modified:   2022-08-25
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Strings and things.
;;
;;; Code:


(require 'mis-error)
(require 'mis-valid)
(require 'mis-parse)


;;------------------------------------------------------------------------------
;; Helper Functions
;;------------------------------------------------------------------------------

(defun int<mis>:string:split/lines (string)
  "Split STRING on newline character into a list on strings."
  (split-string string "\n"))


(defun int<mis>:string:join/lines (&rest string)
  "Join STRINGs with newline character."
  (mapconcat #'identity
             string
             "\n"))

(defun int<mis>:string:combine/lines (prefix postfix &rest line)
  "Append PREFIX and POSTFIX to each LINE, then join with newlines into a string."
  (mapconcat (lambda (str) "Combine prefix, str, and postfix."
               (concat (or prefix "") str (or postfix "")))
             line
             "\n"))
;; (int<mis>:string:combine/lines ";; " "" "foo" "bar" "baz")


;;------------------------------------------------------------------------------
;; Output Builder
;;------------------------------------------------------------------------------

(defun int<mis>:compile:string (caller syntax style)
  "Compile `:mis:string' SYNTAX using STYLE; return string.

SYNTAX should be `:mis:string' syntax tree.

STYLE should be nil or a `:mis:style' syntax tree.
Example: (mis:style :width 80) -> '((:mis:style (:width . 80)))

Only STYLE will be checked for styling; style in SYNTAX is ignored.

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)"
  (let* ((caller (list 'int<mis>:compile:string caller)))

    ;;------------------------------
    ;; Format into a string.
    ;;------------------------------
    ;; TODO: Style: Align? Indent? Trim? Etc?
    (int<mis>:format:syntax caller syntax)))


(int<mis>:register:compiler :mis:string  #'int<mis>:compile:string)
(int<mis>:register:compiler :mis:message #'int<mis>:compile:string)
(int<mis>:register:compiler :mis:char    #'int<mis>:compile:string)


;;------------------------------------------------------------------------------
;; Output API
;;------------------------------------------------------------------------------

(defun mis:string (&rest args)
  "Parse ARGS into a Mis message/string syntax tree.

ARGS should start off with styling key/values before supplying
the format string and format args. Example:
  Valid:
    (mis:string :type 'inline \"hello world\")
    (mis:string :type 'inline :language 'emacs-lisp \"hello %s\" (get-greeted))
  Invalid:
    (mis:string \"hello %s\" :type 'inline (get-greeted))
    (mis:string \"hello %s\" (get-greeted) :type 'inline :language 'emacs-lisp)

NOTE: The \"invalids\" will just be interpreted as having extra message string
formatting args.

NOTE: Mis keyword args must always have both a keyword and a value."
  (apply 'int<mis>:parse
         'mis:string
         '(:style) ; Also allow styling in our string.
         args))
;; (mis:string "hello %S" "there")
;; (mis:string :width 80 "hello there")


(defalias 'mis:message 'mis:string)


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'mis-string)
;;; mis-string.el ends here

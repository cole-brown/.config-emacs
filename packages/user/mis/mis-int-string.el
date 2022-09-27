;;; mis-int-string.el --- String Manipulation Functions -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <code@brown.dev>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2022-09-27
;; Modified:   2022-09-27
;;
;;; Commentary:
;;
;;  String Manipulation Functions
;;
;;; Code:


(require 'mis-error)
(require 'mis-valid)


;;------------------------------------------------------------------------------
;; Helper Functions
;;------------------------------------------------------------------------------

(defun int<mis>:string:lines/split (string)
  "Split STRING on newline character into a list on strings."
  (split-string string
                "\n"
                nil)) ;; Must not OMIT-NULLs here as we want to preserve empty lines.
;; (int<mis>:string:lines/split "\nhello\nthere\n")


(defun int<mis>:string:lines/join (&rest string)
  "Join STRINGs with newline character."
  (mapconcat #'identity
             string
             "\n"))

(defun int<mis>:string:affix (prefix postfix string)
  "Attach PREFIX and POSTFIX to STRING."
  (concat (or prefix "") string (or postfix "")))
;; (int<mis>:string:affix ";; " nil "foo")


(defun int<mis>:string:lines/affix (prefix postfix &rest string)
  "Attach PREFIX and POSTFIX to each line in each STRING.

Splits each STRING into lines."
  (mapconcat (lambda (each) "Split EACH into lines, do prefix/postfix."
               (mapconcat (lambda (line) "Combine LINE with prefix & postfix."
                            (int<mis>:string:affix prefix postfix line))
                          (int<mis>:string:lines/split each)
                          "\n"))
             string
             "\n"))
;; (int<mis>:string:lines/affix ";; " "" "foo" "bar" "baz\nqux")


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'mis-int-string)
;;; mis-int-string.el ends here

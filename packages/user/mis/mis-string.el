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
;; The End.
;;------------------------------------------------------------------------------
(provide 'mis-string)
;;; mis-string.el ends here

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
(require 'mis-tree-syntax)
(require 'mis-tree-output)
(require 'mis-parse)


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
         :string
         '(:string :style) ; Also allow styling in our string.
         args))
;; (mis:string "hello %S" "there")
;; (mis:string :width 80 "hello there")
;; (mis:string :width 80 "hello %S" "there")


(defalias 'mis:message 'mis:string)


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'mis-string)
;;; mis-string.el ends here

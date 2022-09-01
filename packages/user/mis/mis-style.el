;;; mis-style.el --- String styling for Mis -*- lexical-binding: t; -*-
;;
;; Author: Cole Brown <code@brown.dev>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:  2019-10-23
;; Modified: 2022-08-09
;;
;;; Commentary:
;;
;; String styling for Mis.
;;
;; Set up styling for anything internal to the `mis:style` call.
;; E.g.:
;;   (mis "Hello, "
;;        (mis:style :bold "world")
;;        ".")
;;   -> "world" styled as bold.
;;
;;   (mis (mis:style '(:align 'center :width 80)
;;                   "Hello, "
;;                   (mis:style :bold "world")
;;                   "."))
;;   -> "world" styled as bold.
;;   -> "Hello world." aligned to center (of the 80 width).
;;
;;; Code:


(require 'cl-lib)

(require 'mis-error)
(require 'mis-valid)
(require 'mis-parse)


;;------------------------------------------------------------------------------
;; Style Helpers
;;------------------------------------------------------------------------------

(defun int<mis>:style:exclusive? (syntax)
  "Return SYNTAX if SYNTAX is _only_ styling; return nil otherwise."
  ;; Must have valid syntax.
  (cond ((not (int<mis>:valid:syntax? 'int<mis>:style:exclusive?
                                      'syntax
                                      syntax
                                      :no-error))
         nil)

        ;; (Alist) syntax should contain only 1 element.
        ((not (eq 1 (length syntax)))
         nil)

        ;; Key should be `:style'.
        ((int<mis>:syntax:find 'int<mis>:style:exclusive?
                               syntax
                               :style)
         syntax)

        ;; Fallthrough: not styling so return nil.
        (t
         nil)))
;; (int<mis>:style:exclusive? (mis:style :width 80))
;; (int<mis>:style:exclusive? '((:style (:align . center)) (:string . "hello")))


;;------------------------------------------------------------------------------
;; API: Styling
;;------------------------------------------------------------------------------

(defun mis:style (&rest args)
  "Validate ARGS and return a Mis style list.

ARGS should start off with styling key/values before supplying
the format string and format args. Example:
  Valid styles:
    (mis:style :bold t \"hello world\")
    (mis:style :bold t :align 'center \"hello %s\" (get-greeted))
  Invalid styles:
    (mis:style \"hello %s\" :bold :align 'center (get-greeted))
    (mis:style \"hello %s\" (get-greeted) :bold :align 'center)

NOTE: The \"invalid styles\" will just be interpreted as having no styling and
extra message args.

NOTE: Styles must always have both a keyword and a value."
  (apply 'int<mis>:parse
         'mis:style
         :style
         :style ; Only allow style keywords.
         args))
;; (mis:style :align 'center "hello")
;; (mis:style :align 'center "hello %s" "world")
;; (mis:style :align 'center :width 11 "hello %s" "world")
;; (mis:style :align 'center :width 11 "hello")
;; (mis:style :indent 'auto "hello")


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'mis-style)
;;; mis-style.el ends here

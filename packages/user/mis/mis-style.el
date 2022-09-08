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


(defun int<mis>:style:width (caller style &optional default)
  "Return `:width' from STYLE, or default.

STYLE should be a Mis Syntax Tree of styling.
  - Optional:
    - `:width'
      - Must be a positive integer.
      - If it is not provided, will try to use DEFAULT.
        - If DEFAULT is not provided, will use buffer's `fill-column'.

Must be called in the context of the targeted output buffer so that
`fill-column' can be correct.

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)"
  (or (int<mis>:syntax:find caller
                            style
                            :style :width)
      ;; Fallback to default if it's an integer > 0.
      (and (integerp default)
           (> default 0)
           default)
      ;; Fallback to buffer's `fill-column'.
      fill-column))


(defun int<mis>:style:padding (caller style &optional default)
  "Return `:padding' from STYLE, or DEFAULT.

STYLE should be a Mis Syntax Tree of styling.
  - Optional:
    - `:padding'
      - Must be a character or a string of length 1.
      - If not supplied, it will default to DEFAULT.
        - If not supplied, it will default to a space (\" \").

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)"
  (let ((padding (int<mis>:syntax:find caller
                                       style
                                       :style :padding)))

    ;;------------------------------
    ;; Error Checks & Fallbacks
    ;;------------------------------
    (cond ((and (not (null padding))
                (not (stringp padding))
                (not (characterp padding)))
           (int<mis>:error 'int<mis>:align
                           "PADDING must be nil, a character, or a string of length 1. Got a %S: %S"
                           (type-of padding)
                           padding))

          ((and (stringp padding)
                (not (= (length padding) 1)))
           (int<mis>:error 'int<mis>:align
                           '("PADDING must be nil, a character, or a string of length 1. "
                             "Got a string of length %S: %S")
                           (length padding)
                           padding))

          ;;---
          ;; Initial Fallback: DEFAULT
          ;;---
          ((and (null padding)
                (not (null default)))
           ;; Fallback to DEFAULT, and do the same error checks on it.
           (setq padding default)
           (cond ((and (not (null padding))
                       (not (stringp padding))
                       (not (characterp padding)))
                  (int<mis>:error 'int<mis>:align
                                  "PADDING must be nil, a character, or a string of length 1. Got a %S: %S"
                                  (type-of padding)
                                  padding))

                 ((and (stringp padding)
                       (not (= (length padding) 1)))
                  (int<mis>:error 'int<mis>:align
                                  '("PADDING must be nil, a character, or a string of length 1. "
                                    "Got a string of length %S: %S")
                                  (length padding)
                                  padding))

                 ;;---
                 ;; Valid `default'.
                 ;;---
                 (t
                  nil)))

          ;;---
          ;; Final Fallback: A Space.
          ;;---
          ((and (null padding)
                (null default))
           (setq padding " "))

          ;;---
          ;; Valid `padding'.
          ;;---
          (t
           nil))

    ;;------------------------------
    ;; Normalize & Return
    ;;------------------------------
    ;; `padding' is set to something valid: char or string.
    ;; Let's make that a string or a string.
    (if (characterp padding)
        (make-string 1 padding)
      padding)))
;; (int<mis>:style:padding 'test (mis:style :padding "?"))
;; (int<mis>:style:padding 'test nil)
;; (int<mis>:style:padding 'test nil "!")
;; (int<mis>:style:padding 'test nil ?!)
;; (int<mis>:style:padding 'test (mis:style :padding "?") "!")
;; (int<mis>:style:padding 'test (mis:style :width 10) "!")


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
;; (mis:style :width 80)
;; (mis:style :align 'center "hello")
;; (mis:style :align 'center "hello %s" "world")
;; (mis:style :align 'center :width 11 "hello %s" "world")
;; (mis:style :align 'center :width 11 "hello")
;; (mis:style :indent 'auto "hello")
;; (mis:style :padding "?")


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'mis-style)
;;; mis-style.el ends here

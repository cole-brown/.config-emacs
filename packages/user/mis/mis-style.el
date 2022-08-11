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
(require 'mis-align)


;;------------------------------------------------------------------------------
;; Styling
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
  ;;------------------------------
  ;; Error Checking
  ;;------------------------------
  ;; Don't want any circular lists.
  (unless (proper-list-p args)
    (int<mis>:error 'mis:style
                    "ARGS must be a proper list (no circular references)! Got: %S"
                    args))

  (let ((parsing-styles t)
        styling   ; Validated/normalized styles output list.
        (args/len (length args))
        key
        value)

    ;;------------------------------
    ;; Build normalized styles list?
    ;;------------------------------
    (while (and args
                (> args/len 2)
                parsing-styles)
      (if (not (keywordp (car args)))
          ;;---
          ;; Done; no more style kvps to parse; rest is messaging stuff.
          ;;---
          (setq parsing-styles nil)

        ;;---
        ;; Validate a key/value pair.
        ;;---
        (setq key   (pop args)
              value (int<mis>:valid:style/kvp? 'mis:style
                                               key
                                               (pop args))
              args/len (- args/len 2))

        ;; Save to the normalized output.
        (push key   styling)
        (push value styling)))

    ;;------------------------------
    ;; Return.
    ;;------------------------------
    (list :style   (nreverse styling)
          :message args)))
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

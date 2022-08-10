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

ARGS should start off with styling keywords, key/values, etc before supplying
the format string and format args. Example:
  Valid styles:
    (mis:style :bold \"hello world\")
    (mis:style :bold :align 'center \"hello %s\" (get-greeted))
  Invalid styles:
    (mis:style \"hello %s\" :bold :align 'center (get-greeted))
    (mis:style \"hello %s\" (get-greeted) :bold :align 'center)

The \"invalid styles\" will just be interpreted as having no styling and extra
message args."
  ;;------------------------------
  ;; Error Checking
  ;;------------------------------
  ;; Don't want any circular lists.
  (unless (proper-list-p args)
    (int<mis>:error 'mis:style
                    "ARGS must be a proper list (no circular references)! Got: %S"
                    args))


  (let ((parsing-styles t)
        styling ; Normalized styles output list.
        messaging           ; Message format string and/or args.
        arg)                 ; Current item from ARGS being parsed.

    ;;------------------------------
    ;; Build normalized styles list?
    ;;------------------------------
    (while (and args
                parsing-styles)
      (setq arg (pop args))

      ;; Is this a style keyword?
      (if-let* ((key           (and (keywordp arg) arg)) ; ARG, if it is a valid Mis key.
                (value         :if-let-placeholder)      ; Value of `key', from ARGS.
                (valid?        (alist-get key int<mis>:valid:style))
                (valid/fn      :if-let-placeholder)
                (valid/members :if-let-placeholder)) ; Will just be nil for many things.
          ;; Style keyword. Validate it.
          (progn
            (setq valid/fn       (nth 0 valid?)
                  valid/members  (nth 1 valid?)) ; nil ok
            (if (null valid/fn)
                (progn
                  ;; No value; just a keyword flag. It's been validated by virtue
                  ;; of `if-let' finding it, so just add to styles.
                  (push key styling)
                  (push t styling)) ;; Normaliation of solo/flag keyword: set to true.

              ;; Grab key's value from ARGS, now that we know key is expected to
              ;; have a value.
              (setq value (pop args))

              ;; Validate key/value pair. Validator should signal error on
              ;; invalid so we don't need to check anything.
              (funcall valid/fn
                       'mis:style
                       key
                       value
                       valid/members)

              ;; Save to the normalized output.
              (push key styling)
              (push value styling)))

        ;; Not a style keyword. Also not a style keyword's value, since we deal
        ;; with those above too. So... Must mean we're done with style params
        ;; and are at message/message-arg params.
        (setq parsing-styles nil)))

    ;;------------------------------
    ;; Build message args.
    ;;------------------------------
    ;; Combine that last arg we were processing back together with the other
    ;; messaging args.
    (setq messaging (cons arg args))

    ;;------------------------------
    ;; Return.
    ;;------------------------------
    (list :style   (nreverse styling)
          :message messaging)))
;; (mis:style :align 'center "hello %s" "world")


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'mis-style)
;;; mis-style.el ends here

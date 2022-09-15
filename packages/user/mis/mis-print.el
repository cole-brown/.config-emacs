;;; mis-print.el --- Output & Printing -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <code@brown.dev>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2022-09-15
;; Modified:   2022-09-15
;;
;;; Commentary:
;;
;;  Output & Printing
;;
;;; Code:


(require 'mis-error)
(require 'mis-valid)
(require 'mis-parse)
(require 'mis-buffer)
(require 'mis-string)
(require 'mis-compile)
(require 'mis-style)


;;------------------------------------------------------------------------------
;; Output Trees
;;------------------------------
;; These trees are formatted as an alist with key/value cons:
;;   - key: `:output'
;;   - value:
;;     - list of alists with key/value conses
;;     - valid keys: `:string', `:metadata'
;;
;; Example
;; '((:output ((:string . "foo") (:metadata . ...))
;;            ((:string . "bar") (:metadata . ...))))
;;
;;------------------------------------------------------------------------------

(defun int<mis>:output:create (caller string &rest metadata)
  "Create a Mis Output Tree from STRING and METADATA(s).

String should be nil or a string.

Each METADATA should nil or a cons of a keyword and... some value.

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)"
  (let* ((caller (list 'int<mis>:print:string caller)))
    ;;------------------------------
    ;; Error Checks
    ;;------------------------------
    (cond ((and (not (null string))
                (not (stringp string)))
           (int<mis>:error caller
                           "Mis Output String must be a string or nil. Got %S: %S"
                           (type-of string)
                           string))

          ((and (not (null metadata))
                (not (seq-every-p (lambda (m) "Validate METADATA param."
                                    (and
                                     ;; Is a cons but is not a list?
                                     (and (listp m)
                                          (cdr m)
                                          (atom (cdr m)))
                                     ;; Key is a keyword?
                                     (keywordp (car m))))
                                  metadata)))
           (int<mis>:error caller
                           "Mis Output Metadata must be nil or a cons alist. Got %S: %S"
                           (type-of metadata)
                           metadata))

          ;;------------------------------
          ;; Create the Mis Output Tree
          ;;------------------------------
          (t
           ;; Alist of `:output' to alist...
           (list (cons :output
                       ;; Alist with `:string' & `:metadata' keys, validated values.
                       (list (cons :string string)
                             (cons :metadata metadata))))))))
;; (int<mis>:output:create 'test "foo" '(:buffer . "bar") '(:align . baz))


;;------------------------------------------------------------------------------
;; Output API
;;------------------------------------------------------------------------------

(defun mis (&rest args)
  "Output a message built from ARGS."
  (let (styling
        content
        output
        buffer)

    ;;------------------------------
    ;; Parsing & Validation
    ;;------------------------------
    ;; Check for validity, duplicate styles. Also look for any options we need
    ;; to skim off (e.g. `:buffer').
    (while args
      (let ((arg (pop args)))
        ;; Is this non-nil and /only/ styling info? Then it's global styling for
        ;; use in all of the things here.
        (cond ((and arg
                    (int<mis>:style:exclusive? arg))
               ;; Make sure it's valid...ish.
               (int<mis>:valid:syntax? 'mis 'arg arg)

               (if styling
                   ;; There can be only one.
                   ;;
                   ;; NOTE: We could allow more than one and merge, but then
                   ;; we'd have to do collision detection...
                   (int<mis>:error 'mis
                                   '("Only one Mis `:style' allowed per level in ARGS. "
                                     "have: %S, found: %S, args: %S")
                                   styling
                                   value
                                   args)
                 ;; Save the styling.
                 (setq styling arg)))

              ((listp arg)
               ;; Think this is a Mis syntax tree? Should be anyways; error if not.
               (int<mis>:valid:syntax? 'mis 'arg arg)
               ;; Save off this mis content type (messages, comments, arts...).
               (push arg content))

              ;; Top-level arg? Example: `:buffer'
              ((keywordp arg)
               (let ((key arg)
                     (value (pop arg)))
                 ;; Validate top-level Mis arg.
                 (pcase key
                   (:buffer
                    (if buffer
                        (int<mis>:error 'mis
                                        '("Only one Mis `:buffer' allowed. "
                                          "have: %S, found: %S, args: %S")
                                        buffer
                                        value
                                        args)
                      ;; Validate value later (when used)?
                      (setq buffer value)))
                   ;; TODO: Other top-level keywords?
                   (_
                    (int<mis>:error 'mis
                                    '("Unknown/unhandled args keyword/value pair!"
                                      "unknown keyword: %S, value: %S, args: %S")
                                    key
                                    value
                                    args)))))

              ;; Fallthrough: Error!
              (t
               (int<mis>:error 'mis
                               '("Unknown/unhandled argument!"
                                 "unknown: %S, args: %S")
                               arg
                               args)))))

    ;;------------------------------
    ;; Compile Mis syntax into strings.
    ;;------------------------------
    ;; Compile each piece of `content' in current (reverse) order & push to
    ;; `output' list. Final step will then have `output' in forwards order.
    (while content
      (int<mis>:debug 'mis
                      "compile: %S"
                      (nth 0 content))
      ;; Get a syntax tree; turn into a string.
      (push (int<mis>:compile 'mis
                              (pop content)
                              styling)
            output))

    (int<mis>:debug 'mis
                    "compiled output: %S"
                    output)

    ;;------------------------------
    ;; Print Output
    ;;------------------------------
    ;; Now that we only have output left, switch to output buffer and print.
    (mis:print:strings 'mis
                       buffer
                       output)))
;; (mis
;;  (mis:style :width 80)
;;  (mis:line "-"))
;; (mis (mis:comment (mis:line "-")))
;; (mis
;;   (mis:style :width 80)
;;   (mis:comment (mis:line "-"))
;;   (mis:comment "Hello there.")
;;   (mis:comment (mis:line "-")))
;; TODO-mis: YOU ARE HERE:
;; (mis (mis:comment (mis:string :align 'center "Hello there.")))
;; (mis
;;   (mis:style :width 80)
;;   (mis:comment (mis:line "-")
;;                (mis:string :align 'center "Hello there.")
;;                (mis:line "-")))
;; (mis
;;   (mis:comment :width 80
;;                (mis:line "-")
;;                (mis:string :align 'center "Hello there.")
;;                (mis:line "-")))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'mis-print)
;;; mis-print.el ends here
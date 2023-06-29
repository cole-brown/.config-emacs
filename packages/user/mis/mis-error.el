;;; mis-error.el --- Internal error helpers. -*- lexical-binding: t; -*-
;;
;; Author: Cole Brown <code@brown.dev>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:  2019-10-23
;; Timestamp:2022-08-08
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;  Internal error helpers.
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Settings
;;------------------------------------------------------------------------------

(defvar int<mis>:error:caller/format :first
  "Formatting options for `int<mis>:error:caller/string'.

Formats:
  - :full  - print all callers
  - :first - only print first caller (the erroring/debugging func)")


(defvar int<mis>:debugging? nil
  "Enable/disable `int<mis>:debug' output.

Set to non-nil to enable.
Set to nil to disable.")


(defun mis:cmd:debug/toggle ()
  "Enable/disable mis debug log statements."
  (interactive)
  (prog1
      (setq int<mis>:debugging? (not int<mis>:debugging?))
    (message "mis debugging %s"
             (if int<mis>:debugging?
                 "enabled!"
               "disabled."))))


;;------------------------------------------------------------------------------
;; Error Formatting
;;------------------------------------------------------------------------------

(defun int<mis>:error:name (name)
  "Return NAME as an uppercased string.

NAME should be either:
  - a keyword
  - a string
  - a quoted symbol"
  (cond ((keywordp name)
         ;; Keywords will just be themselves, but strings. Add quotes?
         (format "`%s'" name))

        ((or (stringp name)
             (symbolp name))
         ;; Strings & symbols should be upcased, as we're expecting those to be
         ;; function parameter names. `format' handles these well given "%s"
         ;; formatting spec.
         (upcase (format "%s" name)))

        (t
         ;; `format' so that we can return /something/. Throw in some extras so
         ;; we can maybe track down what's wrong to here.
         (format "¿(%s)?" name))))


(defun int<mis>:error:caller/normalize (&rest callers)
  "Return CALLERS as a list of callers.

CALLERS should each be calling function's name. They can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)"
  (let (normalized)
    ;;------------------------------
    ;; Process CALLERS into list of strings.
    ;;------------------------------
    (dolist (caller callers normalized)
      (cond ((or (null caller)
                 (eq caller 'quote)
                 (eq caller 'function))
             ;; Just ignore it.
             nil)

            ((proper-list-p caller)
             (setq normalized (append (apply #'int<mis>:error:caller/normalize caller)
                                      normalized)))

            ;; `consp' needs some help to catch "conses-but-not-lists".
            ;; "Some help" being "don't use it at all", apparently.
            ((and (listp caller)
                  (cdr caller)
                  (atom (cdr caller)))
             (setq normalized (append (int<mis>:error:caller/normalize (cdr caller))
                                      (int<mis>:error:caller/normalize (car caller))
                                      normalized)))

            ;; Some sort of improper list that isn't a cons?
            ((listp caller)
             (push (format "%s" caller) normalized))

            ;; Valids (strings, symbols)? Convert to string.
            ((stringp caller)
             (push caller normalized))
            ((symbolp caller)
             (push (format "%s" caller) normalized))

            ;; Invalids?
            (t
             ;; ...we can't really error... Format differently?
             (push (format "UNK:%S" caller) normalized))))))
;; (int<mis>:error:caller/normalize "foo" 'bar '(baz qux) '(quux . quuux))
;; (int<mis>:error:caller/normalize '((int<mis>:parse . tester)))


(defun int<mis>:error:caller/string (caller)
  "Return CALLER as a string.

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)

`int<mis>:error:caller/format' affects the returned string's formatting.
  - `:full' - all callers in CALLER are printed.
  - `:first' - only first in CALLER is printed."
  (let ((normalized (nreverse (int<mis>:error:caller/normalize caller))))
    ;;------------------------------
    ;; Determine formatting.
    ;;------------------------------
    (pcase int<mis>:error:caller/format
      ;; We don't care about tracing; just want to know the current caller.
      (:first
       (car normalized))
      ;; Combine list of strings into one.
      (:full
       (mapconcat #'identity normalized " <- "))
      ;; Dunno but this is used for erroring, so just sneak the dunno into the
      ;; return value.
      (_
       (concat "(caller-fmt-unknown) "
               (mapconcat #'identity normalized " <- "))))))
;; (int<mis>:error:caller/string '("foo" 'bar '(baz qux) '(quux . quuux)))
;; (int<mis>:error:caller/string '(baz bar foo))
;; (int<mis>:error:caller/string '(bar . foo))


;; TODO: This is the Nub message way. Convert to the mis way of doing concat/newlines?
(defun int<mis>:error:message (message)
  "Format MESSAGE into a string.

MESSAGE should be one of:
  - a string that `format' understands
  - a list of strings
    - will be concatenated without a separator (so use spaces where needed)
  - a list of strings that starts with the `:newlines' keyword
    - will be concatenated with \"\n\"

Return cons 2-tuple of:
  - nil or an error string about MESSAGE arg
  - formatted MESSAGE string"
  (let (string/error
        string/message)
    ;; Can't really error, since we're in the middle of erroring, so try to stuff
    ;; our error into the returned string. But try even harder not to call
    ;; anything that will error, as we want the caller to be able to error with
    ;; this string we're building.
    (cond ((stringp message)
           (setq string/error nil
                 string/message message))

          ((and (listp message)
                (seq-every-p #'stringp message))
           ;; Concat, no separator.
           (setq string/error nil
                 string/message (apply #'concat message)))

          ((and (listp message)
                (keywordp (car message)))
           (cond ((not (seq-every-p #'stringp (cdr message)))
                  (setq string/error (format "(mis: message should be strings: %S)"
                                             (cdr message))
                        ;; Can't use it as-is so just try to use it somehow..?
                        string/message (format "%S" message)))

                 ;; `:newlines' means append w/ newline separator.
                 ((eq (car message) :newlines)
                  (setq string/error nil
                        ;; Concat with newlines.
                        string/message (mapconcat #'identity
                                                  (cdr message)
                                                  "\n")))

                 ;; Fallthrough: Don't know what to do with this keyword...
                 (t
                  (setq string/error (format "(mis: message has invalid/unhandled keyword %S)"
                                             (car message))
                        ;; Can't use it as-is so just try to use it somehow..?
                        string/message (format "%S" message)))))

          ;; Fallthrough: Dunno what this is.
          (t
           (setq string/error (format "(mis: cannot handle message: %S)"
                                      message)
                 string/message (format "%S" message))))

    (format "%s%s"
            (if (stringp string/error)
                string/error
              "")
            string/message)))


;;------------------------------------------------------------------------------
;; Erroring
;;------------------------------------------------------------------------------

(defun int<mis>:error (caller message &rest args)
  "Signal an error MESSAGE, formatted with ARGS, from CALLER.

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)

MESSAGE should be one of:
  - a string that `format' understands
  - a list of strings
    - will be concatenated without a separator (so use spaces where needed)
  - a list of strings that starts with the `:newlines' keyword
    - will be concatenated with \"\n\"

ARGS should be the `format' ARGS for MESSAGE."
  ;; Raise error with args formatted into the finalized formatting string.
  (apply #'error
         ;; Format CALLER and MESSAGE inputs.
         (format "%s: %s"
                 (int<mis>:error:caller/string caller)
                 (int<mis>:error:message message))
         args))


;;------------------------------------------------------------------------------
;; Debugging
;;------------------------------------------------------------------------------

(defun int<mis>:debug (caller message &rest args)
  "If debugging, output a debug MESSAGE, formatted with ARGS, from CALLER.

Will only output if `int<mis>:debugging?' is non-nil.

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)

MESSAGE should be one of:
  - a string that `format' understands
  - a list of strings
    - will be concatenated without a separator (so use spaces where needed)
  - a list of strings that starts with the `:newlines' keyword
    - will be concatenated with \"\n\"

ARGS should be the `format' ARGS for MESSAGE."
  (when int<mis>:debugging?
    (apply #'message
           (format "[mis:DEBUG] %s: %s"
                   (int<mis>:error:caller/string caller)
                   (int<mis>:error:message message))
           args)))
;; (let ((int<mis>:debugging? t)) (int<mis>:debug 'test "hello %S" :there))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'mis-error)
;;; mis-error.el ends here

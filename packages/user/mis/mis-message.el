;;; mis-message.el --- Message output. -*- lexical-binding: t; -*-
;;
;; Author: Cole Brown <code@brown.dev>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:  2019-10-23
;; Modified: 2022-08-08
;;
;;; Commentary:
;;
;; Mis API for messages.
;;
;;; Code:


(require 'mis-error)
(require 'mis-valid)
(require 'mis-buffer)
(require 'mis-format)
(require 'mis-parse)

(require 'mis-align)
(require 'mis-style)
(require 'mis-art)

(require 'mis-comment)


;;------------------------------------------------------------------------------
;; Output Helpers
;;------------------------------------------------------------------------------

(defun mis:print:message (buffer message &rest args)
  "Output formatted/propertized MESSAGE & ARGS to BUFFER.

NOTE: Hacks/shenanigans for outputting propertized message/args to the
*Messages* buffer, which is normally plain strings only.

BUFFER should be nil, a string, or a few different keywords/symbols.
See `int<mis>:buffer:name'.

MESSAGE should be a string that `format' understands.

ARGS should be the `format' ARGS for MESSAGE."
  (let ((func/name (list 'mis caller))
        (output    (apply #'format message args)))
    (with-current-buffer (int<mis>:buffer:get-or-create func/name buffer)
      (save-mark-and-excursion
        ;; Special Shenanigans™ Part 01: *Messages*
        ;; Set `message-log-max' to nothing and then `message' so normal stuff
        ;; happens /EXCEPT/ for `output' getting printed to *Messages*.
        (when (eq (int<mis>:buffer:type func/name buffer) :messages)
          (let ((message-log-max nil))
            (message output)))

        (goto-char (point-max))

        ;; Special Shenanigans™ Part 02: Read-Only Buffers (e.g. *Messages*)
        ;; Ignore read-only status of buffer while we output to it. Need this to
        ;; be able to actually print our output to *Messages* with its properties
        ;; intact.
        (let ((inhibit-read-only t))
          ;; TODO: Test this to make sure we don't get extra newlines. It looks correct?
          (unless (zerop (current-column))
            (insert "\n"))
          (insert output "\n"))))))


(defun mis:print:strings (caller buffer strings)
  "Output formatted/propertized MESSAGES to current buffer.

BUFFER should be a buffer or string.

MESSAGES should be a list of strings.

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)"
  (let ((func/name (list 'mis caller)))
    (with-current-buffer (int<mis>:buffer:get-or-create func/name buffer)
      (save-mark-and-excursion
        (dolist (string strings)
          ;; Special Shenanigans™ Part 01: *Messages*
          ;; Set `message-log-max' to nothing and then `message' so normal stuff
          ;; happens /EXCEPT/ for STRING getting printed to *Messages*.
          (when (eq (int<mis>:buffer:type func/name buffer) :messages)
            (let ((message-log-max nil))
              (message string)))

          (goto-char (point-max))

          ;; Special Shenanigans™ Part 02: Read-Only Buffers (e.g. *Messages*)
          ;; Ignore read-only status of buffer while we output to it. Need this to
          ;; be able to actually print our output to *Messages* with its properties
          ;; intact.
          (let ((inhibit-read-only t))
            ;; TODO: Test this to make sure we don't get extra newlines. It looks correct?
            (unless (zerop (current-column))
              (insert "\n"))
            (insert string "\n")))))))


;;------------------------------------------------------------------------------
;; Compiling
;;------------------------------------------------------------------------------

(defun int<mis>:compile:mis (caller syntax &optional style)
  "Compile any `:mis' SYNTAX using STYLE; replace the result in SYNTAX.

SYNTAX should be a Mis Syntax Tree. It can contain styling of its own, which
will override any styling in STYLE.

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)"
  (let ((caller (list 'int<mis>:compile:wrapped caller))
        syntax/out)
    ;; Only look at top level.
    (dolist (assoc/child syntax)
      (let ((key   (car assoc/child))
            (value (cdr assoc/child)))
        (if (eq key :mis)
            ;; Compile whatever this wrapped thing is!
            (setq syntax/out (int<mis>:syntax:append caller
                                                     syntax/out
                                                     (int<mis>:syntax:create caller
                                                                             :mis:string
                                                                             (int<mis>:compile caller value style))))
          ;; Just put it back.
          (setq syntax/out (int<mis>:syntax:append caller
                                                   syntax/out
                                                   (int<mis>:syntax:set caller key value))))))

    ;; Return in correct order.
    (nreverse syntax/out)))
;; (int<mis>:compile:mis 'test (mis:comment (mis:line "-")))


(defun int<mis>:compile (caller syntax &optional style)
  "Compile SYNTAX into a propertized string for output using STYLE.

SYNTAX should be a Mis Syntax Tree. It can contain styling of its own, which
will override any styling in STYLE.

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)"
  (let* ((caller (list 'int<mis>:compile caller))
         ;; Figure out complete styling given STYLE and any `:mis:style' in SYNTAX.
         (styling (int<mis>:syntax:set caller
                                       :mis:style
                                       (int<mis>:syntax:merge
                                        caller
                                        (int<mis>:syntax:get/value caller
                                                                   :mis:style
                                                                   style)
                                        (int<mis>:syntax:get/value caller
                                                                   :mis:style
                                                                   syntax))))
         ;; Delete styling so we don't get confused about needing it or not.
         (syntax (int<mis>:syntax:delete caller
                                         :mis:style
                                         syntax))
         output)

    ;;------------------------------
    ;; Compile!
    ;;------------------------------
    ;; Our job is just to find the compiler for the mis type and tell it to do
    ;; its job. In some (most?) cases, we want to just send the whole of SYNTAX
    ;; to a compiler. For example, `:mis:comment' only contains the comment data
    ;; and needs a sibling string to actually wrap the comment start/end strings
    ;; around.
    ;;------------------------------

    ;; First check for any wrapped children.
    (when (int<mis>:syntax:find caller
                                syntax
                                :mis)
      (setq syntax (int<mis>:compile:mis caller
                                         syntax
                                         styling)))

    ;; Next check for the main compiler...
    (cond ((int<mis>:syntax:find caller
                                 syntax
                                 :mis:comment)
           (push (int<mis>:compile:comment caller
                                           syntax
                                           styling)
                 output))

          ((int<mis>:syntax:find caller
                                 syntax
                                 :mis:comment)
           (push (int<mis>:compile:comment caller
                                           syntax
                                           styling)
                 output))

          ((int<mis>:syntax:find caller
                                 syntax
                                 :mis:format)
            (push (int<mis>:compile:format caller
                                           syntax
                                           styling)
                  output))

          (t
           (int<mis>:error caller
                           "Unhandled Mis syntax: %S"
                           syntax)))

    ;; Just concat for final return string?
    (apply #'concat output)))
;; (int<mis>:compile 'test
;;                   '((:mis:format (:formatter . repeat) (:string . "-")))
;;                   (mis:style :width 80))
;; (int<mis>:compile 'test (mis:comment "hi") (mis:style :width 80))
;; (int<mis>:compile 'test (mis:comment (mis:line "-")))


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


;; TODO: Move this to "mis.el"?
(defun mis (&rest args)
  "Output a message built from ARGS."
  (let (styling
        content
        output
        buffer)

    ;; (message "\nargs!!!")
    ;; (message (pp-to-string args))
    ;; (message "\n\n")

    ;;------------------------------
    ;; Parsing & Validation
    ;;------------------------------
    ;; Check for validity, duplicate styles. Also look for any options we need
    ;; to skim off (e.g. `:buffer').
    (while args
      (let ((arg (pop args)))
        ;; Is this /only/ styling info? Save it for use on all its siblings.
        (cond ((int<mis>:style:exclusive? arg)
               ;; Make sure it's valid...ish.
               (int<mis>:valid:syntax? 'mis 'arg arg)

               (if styling
                   ;; There can be only one.
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
      ;; Get a syntax tree; turn into a string.
      (push (int<mis>:compile 'mis
                              (pop content)
                              styling)
            output))

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
;;   (mis:comment :align 'center "Hello there.")
;;   (mis:comment (mis:line "-")))
;; TODO-mis: YOU ARE HERE:
(mis
  (mis:style :width 80)
  (mis:comment (mis:line "-")
               (mis:message :align 'center "Hello there.")
               (mis:line "-")))
;; (mis
;;   (mis:comment :width 80
;;                (mis:line "-")
;;                (mis:style :align 'center "Hello there.")
;;                (mis:line "-")))



;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'mis-message)
;;; mis-message.el ends here

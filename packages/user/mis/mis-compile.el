;;; mis-compile.el --- Compile Mis Syntax Trees into message strings. -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <code@brown.dev>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2019-10-23
;; Modified:   2022-08-26
;;
;;; Commentary:
;;
;;  Compile Mis Syntax Trees into message strings.
;;
;;; Code:


(require 'mis-error)
(require 'mis-valid)
(require 'mis-buffer)
(require 'mis-format)


;;------------------------------------------------------------------------------
;; TODO: Generators?
;;------------------------------------------------------------------------------

;; TODO: Change to generators?
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Generators.html
;;
;; (require 'generator)
;;
;; (iter-defun int<mis>:compile/iter (caller syntax &optional style)
;;   "Compile any `:mis' SYNTAX using STYLE; replace the result in SYNTAX.
;;
;; SYNTAX should be a Mis Syntax Tree. It can contain styling of its own, which
;; will override any styling in STYLE.
;;
;; CALLER should be calling function's name. It can be one of:
;;   - a string
;;   - a quoted symbol
;;   - a function-quoted symbol
;;   - a list of the above, most recent first
;;     - e.g. '(#'error-caller \"parent\" 'grandparent)"
;;   (iter-yield "TODO 0")
;;   (iter-yield "TODO 1")
;;   (iter-yield "TODO 2")
;;   "TODO NORMAL RETURNED?")
;;
;; (iter-do (syntax (int<mis>:compile/iter 'test (mis:comment (mis:line "-"))))
;;   (message "iter syntax: %S" syntax))


;;------------------------------------------------------------------------------
;; Compilers
;;------------------------------------------------------------------------------

(defvar int<mis>:compilers
  nil
  "Alist of Mis keyword to compiler function.

Keyword must be a member of `int<mis>:keywords:category/internal'.

Function must have params: (CALLER SYNTAX STYLE)

SYNTAX should be `:mis:comment' syntax tree.

STYLE should be nil or a `:mis:style' syntax tree.
Example: (mis:style :width 80) -> '((:mis:style (:width . 80)))

Only STYLE will be checked for styling; style in SYNTAX is ignored.

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)")


(defun int<mis>:register:compiler (category function)
  "Register FUNCTION as the compiler for CATEGORY.

CATEGORY must be a member of `int<mis>:keywords:category/internal'.

Function must have params: (CALLER SYNTAX STYLE)

SYNTAX should be `:mis:comment' syntax tree.

STYLE should be nil or a `:mis:style' syntax tree.
Example: (mis:style :width 80) -> '((:mis:style (:width . 80)))

Only STYLE will be checked for styling; style in SYNTAX is ignored.

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)"
  (if-let ((existing (alist-get category int<mis>:compilers)))
      ;; Just ignore re-registrations...
      (unless (eq existing function)
        (int<mis>:error 'int<mis>:compile:register
                        "Compiler already exists for `%S'! Have: %S, attempted: %S"
                        category
                        existing
                        function))
    (push (cons category function) int<mis>:compilers)))


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


(int<mis>:register:compiler :mis #'int<mis>:compile:mis)


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
;; (mis (mis:comment (mis:string :align 'center "Hello there.")))
;; (mis
;;   (mis:style :width 80)
;;   (mis:comment (mis:line "-")
;;                (mis:string :align 'center "Hello there.")
;;                (mis:line "-")))
;; (mis
;;   (mis:comment :width 80
;;                (mis:line "-")
;;                (mis:style :align 'center "Hello there.")
;;                (mis:line "-")))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'mis-compile)
;;; mis-compile.el ends here

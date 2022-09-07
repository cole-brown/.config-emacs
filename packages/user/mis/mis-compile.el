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
;;   "Compile SYNTAX using STYLE; replace the result in SYNTAX.
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

SYNTAX should be a Mis Syntax Tree.

STYLE should be nil or a Mis `:style' Syntax Tree.
Example: (mis:style :width 80) -> '((:style (:width . 80)))

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)")


(defun int<mis>:compiler:register (category function)
  "Register FUNCTION as the compiler for CATEGORY.

CATEGORY must be a member of `int<mis>:keywords:category/internal'.

Function must have params: (CALLER SYNTAX STYLE)

SYNTAX should be a Mis Syntax Tree.

STYLE should be nil or a Mis `:style' Syntax Tree.
Example: (mis:style :width 80) -> '((:style (:width . 80)))

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)"
  ;; Just overwrite re-registrations.
  (setf (alist-get category int<mis>:compilers) function))


(defun int<mis>:compiler:get (category)
  "Get registered compiler function for CATEGORY.

CATEGORY must be a member of `int<mis>:keywords:category/internal'.

Function must have params: (CALLER SYNTAX STYLE)

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)"
  (or (alist-get category int<mis>:compilers)
      (int<mis>:error 'int<mis>:compile:get
                      "No compiler found for `%S'!"
                      category)))
;; (int<mis>:compiler:get :comment)


;;------------------------------------------------------------------------------
;; Compiling
;;------------------------------------------------------------------------------

(defun int<mis>:compile:syntax (caller syntax &optional style)
  "Compile SYNTAX into a propertized string for output using STYLE.

SYNTAX should be a Mis Syntax Tree. It can contain styling of its own, which
will override any styling in STYLE.

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)"
  (let* ((caller (list 'int<mis>:compile:syntax caller))
         ;; Figure out complete styling given STYLE and any `:style' in SYNTAX.
         (styling (int<mis>:syntax:set caller
                                       :style
                                       (int<mis>:syntax:merge caller
                                                              (int<mis>:syntax:get/value caller :style style)
                                                              (int<mis>:syntax:get/value caller :style syntax))))
         ;; Delete styling so we don't get confused about needing it or not.
         (syntax (int<mis>:syntax:delete caller
                                         :style
                                         syntax))
         output)

    ;;------------------------------
    ;; Compile!
    ;;------------------------------
    ;; Our job is just to find the compiler for the mis type and tell it to do
    ;; its job.
    ;;------------------------------
    (dolist (branch syntax)
      (let* ((category      (car branch))
             (syntax/branch (int<mis>:syntax:set caller
                                                 category
                                                 (cdr branch)))
             ;; Error checking for category's compiler func done in `int<mis>:compiler:get'.
             (compiler      (int<mis>:compiler:get category)))
        (push (funcall compiler
                       caller
                       syntax/branch
                       styling)
              output)))

    ;;------------------------------
    ;; Finalize
    ;;------------------------------
    (when (not (seq-every-p #'stringp output))
      (int<mis>:error caller
                      "Syntax should have been compiled to strings, got: %S"
                      output))
    (apply #'concat (nreverse output))))
;; (int<mis>:compile:syntax 'test (mis:string "-"))
;; (int<mis>:compile:syntax 'test (mis:line "-"))


(defun int<mis>:compile:children (caller syntax &optional style)
  "Compile SYNTAX into a propertized string for output using STYLE.

SYNTAX should be a Mis Syntax Tree. It can contain styling of its own, which
will override any styling in STYLE.

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)"
  (let* ((caller (list 'int<mis>:compile:children caller))
         (syntax/children (int<mis>:syntax:get/value caller
                                                     :children
                                                     syntax)))
    ;;------------------------------
    ;; Error Checks
    ;;------------------------------
    (unless syntax/children
      (int<mis>:error caller
                      "No children to compile: %S"
                      syntax))

    ;;------------------------------
    ;; Compile
    ;;------------------------------
    ;; Figure out complete styling given STYLE and any `:style' in SYNTAX.
    (let* ((styling (int<mis>:syntax:set caller :style
                                         (int<mis>:syntax:merge caller
                                                                (int<mis>:syntax:get/value caller :style style)
                                                                (int<mis>:syntax:get/value caller :style syntax/children))))
         ;; Delete styling so we don't get confused about needing it or not.
           (syntax/children (int<mis>:syntax:delete caller
                                                    :style
                                                    syntax/children))
           output)

      ;; And now we can just loop into the core compiling function.
      (dolist (child syntax/children)
        (let* ((category     (car child))
               (syntax/child (int<mis>:syntax:set caller
                                                  category
                                                  (cdr child))))
          (push (int<mis>:compile:syntax caller
                                         syntax/child
                                         styling)
                output)))

      ;;------------------------------
      ;; Finalize
      ;;------------------------------
      (when (not (seq-every-p #'stringp output))
        (int<mis>:error caller
                        "Children should have been compiled to strings, got: %S"
                        output))
      (apply #'concat (nreverse output)))))
;; (int<mis>:compile:children 'test '((:children (:format (:formatter . string) (:value . "-")))))


(int<mis>:register:compiler :children #'int<mis>:compile:children)


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
  ;; Just find the registered compiler and tell them to do it. Which is what
  ;; this function does, so just tell it to do it.
  (int<mis>:compile:syntax (list 'int<mis>:compile caller)
                           syntax
                           style))
;; (int<mis>:compile 'test '((:format (:formatter . repeat) (:value . "-"))) (mis:style :width 80))
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

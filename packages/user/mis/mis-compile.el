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
(require 'mis-tree-syntax)
(require 'mis-tree-output)


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
Example: (mis:style :width 80) -> '((:style (:width . 80)))")


(defun int<mis>:compiler:register (category function)
  "Register FUNCTION as the compiler for CATEGORY.

CATEGORY must be a member of `int<mis>:keywords:category/internal'.

Function must have params: (CALLER SYNTAX STYLE)

SYNTAX should be a Mis Syntax Tree.

STYLE should be nil or a Mis `:style' Syntax Tree.
Example: (mis:style :width 80) -> '((:style (:width . 80)))"
  ;; Just overwrite re-registrations.
  (setf (alist-get category int<mis>:compilers) function))


(defun int<mis>:compiler:get (category)
  "Get registered compiler function for CATEGORY.

CATEGORY must be a member of `int<mis>:keywords:category/internal'.

Function must have params: (CALLER SYNTAX STYLE)"
  (or (alist-get category int<mis>:compilers)
      (int<mis>:error 'int<mis>:compile:get
                      "No compiler found for `%S'!"
                      category)))
;; (int<mis>:compiler:get :comment)


;;------------------------------------------------------------------------------
;; Compiling
;;------------------------------------------------------------------------------

(defun int<mis>:compile:syntax (caller syntax &optional style/parents)
  "Compile SYNTAX into a propertized string for output using STYLE/PARENTS.

SYNTAX should be a Mis Syntax Tree. It can contain styling of its own, which
will override any styling in STYLE/PARENTS.

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)"
  (let* ((caller (list 'int<mis>:compile:syntax caller))
         output/trees
         output
         output/styled)
    (int<mis>:debug caller
                    "syntax:           %S"
                    syntax)

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
                       style/parents)
              output/trees)))
    (int<mis>:debug caller
                    "compiled outputs: %S"
                    output)

    ;;------------------------------
    ;; Validate
    ;;------------------------------
    ;; Should have a list of Mis Output Trees after compiling.
    ;; Combine into one Mis Output Tree while checking that.
    (dolist (tree (nreverse output/trees))
      (unless (int<mis>:valid:output? caller
                                      'tree
                                      tree
                                      :no-error)
        (int<mis>:error caller
                        "Invalid Mis Output Tree in compiled result: tree: %S, from: %S"
                        tree
                        output/trees))
      (setq output (int<mis>:output:append caller
                                           output
                                           tree)))
    ;;------------------------------
    ;; Finalize
    ;;------------------------------
    ;; Style all of the MOT strings.
    (int<mis>:debug caller
                    "output to style:  %S"
                    output)
    (setq output/styled (int<mis>:style caller
                                        output
                                        nil
                                        nil
                                        style/parents))
    (int<mis>:debug caller
                    "<--styled output: %S"
                    output/styled)
    output/styled))
;; (int<mis>:compile:syntax 'test (mis:string "-"))
;; (int<mis>:compile:syntax 'test (mis:line "-"))


(defun int<mis>:compile:children (caller parent syntax &optional style/ancestors)
  "Compile SYNTAX into a propertized string for output.

PARENT must be a keyword & member of `int<mis>:keywords:category/internal'. It
is the keyword we look under for `:children' in SYNTAX.

SYNTAX should be a Mis Syntax Tree. It can contain styling of its own, which
will override any styling in STYLE/ANCESTORS.

STYLE/ANCESTORS should be nil or a Mis Syntax Tree of only `:style'.

Return Mis Output Tree of compiled/styled children.

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)"
  (let* ((caller (list 'int<mis>:compile:children caller))
         ;; Update STYLE/ANCESTORS with our parents' styling for cascading into
         ;; the children.
         (style/this (int<mis>:syntax:merge/style caller
                                                  parent
                                                  syntax
                                                  style/ancestors))
         ;; Children are not allowed to inherit the `:newlines' style.
         (style/children (int<mis>:syntax:filter/style caller
                                                       style/this
                                                       :newlines))
         (syntax/children (int<mis>:syntax:find caller
                                                syntax
                                                parent
                                                :children)))
    (int<mis>:debug caller
                    "parent:          %S"
                    parent)
    (int<mis>:debug caller
                    "syntax:          %S"
                    syntax)
    (int<mis>:debug caller
                    "style/ancestors: %S"
                    style/ancestors)
    (int<mis>:debug caller
                    "syntax/children: %S"
                    syntax)
    (int<mis>:debug caller
                    "style/this:      %S"
                    style/this)
    (int<mis>:debug caller
                    "style/children: %S"
                    style/children)

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
    (let* (output/trees
           output
           output/styled)

      ;; And now we can just loop into the core compiling function.
      (dolist (child syntax/children)
        (let* ((category     (car child))
               (syntax/child (int<mis>:syntax:set caller
                                                  category
                                                  (cdr child))))
          (push (int<mis>:compile:syntax caller
                                         syntax/child    ;; Childrens' styling will come from their syntax.
                                         style/children) ;; Parents' styling cascades into children.
                output/trees)))

      ;;------------------------------
      ;; Validate
      ;;------------------------------
      ;; Should have a list of Mis Output Trees after compiling.
      ;; Combine into one Mis Output Tree while checking that.
      (dolist (tree (nreverse output/trees))
        (unless (int<mis>:valid:output? caller
                                        'tree
                                        tree
                                        :no-error)
          (int<mis>:error caller
                          "Invalid Mis Output Tree in compiled result: tree: %S, from: %S"
                          tree
                          output/trees))
        (setq output (int<mis>:output:append caller
                                             output
                                             tree)))

      ;;------------------------------
      ;; Finalize
      ;;------------------------------
      ;; Style all of the MOT strings.
      (int<mis>:debug caller
                      "output to style:  %S"
                      output)
      (setq output/styled (int<mis>:style caller
                                          output
                                          nil
                                          nil
                                          style/this))
      (int<mis>:debug caller
                      "<--styled output: %S"
                      output/styled)
      output/styled)))
;; (int<mis>:compile:children 'test :parent '((:parent (:children (:format (:formatter . string) (:value . "-"))))))


(int<mis>:compiler:register :children #'int<mis>:compile:children)


(defun int<mis>:compile (caller syntax &optional style/parent)
  "Compile SYNTAX into a propertized string for output using STYLE/PARENT.

SYNTAX should be a Mis Syntax Tree. It can contain styling of its own, which
will override any styling in STYLE/PARENT.

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)"
  (int<mis>:debug (list 'int<mis>:compile caller)
                  "syntax: %S"
                  syntax)
  ;; Just find the registered compiler and tell them to do it. Which is what
  ;; this function does, so just tell it to do it.
  (int<mis>:compile:syntax (list 'int<mis>:compile caller)
                           syntax
                           style/parent))
;; (int<mis>:compile 'test '((:format (:formatter . repeat) (:value . "-"))) (mis:style :width 80))
;; (int<mis>:compile 'test (mis:string "hi") (mis:style :width 80))
;; (int<mis>:compile 'test (mis:comment (mis:line "-")))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'mis-compile)
;;; mis-compile.el ends here

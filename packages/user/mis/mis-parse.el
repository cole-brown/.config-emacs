;;; mis-parse.el --- Parsing for Mis function calls -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <code@brown.dev>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2019-10-23
;; Modified:   2022-08-18
;;
;;; Commentary:
;;
;; Parsing for Mis function calls.
;;
;; Given a function call like, say:
;;   (mis:comment (mis:style '(:align 'center :width 80)
;;                           greeting
;;                           (mis:style :bold (get-greeted))
;;                           "."))
;;
;; We want Mis to evaluate args so that the caller's variables and functions are
;; resolved, but the top-level function (`mis', etc) needs the full, unaltered
;; tree structure to correctly compile the output.
;;
;; So Mis functions like `mis:comment' must:
;;   - Evaluate their arguments.
;;   - Return an Abstract Syntax Tree of themselves, basically.
;; E.g.
;;   (mis:comment :align 'center (get-greeted))
;;    -> '((:comment
;;          (:style (:align center))
;;          (:children (:format (:formatter . string) (:value . "world")))))
;;
;; NOTE: A Mis Syntax Tree is an alist:
;;   - the key must be from `int<mis>:keywords:category/internal'
;;   - the value must be a list
;;
;; This file is dedicated to the common functionality related to parsing user
;; inputs and building Mis Syntax Trees.
;;
;;
;;; Code:


(require 'cl-lib)

(require 'mis-valid)
(require 'mis-error)


;;------------------------------------------------------------------------------
;; Syntax Trees
;;------------------------------------------------------------------------------

(defun int<mis>:syntax:has (caller key syntax)
  "Return non-nil if KEY is a top-level alist key for Mis SYNTAX Tree.

KEY should a keyword.

SYNTAX should be a Mis Syntax Tree.

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)"
  (not (eq :mis:does-not-exist
           (alist-get key syntax :mis:does-not-exist))))
;; (int<mis>:syntax:has 'test :style '((:style (:width . 10) (:align . :center))))
;; No; only top-level.
;;   (int<mis>:syntax:has 'test :width '((:style (:width . 10) (:align . :center))))


(defun int<mis>:syntax:get/value (caller key syntax &optional default)
  "Get KEY value from Mis SYNTAX Tree.

KEY should be:
  - an internal keyword  - From `int<mis>:keywords:category/internal`
  - a temporary keyword - Starts with `:tmp:`

SYNTAX should be a Mis Syntax Tree.

Optional DEFAULT can be a value to return if KEY is not present.

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)"
  (alist-get key syntax default))
;; (int<mis>:syntax:get/value 'test :style '((:style (:width . 10) (:align . :center))))


(defun int<mis>:syntax:get/pair (caller key syntax)
  "Get KEY's assoc (key/value pair) from Mis SYNTAX Tree.

KEY should be:
  - an internal keyword  - From `int<mis>:keywords:category/internal`
  - a tempororay keyword - Starts with `:tmp:`

SYNTAX should be a Mis Syntax Tree.

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)"
  (assoc key syntax))
;; (int<mis>:syntax:get/pair 'test :style '((:style (:width . 10) (:align . :center))))


(defun int<mis>:syntax:get/syntax (caller key syntax)
  "Get KEY's assoc (key/value pair) _as a Mis Syntax Tree_ from Mis SYNTAX Tree.

KEY should be:
  - an internal keyword  - From `int<mis>:keywords:category/internal`
  - a tempororay keyword - Starts with `:tmp:`

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)"
  (when-let ((pair (int<mis>:syntax:get/pair (list 'int<mis>:syntax:get/syntax caller)
                                             key
                                             syntax)))
    (list (assoc key syntax))))
;; (int<mis>:syntax:get/syntax 'test :style '((:style (:width . 10) (:align . :center))))


(defun int<mis>:syntax:set (caller key syntax)
  "Create a new 1 element Mis Syntax Tree with key KEY and value SYNTAX.

KEY should be a keyword.

SYNTAX should be a Mis Syntax Tree.

Similar to `int<mis>:syntax:create' but for a single alist child instead of KVPs
that need to be turned into a syntax tree.
Equivalent Output:
  (int<mis>:syntax:create 'test :style '(:width . 10) '(:align . :center))
  (int<mis>:syntax:set    'test :style '((:width . 10) (:align . :center)))

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)"
  ;; Make an alist out of the inputs.
  (list (cons key syntax)))
;; (alist-get :width (alist-get :style (int<mis>:syntax:create 'test :style '(:width . 10) '(:align . :center))))
;; (alist-get :width (alist-get :style (int<mis>:syntax:set 'test :style '((:width . 10) (:align . :center)))))
;; (int<mis>:syntax:set 'test :style '((:width . 10) (:align . :center)))


(defun int<mis>:syntax:create (caller category/syntax &rest kvp)
  "Create a Mis Syntax Tree for CATEGORY/SYNTAX with KVPs.

CATEGORY/SYNTAX should be:
  - an internal keyword  - From `int<mis>:keywords:category/internal`
  - a tempororay keyword - Starts with `:tmp:`

KVP should (each) be a cons of a CATEGORY/SYNTAX keyword and its value.
example: '(:width . 10)

Similar to `int<mis>:syntax:set' but for one or more KVPs that need to be turned
into a syntax tree instead of just a single child that is already a proper
syntax tree alist.
Equivalent Output:
  (int<mis>:syntax:create 'test :style '(:width . 10) '(:align . :center))
  (int<mis>:syntax:set 'test :style '((:width . 10) (:align . :center)))

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)"
  ;; Make an alist out of the inputs.
  (list (cons category/syntax kvp)))
;; (alist-get :width (alist-get :style (int<mis>:syntax:create 'test :style '(:width . 10) '(:align . :center))))
;; (int<mis>:syntax:create 'test :style '((:width . 10) (:align . :center)))
;; (int<mis>:syntax:create 'test :string :value "hello")
;; (int<mis>:syntax:create 'test :comment)


(defun int<mis>:syntax:append (caller existing new)
  "Append NEW Mis Syntax Tree to EXISTING Mis Syntax Tree.

Return a newly created Mis Syntax Tree of results; caller should set output to
the appropriate variable if calling in a loop.

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)"
  (if (null existing)
      new
    (dolist (syntax/assoc new)
      (push syntax/assoc existing))
    existing))
;; (int<mis>:syntax:append 'test nil '((:style (:width . 10) (:align . :center))))
;; (int<mis>:syntax:append 'test '((:comment (:prefix . ";; ") (:postfix . "") (:type . default))) '((:style (:width . 10) (:align . :center))))


(defun int<mis>:syntax:update (caller key syntax &rest kvp)
  "Add/overwrite KVPs to Mis SYNTAX tree under KEY.

KVP should (each) be a cons of a KEY keyword and its value.
example: '(:width . 10)

SYNTAX should be a Mis abstract syntax tree. It will be updated with KVPs and
the updated value returned. Caller should set the return value back to the input
arg as the update is not guaranteed to be in-place.
Example: (setq syntax (int<mis>:syntax:update 'test :style syntax '(:align . :center)))

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)"
  (let* ((caller (list 'int<mis>:syntax:update caller))
         (syntax/cat (int<mis>:syntax:get/value caller key syntax)))
    (dolist (pair kvp)
      ;; Skip anything that's just a nil.
      (when pair
        (setf (alist-get (car pair) syntax/cat) (cdr pair))))
    (setf (alist-get key syntax) syntax/cat)
    syntax))
;; (int<mis>:syntax:update 'test :style '((:style (:width . 10) (:align . :center))))
;; (int<mis>:syntax:update 'test :style '((:style (:width . 10) (:align . :center))) '(:align . :right))
;; (int<mis>:syntax:update 'test :style '((:style (:width . 10) (:align . :center))) '(:align . :right) nil '(:trim . t))


(defun int<mis>:syntax:create-or-update (caller key syntax &rest kvp)
  "Create or update the Mis SYNTAX tree, depending on if it exists currently.

Will call either `int<mis>:syntax:create' or `int<mis>:syntax:update'.

KVP should (each) be a cons of a KEY keyword and its value.
example: '(:width . 10)

SYNTAX should be a Mis abstract syntax tree. It will be updated with KVPs and
the updated value returned. Caller should set the return value back to the input
arg as the update is not guaranteed to be in-place.
Example:
  (setq syntax (int<mis>:syntax:create-or-update 'test
                                                 :style
                                                 syntax
                                                 '(:align . :center)))

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)"
  (apply (if syntax
             #'int<mis>:syntax:update
           #'int<mis>:syntax:create)
         (list 'int<mis>:syntax:create-or-update caller)
         key
         syntax
         kvp))


(defun int<mis>:syntax:delete (caller key syntax)
  "Remove KEY from Mis SYNTAX tree.

KEY should be a keyword.

SYNTAX should be a Mis abstract syntax tree. Caller should set the return value
back to the input arg as the delete is not guaranteed to be in-place.
Example: (setq syntax (int<mis>:syntax:delete 'test :style syntax))

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)"
  (setf (alist-get key syntax nil :remove) nil)
  syntax)
;; (int<mis>:syntax:delete 'test :style '((:style (:width . 10) (:align . :center))))
;; (int<mis>:syntax:delete 'test :style '((:style (:width . 10) (:align . :center)) (:string . "hi")))


(defun int<mis>:syntax:find (caller syntax &rest key)
  "Find a value from Mis SYNTAX Tree by following KEYs.

SYNTAX should be a Mis abstract syntax tree.

KEYs should be keywords to follow down the SYNTAX tree to find the value.

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)"
  (let ((caller (list 'int<mis>:syntax:find caller))
        (value syntax))
    (dolist (find key)
      (setq value (int<mis>:syntax:get/value caller find value)))
    value))
;; (int<mis>:syntax:find 'test '((:style (:width . 10)) (:tmp:line (:string . "xX"))) :tmp:line :string)
;; (int<mis>:syntax:find 'test '((:style (:width . 10)) (:tmp:line (:string . "xX"))) :style)
;; (int<mis>:syntax:find 'test '((:style (:width . 10)) (:tmp:line (:string . "xX"))) :dne)


;; TODO: Delete? Change?
(defun int<mis>:syntax:merge (caller syntax/to syntax/from &rest ignore/from)
  "Merge two Mis Syntax Trees, ignoring IGNORE/FROM keys.

SYNTAX/TO and SYNTAX/FROM should be Mis abstract syntax trees. SYNTAX/FROM will
be merged into SYNTAX/TO, except any keywords from IGNORE/FROM.

NOTE: Assumes SYNTAX/TO and SYNTAX/FROM are valid. Caller should call
`int<mis>:valid:syntax?' before this if needed.

Return a Mis Syntax Tree combination of SYNTAX/TO and SYNTAX/FROM.

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)"
  (let ((caller (list 'int<mis>:syntax:merge caller)))
    (dolist (assoc/from syntax/from)
      (let ((key/from   (car assoc/from))
            (value/from (cdr assoc/from)))
        ;; Ignore?
        (unless (memq key/from ignore/from)
          ;; Merge this into SYNTAX/TO.
          (setf (alist-get key/from syntax/to) value/from))))
    ;; Return the updated syntax.
    syntax/to))
;; (int<mis>:syntax:merge 'test
;;                         '((:style (:width . 10) (:align . :center)) (:message "hello there"))
;;                         '((:style (:width . 11)) (:ignored (:string . "xxx")))
;;                         :ignored)


(defun int<mis>:syntax:children (caller key syntax &rest kvp)
  "Add each CHILD into SYNTAX under KEY's `:children'.

KEY should be a keyword.

SYNTAX should be a Mis abstract syntax trees.

KVP should (each) be a cons of a KEY keyword and its value.
example: '(:width . 10)

Return a Mis Syntax Tree. Caller should set this return value back to their
syntax variable.

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)"
  (let ((caller (list 'int<mis>:syntax:children caller))
        (children (int<mis>:syntax:get/value caller
                                             :children
                                             (int<mis>:syntax:get/value caller
                                                                        key
                                                                        syntax))))
    ;; Update the list of children with all the KVPs.
    (dolist (each kvp)
      (push each children))
    ;; Update the syntax tree with the children.
    (setf (alist-get :children
                     (alist-get key syntax))
          children)
    ;; Return updated syntax tree.
    syntax))
;; (int<mis>:syntax:children 'test
;;                           :comment
;;                           '((:comment))
;;                           '(:format (:formatter . string) (:value . "hello")))
;; (int<mis>:syntax:children 'test
;;                           :comment
;;                           '((:comment (:children (:test :hello 'there))))
;;                           '(:format (:formatter . string) (:value . "hello")))


;;------------------------------------------------------------------------------
;; Parsing
;;------------------------------------------------------------------------------

(defun int<mis>:parse (caller category valid &rest args)
  "Parse ARGS into a Mis Abstract Syntax Tree.

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)

CATEGORY must be a keyword from `int<mis>:keywords:category/input'.

Optional VALID parameter is for valid/expected category keywords. It should be:
  - nil                - All (existing) categories are valid.
  - a keyword          - Only this category's keywords are valid.
  - a list of keywords - Only these categories' keywords are valid.
                       - Must be from `int<mis>:keywords:category/input'."
  (let ((caller (list 'int<mis>:parse caller)))
    ;;------------------------------
    ;; Error Checking
    ;;------------------------------
    ;; Are VALID & CATEGORY valid?
    (int<mis>:valid:category/mis/input:param? caller valid)
    (int<mis>:valid:category/mis/input?       caller category)
    ;; Don't want to parse any circular lists.
    (unless (proper-list-p args)
      (int<mis>:error caller
                      "ARGS must be a proper list (no circular references)! Got: %S"
                      args))

    ;;------------------------------
    ;; Normalize Categories
    ;;------------------------------
    ;; Normalize to list of keywords.
    (when (keywordp valid)
      (setq valid (list valid)))

    ;;------------------------------
    ;; Parse Args
    ;;------------------------------
    (let ((parsing t)
          (args/len (length args))
          (category/valid-roots (alist-get category int<mis>:keywords:category))
          category/out
          syntax/parsed ; Validated/normalized kvps alist by keyword category.
          syntax/in
          formatting/in
          syntax/out/format
          syntax/out/children
          syntax/out)

      (while (and args
                  (> args/len 1)
                  parsing)
        (if (not (keywordp (car args)))
            ;;---
            ;; Done; no more kvps to parse; rest is either Mis Syntax Trees or messaging stuff.
            ;;---
            (setq parsing nil)

          ;;---
          ;; Validate a key/value pair.
          ;;---
          (setq args/len (- args/len 2))
          (let* ((key                (pop args))
                 (value              (pop args))
                 (validator          (int<mis>:valid:validator caller valid key))
                 (category/validator (car validator)) ; parsed category (internal or tmp)
                 (function/validator (cdr validator)))
            ;; Don't check value yet...
            (if (or (null key)
                    (null category/validator)
                    (null function/validator))
                ;; Can't parse it if we don't know about it.
                (int<mis>:error caller
                                '("Parsing Error: Invalid key, or validator... or something? "
                                  "key: %S, "
                                  "validator: %S, "
                                  "category/validator: %S, "
                                  "function/validator: %S, "
                                  "(unvalidated) value: %S")
                                key
                                validator
                                category/validator
                                function/validator
                                value)

              ;; Key/value exist; validate and then save to the syntax tree for
              ;; this parsed category.
              (let ((value      (funcall function/validator caller key value))
                    (syntax/cat (alist-get category/validator syntax/parsed)))
                (int<mis>:debug caller "parsing...: %S" key)
                (int<mis>:debug caller "  - key:                  %S" key)
                (int<mis>:debug caller "  - value:                %S" value)
                (int<mis>:debug caller "  - category: . . . . . . %S" category/validator)
                (int<mis>:debug caller "  - syntax/cat: . . . . . %S" syntax/cat)
                (int<mis>:debug caller "  - category/valid-roots: %S" category/valid-roots)
                ;; Is this the root category? We'll need to know it when building the final syntax tree.
                (when (memq category/validator category/valid-roots)
                  (if (and category/out
                           (not (eq category/out category/validator)))
                      ;; Can't print out full input ARGS in error message cuz we're popping them off.
                      ;; NOTE: Change this loop to `pop' from a copy of args if we need better error output here.
                      (int<mis>:error caller
                                      '("Found two output roots while parsing! "
                                        "Have: %S, Found: %S")
                                      category/out
                                      syntax/cat
                                      args)
                    (setq category/out category/validator))
                  (int<mis>:debug caller "  <---category/out:       %S" category/out))
                ;; Save the parsed key/value.
                (setf (alist-get key syntax/cat) value)
                (setf (alist-get category/validator syntax/parsed) syntax/cat))))))

      (int<mis>:debug caller "syntax/parsed initial: %S" syntax/parsed)
      (int<mis>:debug caller "category/out:          %S" category/out)

      ;;------------------------------
      ;; What did we parse?
      ;;------------------------------

      ;; Do we need to default to an output category?
      ;; In general, we shouldn't...
      (unless category/out
        (setq category/out (nth 0 category/valid-roots))
        (int<mis>:debug caller "category/out default:  %S" category/out))

      ;; Do we need to promote `syntax/parsed' to top level? E.g. if we parsed a
      ;; `:style' category then its `:style' keyword and styling key/value pairs
      ;; should be at the top.
      (int<mis>:debug caller "parsed has out cat?:   %S" (int<mis>:syntax:has caller category/out syntax/parsed))
      (when (int<mis>:syntax:has caller
                                 category/out
                                 syntax/parsed)
        (int<mis>:debug caller "move: syntax/parsed:   %S" syntax/parsed)
        ;; Move `syntax/parsed' to `syntax/out'.
        (setq syntax/out (int<mis>:syntax:merge caller
                                                syntax/out
                                                syntax/parsed)
              ;; Clear so rest of the function can check it correctly.
              syntax/parsed nil)
        (int<mis>:debug caller "to: syntax/out:        %S" syntax/out))
      (int<mis>:debug caller "syntax/parsed final:   %S" syntax/parsed)
      (int<mis>:debug caller "args left over:        %S" args)

      ;;------------------------------
      ;; Deal with any pre-parsed Mis Syntax Trees in ARGS.
      ;;------------------------------
      (when args
        ;; Split Mis Syntax Trees out into `syntax/in' var & non-syntax into
        ;; `formatting/in'.
        (dolist (arg args)
          (if (int<mis>:valid:syntax? caller 'arg arg :no-error)
              (setq syntax/in (int<mis>:syntax:append caller
                                                      syntax/in
                                                      arg))
            (push arg formatting/in))

          ;; Get the string/formatting in the correct order again.
          (setq formatting/in (nreverse formatting/in)))

        ;; Correct `syntax/in' to a proper Mis syntax tree for branches.
        (when syntax/in
          (setq syntax/out/children (apply #'int<mis>:syntax:create
                                           caller
                                           :children
                                           syntax/in))))
      (int<mis>:debug caller "formatting/in:         %S" formatting/in)
      (int<mis>:debug caller "syntax/out/children:   %S" syntax/out/children)

      ;;------------------------------
      ;; Build Syntax Tree: Presume that all the `formatting/in' are message/formatting.
      ;;------------------------------
      (when formatting/in
        (cond ((and (= (length formatting/in) 1)
                    (stringp (nth 0 formatting/in)))
               ;; Just a single string.
               (setq syntax/out/format (int<mis>:syntax:create caller
                                                               :format
                                                               (cons :formatter 'string)
                                                               ;; Just the one string.
                                                               (cons :value (nth 0 formatting/in)))))

              ((and (= (length formatting/in) 1)
                    (characterp (nth 0 formatting/in)))
               ;; Just a single character.
               (setq syntax/out/format (int<mis>:syntax:create caller
                                                               :format
                                                               (cons :formatter 'char)
                                                               ;; Just the one string.
                                                               (cons :value (nth 0 formatting/in)))))

              (t
               ;; Generic message format string and args.
               (setq syntax/out/format (int<mis>:syntax:create caller
                                                               :format
                                                               (cons :formatter 'message)
                                                               ;; Everything!
                                                               (cons :value formatting/in))))))
      (int<mis>:debug caller "syntax/out/format:     %S" syntax/out/format)

      ;;------------------------------
      ;; Build Syntax Tree: Place `syntax/out/format' appropriately.
      ;;------------------------------
      ;; Is the parsing CATEGORY `:string'? Then `syntax/out/format' is the root
      ;; output, otherwise it's a child syntax tree.
      (when syntax/out/format
        (if (eq category :string)
            (setq syntax/out (int<mis>:syntax:merge caller
                                                    syntax/out
                                                    syntax/out/format))
          (setq syntax/out/children (int<mis>:syntax:update caller
                                                            :children
                                                            syntax/out/children
                                                            (int<mis>:syntax:get/pair caller
                                                                                      :format
                                                                                      syntax/out/format)))))
      (int<mis>:debug caller "syntax/out:            %S" syntax/out)
      (int<mis>:debug caller "syntax/out/children:   %S" syntax/out/children)

      ;;------------------------------
      ;; Build Syntax Tree: Add new trees.
      ;;------------------------------
      ;; Keep separate from the parsing so we enforce a more human-friendly
      ;; ordering to the alists, though it won't matter to the builder/compiler.
      ;; Just friendly to the programmer to have these first in the output alist.
      (if syntax/parsed
          (setq syntax/out (apply #'int<mis>:syntax:create-or-update
                                  caller
                                  category/out
                                  syntax/out
                                  syntax/parsed))

        ;; Nothing special parsed for `category/out', but we need to make
        ;; `syntax/out' have a root of `category/out'...
        (unless syntax/out
          (setq syntax/out (int<mis>:syntax:create caller
                                                   category/out))))

      ;;------------------------------
      ;; Build Syntax Tree: Add pre-existing syntax trees.
      ;;------------------------------
      (when syntax/out/children
        (setq syntax (apply #'int<mis>:syntax:children
                            caller
                            category/out
                            syntax/out
                            (int<mis>:syntax:get/value caller
                                                       :children
                                                       syntax/out/children))))
      syntax/out)))
;; (int<mis>:parse 'test :string nil "hello %s" "world")
;; (int<mis>:parse 'test :string '(:style :string) "hello")
;; (int<mis>:parse 'test :comment '(:comment :style :string) "hello")
;; (int<mis>:parse 'test :string nil :align 'center "hello")
;; (int<mis>:parse 'test :string :style :align 'center "hello")
;; (int<mis>:parse 'test :string nil :align 'center "hello %s" "world")
;; (int<mis>:parse 'test :string nil :align 'center :width 11 "hello %s" "world")
;; (int<mis>:parse 'test :string nil :align 'center :width 11 "hello")
;; (int<mis>:parse 'test :string nil :indent 'auto "hello")
;; (int<mis>:parse 'test :line '(:line :style) "-")
;; (int<mis>:parse 'test :style :style :width 80)
;; (int<mis>:parse 'test :style :style :padding "?")
;;
;; Parse a syntax tree -> Get a syntax tree.
;; (int<mis>:parse 'test :comment '(:comment :style) '((:format (:formatter repeat :string "-"))))
;;
;; Error: `:align' is a `:style', not a `:comment' category keyword.
;;   (int<mis>:parse 'test :comment :comment :align 'center "hello")


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'mis-parse)
;;; mis-parse.el ends here

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
;;    -> '((:mis:comment
;;          (:mis:style (:align center))
;;          (:mis:string "world")))
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

(defun int<mis>:syntax:get/value (caller key syntax &optional default)
  "Get KEY value from Mis SYNTAX Tree.

KEY should be:
  - an internal keyword  - Starts with `:mis:`
                         - From `int<mis>:keywords:key`
  - a tempororay keyword - Starts with `:tmp:`

Optional DEFAULT can be a value to return if KEY is not present.

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)"
  (alist-get key syntax default))
;; (int<mis>:syntax:get/value 'test :mis:style '((:mis:style (:width . 10) (:align . :center))))


(defun int<mis>:syntax:get/pair (caller key syntax)
  "Get KEY's assoc (key/value pair) from Mis SYNTAX Tree.

KEY should be:
  - an internal keyword  - Starts with `:mis:`
                         - From `int<mis>:keywords:key`
  - a tempororay keyword - Starts with `:tmp:`

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)"
  (assoc key syntax))
;; (int<mis>:syntax:get/pair 'test :mis:style '((:mis:style (:width . 10) (:align . :center))))


(defun int<mis>:syntax:get/syntax (caller key syntax)
  "Get KEY's assoc (key/value pair) _as a Mis Syntax Tree_ from Mis SYNTAX Tree.

KEY should be:
  - an internal keyword  - Starts with `:mis:`
                         - From `int<mis>:keywords:key`
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
;; (int<mis>:syntax:get/syntax 'test :mis:style '((:mis:style (:width . 10) (:align . :center))))


(defun int<mis>:syntax:set (caller key syntax)
  "Create a new 1 element Mis Syntax Tree with key KEY and value SYNTAX.

KEY should be a keyword.

SYNTAX should be a Mis Syntax Tree.

Similar to `int<mis>:syntax:create' but for a single alist child instead of KVPs
that need to be turned into a syntax tree.
Equivalent Output:
  (int<mis>:syntax:create 'test :mis:style '(:width . 10) '(:align . :center))
  (int<mis>:syntax:set 'test :mis:style '((:width . 10) (:align . :center)))

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)"
  ;; Make an alist out of the inputs.
  (list (cons key syntax)))
;; (alist-get :width (alist-get :mis:style (int<mis>:syntax:create 'test :mis:style '(:width . 10) '(:align . :center))))
;; (alist-get :width (alist-get :mis:style (int<mis>:syntax:set 'test :mis:style '((:width . 10) (:align . :center)))))
;; (int<mis>:syntax:set 'test :mis:style '((:width . 10) (:align . :center)))


(defun int<mis>:syntax:create (caller category/syntax &rest kvp)
  "Create a Mis Syntax Tree for CATEGORY/SYNTAX with KVPs.

CATEGORY/SYNTAX should be:
  - an internal keyword  - Starts with `:mis:`
                         - From `int<mis>:keywords:category/syntax`
  - a tempororay keyword - Starts with `:tmp:`

KVP should (each) be a cons of a CATEGORY/SYNTAX keyword and its value.
example: '(:width . 10)

Similar to `int<mis>:syntax:set' but for one or more KVPs that need to be turned
into a syntax tree instead of just a single child that is already a proper
syntax tree alist.
Equivalent Output:
  (int<mis>:syntax:create 'test :mis:style '(:width . 10) '(:align . :center))
  (int<mis>:syntax:set 'test :mis:style '((:width . 10) (:align . :center)))

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)"
  ;; Make an alist out of the inputs.
  (list (cons category/syntax kvp)))
;; (alist-get :width (alist-get :mis:style (int<mis>:syntax:create 'test :mis:style '(:width . 10) '(:align . :center))))
;; (int<mis>:syntax:create 'test :mis:style '((:width . 10) (:align . :center)))
;; (int<mis>:syntax:create 'test :mis:string :value "hello")
;; (int<mis>:syntax:create 'test :mis:comment)


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
;; (int<mis>:syntax:append 'test nil '((:mis:style (:width . 10) (:align . :center))))
;; (int<mis>:syntax:append 'test '((:mis:comment (:prefix . ";; ") (:postfix . "") (:type . default)) (:mis (:mis:format (:formatter . repeat) (:string . "-")))) '((:mis:style (:width . 10) (:align . :center))))


(defun int<mis>:syntax:update (caller key syntax &rest kvp)
  "Add/overwrite KVPs to Mis SYNTAX tree under KEY.

KVP should (each) be a cons of a KEY keyword and its value.
example: '(:width . 10)

SYNTAX should be a Mis abstract syntax tree. It will be updated with KVPs and
the updated value returned. Caller should set the return value back to the input
arg as the update is not guaranteed to be in-place.
Example: (setq syntax (int<mis>:syntax:update 'test :mis:style syntax '(:align . :center)))

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
;; (int<mis>:syntax:update 'test :mis:style '((:mis:style (:width . 10) (:align . :center))))
;; (int<mis>:syntax:update 'test :mis:style '((:mis:style (:width . 10) (:align . :center))) '(:align . :right))
;; (int<mis>:syntax:update 'test :mis:style '((:mis:style (:width . 10) (:align . :center))) '(:align . :right) nil '(:trim . t))


(defun int<mis>:syntax:delete (caller key syntax)
  "Remove KEY from Mis SYNTAX tree.

KEY should be a keyword.

SYNTAX should be a Mis abstract syntax tree. Caller should set the return value
back to the input arg as the delete is not guaranteed to be in-place.
Example: (setq syntax (int<mis>:syntax:delete 'test :mis:style syntax))

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)"
  (setf (alist-get key syntax nil :remove) nil)
  syntax)
;; (int<mis>:syntax:delete 'test :mis:style '((:mis:style (:width . 10) (:align . :center))))
;; (int<mis>:syntax:delete 'test :mis:style '((:mis:style (:width . 10) (:align . :center)) (:mis:string . "hi")))


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
;; (int<mis>:syntax:find 'test '((:mis:style (:width . 10)) (:tmp:line (:string . "xX"))) :tmp:line :string)
;; (int<mis>:syntax:find 'test '((:mis:style (:width . 10)) (:tmp:line (:string . "xX"))) :mis:style)
;; (int<mis>:syntax:find 'test '((:mis:style (:width . 10)) (:tmp:line (:string . "xX"))) :dne)


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
;;                         '((:mis:style (:width . 10) (:align . :center)) (:mis:message "hello there"))
;;                         '((:mis:style (:width . 11) (:align . :right)) (:tmp:line (:string . "xxx")))
;;                         :tmp:line)


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
;;                           :mis:comment
;;                           '((:mis:comment))
;;                           '(:mis:format (:formatter . string) (:value . "hello")))
;; (int<mis>:syntax:children 'test
;;                           :mis:comment
;;                           '((:mis:comment (:children (:mis:test :hello 'there))))
;;                           '(:mis:format (:formatter . string) (:value . "hello")))


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
          (if-let* ((key           (pop args))
                    (validator     (int<mis>:valid:validator caller valid key))
                    (validator-cat (car validator)) ; parsed category (internal or tmp)
                    (validator-fn  (cdr validator))
                    (value         (funcall validator-fn caller key (pop args))))
              ;; Key/value exist and are now known to be valid; save to the syntax tree
              ;; for this parsed category.
              (let ((syntax/cat (alist-get validator-cat syntax/parsed)))
                ;; Is this the root category? We'll need to know it when building the final syntax tree.
                (when (memq syntax/cat category/valid-roots)
                  (if category/out
                      ;; Can't print out full input ARGS cuz we're popping. :|
                      ;; NOTE: Change this loop to pop from a copy of args if we need better error output here.
                      (int<mis>:error caller
                                      '("Found two output roots while parsing! "
                                        "Have: %S, Found: %S")
                                      category/out
                                      syntax/cat
                                      args)
                    (setq category/out syntax/cat)))
                ;; Save the parsed key/value.
                (setf (alist-get key syntax/cat) value)
                (setf (alist-get validator-cat syntax/parsed) syntax/cat))

            ;; `if-let' failure... There /should/ have been an error raised already?
            (int<mis>:error caller
                            '("Invalid key, value or something? "
                              "key: %S, "
                              "validator: %S, "
                              "validator-cat: %S, "
                              "validator-fn: %S, "
                              "value: %S")
                            key
                            validator
                            validator-cat
                            validator-fn
                            value))))

      ;; Do we need to default to an output category?
      ;; In general, we shouldn't...
      (unless category/out
        (setq category/out (nth 0 category/valid-roots)))

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

      ;;------------------------------
      ;; Build Syntax Tree: Presume that all the `formatting/in' are message/formatting.
      ;;------------------------------
      (when formatting/in
        (cond ((and (= (length formatting/in) 1)
                    (stringp (nth 0 formatting/in)))
               ;; Just a single string.
               (setq syntax/out/format (int<mis>:syntax:create caller
                                                               :mis:format
                                                               (cons :formatter 'string)
                                                               (cons :value (nth 0 formatting/in)))))

              ((and (= (length formatting/in) 1)
                    (characterp (nth 0 formatting/in)))
               ;; Just a single string; use `:mis:string' to simplify things later.
               (setq syntax/out/format (int<mis>:syntax:create caller
                                                               :mis:format
                                                               (cons :formatter 'char)
                                                               (cons :value (nth 0 formatting/in)))))

              (t
               ;; Generic message format string and/or args go into the `:mis:message'.
               (setq syntax/out/format (int<mis>:syntax:create caller
                                                               :mis:format
                                                               (cons :formatter 'message)
                                                               (cons :value (nth 0 formatting/in)))))))

      ;;------------------------------
      ;; Build Syntax Tree: Place `syntax/out/format' appropriately.
      ;;------------------------------
      ;; Is the parsing CATEGORY `:string'? Then `syntax/out/format' is the root
      ;; output, otherwise it's a child syntax tree.
      (when syntax/out/format
        (if (eq category :string)
            (setq syntax/out syntax/out/format)
          (setq syntax/out/children (int<mis>:syntax:update caller
                                                            :children
                                                            syntax/out/children
                                                            (int<mis>:syntax:get/pair caller
                                                                                      :mis:format
                                                                                      syntax/out/format)))))

      ;;------------------------------
      ;; Build Syntax Tree: Add new trees.
      ;;------------------------------
      ;; Keep separate from the parsing so we enforce a more human-friendly
      ;; ordering to the alists, though it won't matter to the builder/compiler.
      ;; Just friendly to the programmer to have these first in the output alist.
      (if syntax/parsed
          (if syntax/out
              (setq syntax/out (apply #'int<mis>:syntax:update
                                      caller
                                      category/out
                                      syntax/out
                                      syntax/parsed))

            (setq syntax/out (apply #'int<mis>:syntax:create
                                    caller
                                    category/out
                                    syntax/parsed)))
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
;; (int<mis>:parse 'test :string '(:style :string) "hello")
;; (int<mis>:parse 'test :comment '(:comment :style :string) "hello")
;; (int<mis>:parse 'test :string nil :align 'center "hello")
;; (int<mis>:parse 'test :string :style :align 'center "hello")
;; (int<mis>:parse 'test :string nil :align 'center "hello %s" "world")
;; (int<mis>:parse 'test :string nil :type 'block :align 'center "hello %s" "world")
;; (int<mis>:parse 'test :string nil :align 'center :width 11 "hello %s" "world")
;; (int<mis>:parse 'test :string nil :align 'center :width 11 "hello")
;; (int<mis>:parse 'test :string nil :indent 'auto "hello")
;;
;; Parse a syntax tree -> Get a `:mis' SYNTAX.
;; (int<mis>:parse 'test :comment '(:comment :style) '((:mis:format (:formatter repeat :string "-"))))
;;
;; Error: `:align' is a `:mis:style', not a `:mis:comment' category keyword.
;;   (int<mis>:parse 'test :comment :comment :align 'center "hello")


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'mis-parse)
;;; mis-parse.el ends here

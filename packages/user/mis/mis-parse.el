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
;; NOTE: A Mis AST is an alist:
;;   - the key must be from `int<mis>:keywords:category/internal'
;;   - the value must be a list
;;
;; This file is dedicated to the common functionality related to parsing user
;; inputs and building Mis ASTs.
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
;; (int<mis>:syntax:create 'test :mis:style '((:width . 10) (:align . :center)))


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


(defun int<mis>:syntax:update (caller key syntax &rest kvp)
  "Add/overwrite KVPs to Mis SYNTAX tree under KEY.

KVP should (each) be a cons of a KEY keyword and its value.
example: '(:width . 10)

SYNTAX should be a Mis abstract syntax tree. It will be updated with KVPs and
the updated value returned. Caller should set the return value back to the input
arg as the update is not guaranteed to be in-place.

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)"
  (let* ((caller (list 'int<mis>:syntax:update caller))
         (syntax/cat (int<mis>:syntax:get/value caller key syntax)))
  (dolist (pair kvp)
      (setf (alist-get (car pair) syntax/cat) (cdr pair)))
  (setf (alist-get key syntax) syntax/cat)
  syntax))
;; (int<mis>:syntax:update 'test :mis:style '((:mis:style (:width . 10) (:align . :center))))
;; (int<mis>:syntax:update 'test :mis:style '((:mis:style (:width . 10) (:align . :center))) '(:align . :right))
;; (int<mis>:syntax:update 'test :mis:style '((:mis:style (:width . 10) (:align . :center))) '(:align . :right) '(:trim . t))


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


;;------------------------------------------------------------------------------
;; Parsing
;;------------------------------------------------------------------------------

(defun int<mis>:parse (caller category &rest args)
  "Parse ARGS into a Mis Abstract Syntax Tree.

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)

Optional CATEGORY should be:
  - nil                - All (existing) categories are valid.
  - a keyword          - Only this category's keywords are valid.
  - a list of keywords - Only these categories' keywords are valid.
                       - Must be from `int<mis>:keywords:category/input' or
                         `int<mis>:keywords:category/internal'."
  (let ((caller (list 'int<mis>:parse caller)))
    ;;------------------------------
    ;; Error Checking
    ;;------------------------------
    ;; Is CATEGORY a valid type?
    (cond ((and (not (null category))      ; nil
                (not (keywordp category))  ; keyword
                (not (and (listp category) ; list of keywords
                          (seq-every-p #'keywordp category))))
           (int<mis>:error caller
                           '("CATEGORY must be nil, a keyword, or a list of keywords. "
                             "Got %S: %S")
                           (type-of category)
                           category))

          ;; Is the single keyword CATEGORY a valid keyword?
          ((and (keywordp category)
                (not (int<mis>:valid:category/mis/input? 'mis category)))
           (int<mis>:error caller
                           "CATEGORY keyword must be a member of %S or nil. Got: %S"
                           int<mis>:keywords:category/input
                           category))

          ;; Does the list of keywords CATEGORY contain only valid keywords?
          ((and (listp category)
                (not (seq-every-p (lambda (cat)
                                    "Is every CATEGORY keyword valid?"
                                    (int<mis>:valid:category/mis/input? 'mis cat))
                                  category)))
           (int<mis>:error caller
                           int<mis>:keywords:category/input
                           category))

          ;; Don't want any circular lists.
          ((not (proper-list-p args))
           (int<mis>:error caller
                           "ARGS must be a proper list (no circular references)! Got: %S"
                           args))

          (t
           nil))

    ;;------------------------------
    ;; Normalize Categories
    ;;------------------------------
    ;; Normalize to list of keywords.
    (when (keywordp category)
      (setq category (list category)))

    ;;------------------------------
    ;; Parse Args
    ;;------------------------------
    (let ((parsing t)
          (args/len (length args))
          ast/parsed ; Validated/normalized kvps alist by keyword category.
          ast/in     ; Input Mis ASTs.
          ast/out)   ; Output Mis Abstract Syntax Tree.

      (while (and args
                  (> args/len 1)
                  parsing)
        (if (not (keywordp (car args)))
            ;;---
            ;; Done; no more kvps to parse; rest is either Mis ASTs or messaging stuff.
            ;;---
            (setq parsing nil)

          ;;---
          ;; Validate a key/value pair.
          ;;---
          (setq args/len (- args/len 2))
          (if-let* ((key           (pop args))
                    (validator     (int<mis>:valid:validator caller category key))
                    (validator-cat (car validator)) ; parsed category (internal or tmp)
                    (validator-fn  (cdr validator))
                    (value         (funcall validator-fn caller key (pop args))))
              ;; Key/value exist and are now known to be valid; save to the AST
              ;; for this parsed category.
              (let ((ast/cat (alist-get validator-cat ast/parsed)))
                (setf (alist-get key ast/cat) value)
                (setf (alist-get validator-cat ast/parsed) ast/cat))

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

      ;;------------------------------
      ;; Deal with any pre-parsed ASTs in ARGS.
      ;;------------------------------
      (when args
        (let (misc)
          ;; Split Mis ASTs out into `ast/in' var.
          (dolist (arg args)
            (if (int<mis>:valid:syntax? caller 'arg arg :no-error)
                (push (cons :mis arg) ast/in)
              (push arg misc)))

          ;; Set ARGS as "everything left over now".
          (setq args (nreverse misc))))

      ;;------------------------------
      ;; Build AST: Presume that the rest of ARGS are message/formatting.
      ;;------------------------------
      (when args
        (if (and (= (length args) 1)
                 (stringp (nth 0 args)))
            ;; Just a single string; use `:mis:string' to simplify things later.
            (push (cons :mis:string (nth 0 args)) ast/out)
          ;; Generic message format string and/or args go into the `:mis:message'.
          (push (cons :mis:message args) ast/out)))

      ;;------------------------------
      ;; Build AST: Add pre-existing ASTs.
      ;;------------------------------
      ;; Insert each as a child AST of `ast/out'.
      (dolist (child ast/in)
        (push child ast/out))

      ;;------------------------------
      ;; Build AST: Add new ASTs.
      ;;------------------------------
      ;; Keep separate from the parsing so we enforce a more human-friendly
      ;; ordering to the alists, though it won't matter to the builder/compiler.
      ;; Just friendly to the programmer to have these first in the output alist.
      (dolist (ast ast/parsed ast/out)
        (push ast ast/out)))))
;; (int<mis>:parse 'tester nil :align 'center "hello")
;; (int<mis>:parse 'tester :style :align 'center "hello")
;; (int<mis>:parse 'tester nil :align 'center "hello %s" "world")
;; (int<mis>:parse 'tester nil :type 'block :align 'center "hello %s" "world")
;; (int<mis>:parse 'tester nil :align 'center :width 11 "hello %s" "world")
;; (int<mis>:parse 'tester nil :align 'center :width 11 "hello")
;; (int<mis>:parse 'tester nil :indent 'auto "hello")
;;
;; Parse an AST -> Get a `:mis' AST.
;; (int<mis>:parse 'test '(:comment :style) '((:mis:format (:formatter repeat :string "-"))))
;;   -get-> ((:mis (:mis:format (:formatter repeat :string "-"))))
;;   -aka-> ((:mis . ((:mis:format (:formatter repeat :string "-")))))
;;
;; TODO: More complicated:
;; (int<mis>:parse 'test '(:comment :style :mis:format) :type 'inline "hello" '(:mis:format (:formatter repeat :string "-")))
;;
;; Error: `:align' is a `:mis:style', not a `:mis:comment' category keyword.
;;   (int<mis>:parse 'tester :comment :mis:comment :align 'center "hello")


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'mis-parse)
;;; mis-parse.el ends here

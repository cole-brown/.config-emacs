;;; mis-tree-syntax.el --- Mis Syntax Tree Functions -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <code@brown.dev>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2022-09-26
;; Modified:   2022-09-26
;;
;;
;;; Commentary:
;;
;;  Mis Syntax Tree Functions
;;
;;; Code:


(require 'mis-error)


;;------------------------------------------------------------------------------
;; Syntax Trees
;;------------------------------------------------------------------------------

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
    (list pair)))
;; (int<mis>:syntax:get/syntax 'test :style '((:style (:width . 10) (:align . :center))))


(defun int<mis>:syntax:get/style (caller category syntax)
  "Get the Mis Style Syntax Tree from CATEGORY in SYNTAX.

It should be used as the primary source of styling. The parents' / ancestors'
styling should be used as a fallback/secondary source.

CATEGORY should be a keyword from `int<mis>:keywords:category/internal`.

SYNTAX should be a Mis Syntax Tree.

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)"
  ;;------------------------------
  ;; Special Case: We want the `:style' from `:style'.
  ;;------------------------------
  ;; Filter `:style' branch down to just styling and return it.
  (if (eq category :style)
      (int<mis>:syntax:filter/style caller syntax)

    ;;------------------------------
    ;; Normal Case
    ;;------------------------------
    ;; Get `:style' from CATEGORY.
    (when-let ((style/assoc (int<mis>:syntax:get/pair caller
                                                      :style
                                                      (int<mis>:syntax:get/value caller
                                                                                 category
                                                                                 syntax))))
    ;; Convert assoc into alist.
    (list style/assoc))))
;; (int<mis>:syntax:get/style 'test :mummy '((:mummy (:style (:width . 10) (:align . :center)))))
;; (int<mis>:syntax:get/style 'test :mummy '((:mummy (:test (:width . 10) (:align . :center)))))
;; (int<mis>:syntax:get/style 'test :style (mis:style :align 'center "Hello there."))


(defun int<mis>:syntax:filter/style (caller syntax)
  "Filter SYNTAX down to only its style entries.

SYNTAX should be nil or a Mis Syntax Tree with a `:style' entry.

Return SYNTAX with non-style entries filtered out, or nil if nothing remains
after filtering.
Example 1:
  (mis:style :align 'center \"Hello there.\")
    -> '((:style (:children (:format (:formatter . string)
                                     (:value . \"Hello there.\")))
                 (:align . center)))
  (int<mis>:style:filter (mis:style :align 'center \"Hello there.\"))
    -> '((:style (:align . center)))

Example 2:
  (mis:style \"Hello there.\")
    -> '((:style (:children (:format (:formatter . string)
                                     (:value . \"Hello there.\")))))
  (int<mis>:style:filter (mis:style :align 'center \"Hello there.\"))
    -> nil

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)"
  (let* ((caller (list 'int<mis>:style:filter caller))
         ;; All entries under `:style'.
         (entries/style (int<mis>:syntax:get/value caller :style syntax))
         ;; Only the entries under `:style' that are styling.
         entries/filtered)

    ;;------------------------------
    ;; Sanity Checks
    ;;------------------------------
    (unless entries/style
      (int<mis>:error caller
                      "SYNTAX has no `:style' to filter. SYNTAX: %S, `:style' entries: %S"
                      syntax
                      entries/style))

    ;;------------------------------
    ;; Filtering
    ;;------------------------------
    (dolist (entry entries/style)
      (when (memq (car entry) int<mis>:keywords:style)
        (push entry
              entries/filtered)))

    (when entries/filtered
      (apply #'int<mis>:syntax:create
             caller
             :style
             (nreverse entries/filtered)))))
;; (int<mis>:syntax:filter/style 'test (mis:style :align 'center "Hello there."))
;; (int<mis>:syntax:filter/style 'test (mis:style "Hello there."))


(defun int<mis>:syntax:merge/style (caller category syntax style/parent)
  "Merge `:style' in SYNTAX and STYLE/PARENT together.

CATEGORY should be a keyword from `int<mis>:keywords:category/internal'. It is
the category in SYNTAX that we will look under for styling.

SYNTAX should be nil or a Mis Syntax Tree.

STYLE/PARENT should be nil or a Mis Syntax Tree.

Return nil or a Mis Syntax Tree of only `:style'.

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)"
  (let* ((caller (list 'int<mis>:style:merge caller))
         (style/child (int<mis>:syntax:get/style caller
                                                 category
                                                 syntax)))
    (if (null style/parent)
        ;; Nothing to merge; return SYNTAX's style (may be nil).
        style/child

      ;; Merge parent styling into child styling. Child takes precedence.
      (let* ((styling/child (int<mis>:syntax:get/value caller :style style/child))
            ;; Start off with all of parent's values...
             (styling/output (copy-alist (int<mis>:syntax:get/value caller :style style/parent))))

        ;; Add/overwrite child's values
        (dolist (assoc/child styling/child)
          (setf (alist-get (car assoc/child) styling/output) (cdr assoc/child)))

        ;; Turn it into a Mis Syntax Tree and return.
        (apply #'int<mis>:syntax:create
               caller
               :style
               styling/output)))))
;; (int<mis>:syntax:merge/style 'test :format (mis:string :width 42 "hi") (mis:style :width 20 :padding "-"))
;; (int<mis>:syntax:merge/style 'test :format (mis:string :width 42 "hi") nil)
;; (int<mis>:syntax:merge/style 'test :style (mis:style :align 'center "Hello there.") nil)


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
;; (int<mis>:syntax:append 'test '((:comment (:prefix:major . ";; ") (:postfix:major . "") (:type . default))) '((:style (:width . 10) (:align . :center))))


(defun int<mis>:syntax:update (caller key syntax &rest kvp)
  "Add/overwrite KVPs to Mis SYNTAX tree under KEY.

KVP should (each) be a cons of a KEY keyword and its value.
example: '(:width . 10)

SYNTAX should be a Mis Syntax Tree. It will be updated with KVPs and
the updated value returned. Caller should set the return value back to the input
arg as the update is not guaranteed to be in-place.
Example:
  (setq syntax (int<mis>:syntax:update 'example
                                       :style
                                       syntax
                                       '(:align . :center)))

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

SYNTAX should be a Mis Syntax Tree. It will be updated with KVPs and
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

SYNTAX should be a Mis Syntax Tree. Caller should set the return value
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

SYNTAX should be a Mis Syntax Tree.

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


(defun int<mis>:syntax:has (caller syntax &rest key)
  "Return non-nil if KEY is a top-level alist key for Mis SYNTAX Tree.
Return non-nil if KEYs are a valid path into the Mis SYNTAX Tree.
\"Valid\" includes having the keys but the value being nil.
Example:
  (int<mis>:syntax:has 'test (mis:style) :style)

SYNTAX should be a Mis Syntax Tree.

KEYs should be keywords to follow down the SYNTAX tree to find the value.

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)"
  (let ((caller (list 'int<mis>:syntax:has caller))
        (value syntax))
    (dolist (find key)
      (setq value (int<mis>:syntax:get/value caller find value :mis:does-not-exist)))
    ;; Return non-nil if we found that sequence of keys in the tree.
    (not (eq value :mis:does-not-exist))))
;; (int<mis>:syntax:has 'test '((:style (:width . 10) (:align . :center))) :style)
;; (int<mis>:syntax:has 'test '((:style (:width . 10) (:align . :center))) :jeff)
;; (int<mis>:syntax:has 'test '((:style (:width . 10) (:align . :center))) :style :width)
;; (int<mis>:syntax:has 'test '((:style (:width . 10) (:align . :center))) :style :width :left)


;; TODO: Delete? Change?
(defun int<mis>:syntax:merge (caller syntax/to syntax/from &rest ignore/from)
  "Merge two Mis Syntax Trees, ignoring IGNORE/FROM keys.

SYNTAX/TO and SYNTAX/FROM should be Mis Syntax Trees. SYNTAX/FROM will
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

SYNTAX should be a Mis Syntax Trees.

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
;; The End.
;;------------------------------------------------------------------------------
(provide 'mis-tree-syntax)
;;; mis-tree-syntax.el ends here

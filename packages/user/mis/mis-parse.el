;;; mis-parse.el --- Parsing for Mis function calls -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <code@brown.dev>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2019-10-23
;; Timestamp:  2023-06-29
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
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
;;   - Return a Mis Syntax Tree of themselves, basically.
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
;;; Code:


(require 'cl-lib)

(require 'mis-valid)
(require 'mis-error)
(require 'mis-tree-syntax)
(require 'mis-tree-output)


;;------------------------------------------------------------------------------
;; Parsing
;;------------------------------------------------------------------------------

(defun int<mis>:parse:string (caller &rest args)
  "Parse message ARGS into a Mis Syntax Tree.

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)"
  (let ((caller (list 'int<mis>:parse:string caller))
        (args/len (length args))
        syntax)
    (int<mis>:debug caller "args:      %S" args)
    (when args
      (cond ((and (= args/len 1)
                  (keywordp (nth 0 args))
                  (eq :children (nth 0 args)))
             ;; Once you get it by compiling/finalizing your children, it should
             ;; just be a string, so just `string' formatter is fine.
             (setq syntax (int<mis>:syntax:create caller
                                                  :format
                                                  (cons :formatter 'string)
                                                  (cons :value 'child))))

            ((and (= args/len 1)
                  (stringp (nth 0 args)))
             ;; Just a single string.
             (setq syntax (int<mis>:syntax:create caller
                                                  :format
                                                  (cons :formatter 'string)
                                                  ;; Just the one string.
                                                  (cons :value (nth 0 args)))))

            ((and (= args/len 1)
                  (characterp (nth 0 args)))
             ;; Just a single character.
             (setq syntax (int<mis>:syntax:create caller
                                                  :format
                                                  (cons :formatter 'char)
                                                  ;; Just the one string.
                                                  (cons :value (nth 0 args)))))

            (t
             ;; A `format' string and args.
             (setq syntax (int<mis>:syntax:create caller
                                                  :format
                                                  (cons :formatter 'format)
                                                  ;; Everything!
                                                  (cons :value args))))))
    (int<mis>:debug caller "<--syntax: %S" syntax)
    syntax))


(defun int<mis>:parse (caller category valid &rest args)
  "Parse ARGS into a Mis Syntax Tree.

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
    (int<mis>:debug caller "category:              %S" category)
    (int<mis>:debug caller "valid cats:            %S" valid)
    (int<mis>:debug caller "args:                  %S" args)

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
                (int<mis>:debug caller "  - category: . . . . . . %S" category/validator)
                (int<mis>:debug caller "  - value:                %S" value)
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
      (int<mis>:debug caller "remaining args:        %S" args)

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
      (int<mis>:debug caller "parsed has out cat?:   %S" (int<mis>:syntax:has caller syntax/parsed category/out))
      (when (int<mis>:syntax:has caller
                                 syntax/parsed
                                 category/out)
        (int<mis>:debug caller "move: syntax/parsed:   %S" syntax/parsed)
        ;; Move `syntax/parsed' to `syntax/out'.
        (setq syntax/out (int<mis>:syntax:merge caller
                                                syntax/out
                                                syntax/parsed)
              ;; Clear so rest of the function can check it correctly.
              syntax/parsed nil)
        (int<mis>:debug caller "to:   syntax/out:      %S" syntax/out))
      (int<mis>:debug caller "syntax/parsed final:   %S" syntax/parsed)

      ;;------------------------------
      ;; Deal with MSTs, strings, etc. in ARGS.
      ;;------------------------------
      (let (args/non-mst)
        (when args
          (int<mis>:debug caller "args left over:        %S" args))

        (while args
          (let ((arg (pop args)))
            (if (not (int<mis>:valid:syntax? caller 'arg arg :no-error))
                ;;---
                ;; Non-MSTs
                ;;---
                ;; Gather as many as possible, then create one MST to append to `syntax/in'.
                (push arg args/non-mst)
              ;;---
              ;; MSTs
              ;;---
              ;; First, we might need to finalize & append a `args/non-mst' grouping.
              (when args/non-mst
                (setq syntax/in (int<mis>:syntax:append caller
                                                        syntax/in
                                                        (apply #'int<mis>:parse:string
                                                               caller
                                                               (nreverse args/non-mst)))
                      args/non-mst nil))
              ;; Now append the MST.
              (setq syntax/in (int<mis>:syntax:append caller
                                                      syntax/in
                                                      arg)))))

        ;; Done parsing `args'; we might need to finalize & append some
        ;; `args/non-mst' from the end of `args'.
        (when args/non-mst
          (int<mis>:debug caller "args, non-syntax:      %S" args/non-mst)
          (let ((syntax/string (apply #'int<mis>:parse:string
                                      caller
                                      (nreverse args/non-mst))))
            ;; If the parsing CATEGORY is `:string' and we have no children,
            ;; this last batch is the root string formatter.
            (if (and (eq category :string)
                     (null syntax/in))
                (setq syntax/out (int<mis>:syntax:merge caller syntax/out syntax/string))
              ;; Otherwise it's just another child syntax tree to add to `syntax/in'.
              (setq syntax/in    (int<mis>:syntax:append caller
                                                         syntax/in
                                                         syntax/string)
                    args/non-mst nil)))))

      ;; Another `:string' special case: When the `:string' has no string to
      ;; string - it needs to get a string from its children.
      (when (and (eq category :string)
                 (null syntax/out))
        (setq syntax/out (int<mis>:parse:string caller :children)))

      (int<mis>:debug caller "syntax/out:            %S" syntax/out)
      (int<mis>:debug caller "syntax/in:             %S" syntax/in)

      ;; Correct `syntax/in' to a proper Mis syntax tree for branches.
      (when syntax/in
        (setq syntax/out/children (apply #'int<mis>:syntax:create
                                         caller
                                         :children
                                         syntax/in)))
      (int<mis>:debug caller "syntax/out/children:   %S" syntax/out/children)

      ;;------------------------------
      ;; Build Syntax Tree: Add new trees.
      ;;------------------------------
      ;; Keep separate from the parsing so we enforce a more human-friendly
      ;; ordering to the alists, though it won't matter to the builder/compiler.
      ;; Just friendly to the programmer to have these +first+ last in the final
      ;; output alist.
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
        (setq syntax/out (apply #'int<mis>:syntax:children
                                caller
                                category/out
                                syntax/out
                                (int<mis>:syntax:get/value caller
                                                           :children
                                                           syntax/out/children))))
      (int<mis>:debug caller "<--parsed syntax:   %S" syntax/out)
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
;; string inside "string" w/ just style otherwise
;;
;; Parse a syntax tree -> Get a syntax tree.
;; (int<mis>:parse 'test :comment '(:comment :style) '((:format (:formatter repeat :string "-"))))
;;
;; Error: `:align' is a `:style', not a `:comment' category keyword.
;;   (int<mis>:parse 'test :comment :comment :align 'center "hello")
;;
;; Make sure MSTs and strings stay in order.
;; (int<mis>:parse 'test
;;                 :comment
;;                 '(:comment :style) ; Also allow styling in our comments.
;;                 :width 80
;;                 :padding " "
;;                 (mis:line "0")
;;                 "\n"
;;                 (mis:line "1")
;;                 "\n"
;;                 (mis:line "2"))
;;
;; String w/ styling inside string w/ styling:
;; (int<mis>:parse 'test :string '(:style :string)
;;                 :padding "-"
;;                 :align 'center
;;                 (int<mis>:parse 'test :string '(:style :string)
;;                                 :padding " "
;;                                 :indent '(:left 5 :right 5)
;;                                 "string with spaces padding it"))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'mis-parse)
;;; mis-parse.el ends here

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
(require 'mis-tree-syntax)
(require 'mis-tree-output)


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

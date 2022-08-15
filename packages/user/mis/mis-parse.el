;;; mis-parse.el --- Parsing for Mis function calls -*- lexical-binding: t; -*-
;;
;; Author: Cole Brown <code@brown.dev>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:  2019-10-23
;; Modified: 2022-08-09
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
;; resolved, but the top-level function (`mis', `mis:comment', etc) needs the
;; full, unaltered tree structure to correctly compile the output.
;;
;; So Mis functions like `mis:style' must:
;;   - Evaluate their arguments.
;;   - Return themselves, basically.
;; E.g.
;;   (mis:style :bold (get-greeted))
;;     -> '(mis:style :bold "world")
;;        or simiar, like: '(:styles '(:bold) "world")
;;
;;; Code:


(require 'cl-lib)

(require 'mis-valid)
(require 'mis-error)


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
  - nil                - All categories are valid.
  - a keyword          - Only this category's keywords are valid
  - a list of keywords - Only these categories' keywords are valid"
  (let ((caller (list 'int<mis>:parse caller)))
    ;;------------------------------
    ;; Error Checking
    ;;------------------------------
    ;; Is CATEGORY a valid type?
    (cond ((and (not (null category)) ; nil
                (not (keywordp category)) ; keyword
                (not (and (listp category) ; list of keywords
                          (seq-every-p #'keywordp category))))
           (int<mis>:error caller
                           '("CATEGORY must be nil, a keyword, or a list of keywords. "
                             "Got %S: %S")
                           (type-of category)
                           category))

          ;; Is the single keyword CATEGORY a valid keyword?
          ((and (keywordp category)
                (not (memq category int<mis>:keywords:category)))
           (int<mis>:error caller
                           "CATEGORY must be a member of %S or nil. Got: %S"
                           int<mis>:keywords:category
                           category))

          ;; Does the list of keywords CATEGORY contain only valid keywords?
          ((and (listp category)
                (not (seq-every-p (lambda (cat)
                                    "Is every CATEGORY keyword valid?"
                                    (memq cat int<mis>:keywords:category))
                                  category)))
           (int<mis>:error caller
                           "All members of CATEGORY be a member of %S. CATEGORY: %S"
                           int<mis>:keywords:category
                           category))

          ;; Don't want any circular lists.
          ((not (proper-list-p args))
           (int<mis>:error caller
                           "ARGS must be a proper list (no circular references)! Got: %S"
                           args))

          (t
           nil))

    ;; Normalize category to to a list of keywords.
    (unless (listp category)
      (setq category (list category)))

    (let ((parsing t)
          normalized ; Validated/normalized kvps alist by keyword category.
          ast        ; Output Mis Abstract Syntax Tree.
          (args/len (length args)))

      ;;------------------------------
      ;; Build normalized plists out of args.
      ;;------------------------------
      (while (and args
                  (> args/len 2)
                  parsing)
        (if (not (keywordp (car args)))
            ;;---
            ;; Done; no more kvps to parse; rest is messaging stuff.
            ;;---
            (setq parsing nil)

          ;;---
          ;; Validate a key/value pair.
          ;;---
          (setq args/len (- args/len 2))
          (if-let* ((key           (pop args))
                    (validator     (int<mis>:valid:validator caller category key))
                    (validator-cat (car validator))
                    (validator-fn  (cdr validator))
                    (value         (funcall validator-fn caller key (pop args))))
              ;; Key/value exist and are now known to be valid; save.
              (let ((plist (alist-get validator-cat normalized)))
                ;; Sanity check category.
                (unless (or (null category)
                            (memq validator-cat category))
                  (int<mis>:error caller
                                  '("Keyword `%S' is not valid for categories `%S'? "
                                    "Got category `%S' instead.")
                                  key
                                  category
                                  validator-cat))

                ;; Save to the normalized output forwards; not reversing the list(s) later.
                (push value plist)
                (push key   plist)
                (setf (alist-get validator-cat normalized) plist))

            ;; There /should/ have been an error raised?
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
      ;; Return w/ rest of ARGS.
      ;;------------------------------
      (when args
        ;; Any message string and/or message args go into the `:message' kvp.
        (push args ast)
        (push :message ast))

      ;; Finalize & return the AST.
      (dolist (assoc/category normalized ast)
        ;; Category's key/value pairs plist.
        (push (cdr assoc/category) ast)
        ;; Category's keyword.
        (push (car assoc/category) ast)))))
;; (int<mis>:parse 'tester nil :align 'center "hello")
;; (int<mis>:parse 'tester :style :align 'center "hello")
;; (int<mis>:parse 'tester nil :align 'center "hello %s" "world")
;; (int<mis>:parse 'tester nil :type 'block :align 'center "hello %s" "world")
;; (int<mis>:parse 'tester nil :align 'center :width 11 "hello %s" "world")
;; (int<mis>:parse 'tester nil :align 'center :width 11 "hello")
;; (int<mis>:parse 'tester nil :indent 'auto "hello")
;;
;; Error: `:align' is a `:style', not a `:comment' category keyword.
;;   (int<mis>:parse 'tester :comment :align 'center "hello")


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'mis-parse)
;;; mis-parse.el ends here

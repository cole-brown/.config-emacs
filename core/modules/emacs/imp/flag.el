;;; feature.el --- Innit Feature Flags -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2022-04-14
;; Modified:   2022-04-14
;; URL:        https://github.com/cole-brown/.config-emacs
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;  Set & get Feature Flags during initialization.
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Variables
;;------------------------------------------------------------------------------

(defvar innit:feature:flags nil
  "Alist of feature to flags (including \"+\" or \"-\" signs).")


;;------------------------------------------------------------------------------
;; Flags
;;------------------------------------------------------------------------------

(defun int<innit>:feature:split (feature)
  "Splits FEATURE into +/- sign and feature name.

Errors if no +/- sign present.

Returns keyword cons: (sign-keyword . name-keyword)"
  (let ((name (symbol-name feature))
        (regex (rx string-start
                   (group
                    (or "+" "-"))
                   (group
                    (one-or-more printing))
                   string-end)))

    (cond ((not (string-match regex name))
           (error "%s: FEATURE doesn't conform to requirements! Regex '%s' didn't match FEATURE '%s'"
                  "int<innit>:feature:split"
                  regex
                  name))

          ((string-match-p (rx string-start (or "+" "-")) (match-string 2 name))
           (error "%s: FEATURE '%s' doesn't conform to requirements! Name must not start with '+' or '-'! Got: '%s'"
                  "int<innit>:feature:split"
                  name
                  (match-string 2 name)))

          ;; Ok; return the split symbol and feature name as keywords:
          (t
           (cons (intern (concat ":" (match-string 1 name)))
                 (intern (concat ":" (match-string 2 name))))))))
;; (int<innit>:feature:split '+foo)
;; (int<innit>:feature:split '-foo)
;; (int<innit>:feature:split '-+foo)


(defun int<innit>:feature:compare (feature-a feature-b)
  "Compare FEATURE-A against FEATURE-B.

Returns nil if they are unrelated.
Returns non-nil if they are related.
  - Returns 0 if equal:  +foo +foo
  - Returns +1 if A > B: +foo -foo
  - Returns -1 if A < B: -foo +foo

Errors if either feature match naming requirements."
  (let* ((split-a (int<innit>:feature:split feature-a))
         (sign-a (car split-a))
         (name-a (cdr split-a))
         (split-b (int<innit>:feature:split feature-b))
         (sign-b (car split-b))
         (name-b (cdr split-b)))
    ;; Unrelated?
    (cond ((not (eq name-a name-b))
           nil)
          ;; Exactly the same?
          ((eq sign-a sign-b)
           0)
          ;; Same name, different signs.
          ((eq sign-a :+)
           1)
          (t
           -1))))
;; (int<innit>:feature:compare '+foo '+bar)
;; (int<innit>:feature:compare '+foo '+foo)
;; (int<innit>:feature:compare '+foo '-foo)
;; (int<innit>:feature:compare '-foo '+foo)


;;------------------------------------------------------------------------------
;; Check for Feature Flags
;;------------------------------------------------------------------------------

(defun int<innit>:feature:exists? (module feature)
  "Check if MODULE has FEATURE flagged either way already.
MODULE should be a keyword.

FEATURE should be a symbol name that starts with a \"+\" or \"-\" sign.

Examples:
  (innit:feature? :numbers +random)
  (innit:feature? :numbers -negative)

Returns non-nil if MODULE has +/- FEATURE flag already, nil if not.
Specifically, returns result of:
  (int<innit>:feature:compare FEATURE existing-feature-matched)"
  (let ((module-features (alist:keyword:get/value module innit:feature:flags))
        found?)
    ;; Search for the feature flag.
    (while (and module-features
                (not found?))
      (setq found? (int<innit>:feature:compare feature
                                               (pop module-features))))
    ;; Return result of search.
    found?))
;; (setq innit:feature:flags '((:foo . (+bar))))
;; (int<innit>:feature:exists? :foo '+bar)


(defmacro innit:feature? (module feature)
  "Check if MODULE has FEATURE flagged.
MODULE should be a keyword.

FEATURE should be a symbol name that starts with a \"+\" or \"-\" sign.

Examples:
  (innit:feature? :numbers +random)
  (innit:feature? :numbers -negative)

Returns non-nil if MODULE has FEATURE flag, nil if not."
  ;; And with true to avoid "void function" error.
  (and (memq feature (alist:keyword:get/value module innit:feature:flags))
       t))
;; (setq innit:feature:flags '((:foo . (+bar))))
;; (innit:feature? :foo +bar)


;;------------------------------------------------------------------------------
;; Set Feature Flags
;;------------------------------------------------------------------------------

(defmacro innit:features (module &rest feature)
  "Set FEATURE flag(s) for MODULE.

MODULE should be a keyword.

FEATURE should be one or more symbol names that start with a \"+\" or \"-\"
sign.

Example:
  (innit:features :numbers +random -negative)
    -> This sets feature flags for the `:numbers' module/package/whatever to:
       - Include optional `random' numbers feature.
       - Exclude optional `negative' numbers feature."
  ;;------------------------------
  ;; Error checks...
  ;;------------------------------
  (unless (keywordp module)
    (error "innit:features: MODULE must be a keyword, got: %S"
           module))

  (unless feature
    (error "innit:features: `%S' must have one or more features to add/remove, got: %S"
           module
           feature))

  ;;------------------------------
  ;; Process features (w/ error checks)...
  ;;------------------------------
  `(let* ((macro<innit>:module            ,module)
          (macro<innit>:features:add      ',feature)
          (macro<innit>:features:existing (alist:keyword:get/value macro<innit>:module innit:feature:flags))
          (macro<innit>:features:update   macro<innit>:features:existing))
     ;; First check all input features against existing and error if any cannot be added.
     ;; Then we can do the actual updated as all-or-nothing.
     (dolist (macro<innit>:feature macro<innit>:features:add)
       (if (int<innit>:feature:exists? macro<innit>:module
                                       macro<innit>:feature)
           ;; Feature is invalid; error out now.
           (error "innit:features: `%S' is already flagged for feature matching `%S'. Existing features: %S"
                  macro<innit>:module
                  macro<innit>:feature
                  (alist:keyword:get/value macro<innit>:module innit:feature:flags))

         ;; Feature is valid; add to the update list.
         (push macro<innit>:feature macro<innit>:features:update)))

     ;;------------------------------
     ;; Add features.
     ;;------------------------------
     ;; Replace existing feature list with the new, updated list.
     (alist:keyword:update macro<innit>:module
                           macro<innit>:features:update
                           innit:feature:flags)

     ;;------------------------------
     ;; Return full feature list for module.
     ;;------------------------------
     macro<innit>:features:update))
;; innit:feature:flags
;; ;; OK:
;; (innit:features :foo +bar)
;; ;; Fail - already has +bar can't add -bar:
;; (innit:features :foo -bar)
;; ;; OK: multiple features
;; (innit:features :foo -baz +qux +quux)


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide:with-emacs :innit 'feature)

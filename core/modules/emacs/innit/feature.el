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
;; The End.
;;------------------------------------------------------------------------------
(imp:provide:with-emacs :innit 'feature)

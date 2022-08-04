;;; elisp/utils/types.el -*- lexical-binding: t; -*-


;;------------------------------------------------------------------------------
;; Type checking
;;------------------------------------------------------------------------------

(defun elisp:cons? (var)
  "Is VAR a cons and not a list?

Apparently lists qualify as conses as far as `consp' cares, so... fucking elisp,
yeah? ...and I can't `listp' or `length' a cons because it's not a list...?!?!

Jesus fuck Emacs Lisp. I just want to check for actual conses...

https://emacs.stackexchange.com/questions/10489/predicate-function-for-dotted-pairs"
  (declare (pure t) (side-effect-free t))
  (and (listp var)
       (cdr var)
       (atom (cdr var))))
;; (elisp:cons? 1)
;; (elisp:cons? '(1 . 2))
;; (elisp:cons? '(1 2))
;; (elisp:cons? '(1))
;; (elisp:cons? '(1 . nil))


(defun elisp:list/proper? (var)
  "Is VAR an actual, proper list?

VAR must be a list and must not be:
  1. circular
  2. dotted (e.g. a `cons' cell)"
  (declare (pure t) (side-effect-free t))
  (proper-list-p var))


(defun elisp:list/any? (var)
  "Is VAR anything that Emacs Lisp considers a \"list\"?

Includes:
  1. nil
  2. conses
  3. proper lists
  4. lists with circular references
  5. etc"
  (declare (pure t) (side-effect-free t))
  (listp var))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :elisp 'utils 'types)

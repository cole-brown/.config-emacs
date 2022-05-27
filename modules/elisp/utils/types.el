;;; elisp/utils/types.el -*- lexical-binding: t; -*-



;;------------------------------------------------------------------------------
;; Imports
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; Type checking
;;------------------------------------------------------------------------------


(defun elisp:cons? (var)
  "Is VAR a cons and not a list?

Apparently lists qualify as conses as far as `consp' cares, so... fucking elisp,
yeah? ...and I can't `listp' or `length' a cons because it's not a list...?!?!

Jesus fuck Emacs Lisp. I just want to check for actual conses...

https://emacs.stackexchange.com/questions/10489/predicate-function-for-dotted-pairs"
  (and (cdr var)
       (atom (cdr var))))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :elisp 'utils 'types)

;;; elisp/utils/functions.el -*- lexical-binding: t; -*-


(imp:require :elisp 'utils 'types)


;;------------------------------------------------------------------------------
;; Imports
;;------------------------------------------------------------------------------

;; Don't rely on anything more than the core modules... or anything at all?
;; This should be low-level stuff for use by other code.


;;------------------------------------------------------------------------------
;; Delete Functions
;;------------------------------------------------------------------------------

(defun elisp:func:delete (symbol)
  "Delete a function by its SYMBOL by calling:
1. `fmakunbound' - Removes the function definition.
2. `makunbound'  - Removes the variable binding.
3. `unintern'    - Removes the symbol name from the symbol table."
  (fmakunbound symbol)
  (makunbound symbol)
  (unintern symbol nil))


;;------------------------------------------------------------------------------
;; List Functions
;;------------------------------------------------------------------------------

;; TODO: move to a list utils module?
;;   - I think I have other list util functions hiding... somewhere.
;;     - Maybe in `alist'...
(defun elisp:list:flatten (args)
  "Return ARGS as a single, flat list.

If ARGS is a list, it must be a proper list (no circular lists or conses)."
  (declare (pure t) (side-effect-free t))
  (cond
   ;; If it's a list, flatten it.
   ((elisp:list/proper? args)
    (seq-mapcat #'elisp:list:flatten args))
   ;; If it's a cons, convert into a list and flatten.
   ((elisp:cons? args)
    (elisp:list:flatten (list (car args) (cdr args))))
   ;; Else it's now a list.
   (t
    (list args))))
;; (elisp:list:flatten 1)
;; (elisp:list:flatten '(1 . 2))
;; (elisp:list:flatten '(1 2 3))
;; (elisp:list:flatten '(1 (2 3)))


;; TODO: move to a list utils module?
;;   - I think I have other list util functions hiding... somewhere.
;;     - Maybe in `alist'...
(defun elisp:list:listify (args)
  "Ensure ARGS is a list.

Return ARGS wrapped in a list, or as-is if already a list.

From Doom's `doom-enlist'."
  (declare (pure t) (side-effect-free t))
  (if (proper-list-p args)
      args
    (list args)))


;;------------------------------------------------------------------------------
;; Unquote List/Function
;;------------------------------------------------------------------------------

(defun elisp:unquote (arg)
  "Return ARG unquoted.

Removes both `quote' ('foo) and `function' (#'foo) style quoting.

Originaly from `doom-unquote'."
  (declare (pure t) (side-effect-free t))
  (while (memq (car-safe arg) '(quote function))
    (setq arg (cadr arg)))
  arg)


;;------------------------------------------------------------------------------
;; Interactive Commands
;;------------------------------------------------------------------------------

(defmacro elisp:cmd (&rest body)
  "Return (lambda () (interactive) ,@BODY)

A factory for quickly producing interaction commands, particularly for keybinds
or aliases.

Proudly nicked from Doom's core-lib.el."
  (declare (doc-string 1) (pure t) (side-effect-free t))
  `(lambda (&rest _) (interactive) ,@body))


(defmacro elisp:cmd/with-args (command &optional prefix-arg &rest args)
  "Return a closure that interactively calls COMMAND with ARGS and PREFIX-ARG.

Like `elisp:cmd', but allows you to change `current-prefix-arg' or pass arguments to
COMMAND. This macro is meant to be used as a target for keybinds (e.g. with
`define-key' or `general-def').

Proudly nicked from Doom's core-lib.el."
  (declare (doc-string 1) (pure t) (side-effect-free t))
  `(lambda (arg &rest _) (interactive "P")
     (let ((current-prefix-arg (or ,prefix-arg arg)))
       (,(if args
             'funcall-interactively
           'call-interactively)
        ,command ,@args))))


;;------------------------------------------------------------------------------
;; Functions for argument parsing.
;;------------------------------------------------------------------------------

(defun elisp:parse:args+kwargs (input &rest claims)
  "Parse INPUT into args (list) and keyword args (plist).

Expects INPUT to be a list of:
  - Some args.
  - Followed by some keyword args.

That is, INPUT should be something like:
'((list of args) \"whatever\" ... et al :k0 v0 :k1 v1 ... :kn vn)

CLAIMS should be keywords that the caller expects. It will be
flattened to a single list of keywords to look for.

Splits INPUT list into the args and the keyword args, where
`:k0' to `:kn' are keywords in CLAIMS.

Once any of the keyword args in CLAIMS is found, that and the
rest of INPUT are assumed to be the keyword args. Everything
before will be the returned args, and that keyword and everything
after will be the returned keyword args.

So this:
  '((list of args) \"whatever\" ... et al :k0 v0 :k1 v1 ... :kn vn)
Becomes this:
  '(((list of args) \"whatever\" ... et al)
    (:k0 v0 :k1 v1 ... :kn vn))

It's sort of like a function signature of:
  '&rest args &keys k0 k1 ... kn'

Example:
  (defun example (&rest args)
    ...
    (-let*/-let (((arg-list kw-plist)
                  (elisp:parse:args+kwargs args
                                           :jeff :zort :vogon)))
    ...

Return a cons of lists: '(args-list . kwargs-plist)"
  (let ((leading-args nil)
        (rest-as-keywords nil)
        (keywords nil)
        ;; Flatten so callers don't have to use `apply' to get CLAIMS in correct format.
        (claims (elisp:list:flatten claims)))
    ;; Sort args into args/kwargs.
    (dolist (arg input)
      ;; Once we hit the first keyword arg, the rest are always all keywords.
      (if (not (or rest-as-keywords
                   (memq arg claims)))
        ;; Still in the args.
        (push arg leading-args)

        ;; Rest are keywords.
        (setq rest-as-keywords t)
        (push arg keywords)))

    ;; Done processing list, but our lists to return are backwords right now.
    (cons (nreverse leading-args) (nreverse keywords))))
;; (elisp:parse:args+kwargs '(jeff jefferson :namespace :work) :namespace)
;; (elisp:parse:args+kwargs '(jeff jefferson :namespace nil :value 42) :namespace :value)
;; (elisp:parse:args+kwargs '((jeff (jefferson)) :namespace nil :value 42) :namespace :value)


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :elisp 'utils 'functions)

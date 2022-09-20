;;; mis-style.el --- String styling for Mis -*- lexical-binding: t; -*-
;;
;; Author: Cole Brown <code@brown.dev>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:  2019-10-23
;; Modified: 2022-08-09
;;
;;; Commentary:
;;
;; String styling for Mis.
;;
;; Set up styling for anything internal to the `mis:style` call.
;; E.g.:
;;   (mis "Hello, "
;;        (mis:style :bold "world")
;;        ".")
;;   -> "world" styled as bold.
;;
;;   (mis (mis:style '(:align 'center :width 80)
;;                   "Hello, "
;;                   (mis:style :bold "world")
;;                   "."))
;;   -> "world" styled as bold.
;;   -> "Hello world." aligned to center (of the 80 width).
;;
;;; Code:


(require 'mis-error)
(require 'mis-valid)
(require 'mis-parse)


;;------------------------------------------------------------------------------
;; Styling Registration
;;------------------------------------------------------------------------------

(defvar int<mis>:stylers
  nil
  "Alist of Mis keyword to styler/styling function.

Keyword must be a member of `int<mis>:keywords:style'.

Styler FUNCTION must have params:
     (CALLER STRING SYNTAX STYLE &optional KEY VALUE)
Or if function doesn't care about key/value:
     (CALLER STRING STYLE &rest _)
Or ignore even more if function doesn't care about them. e.g.:
     (CALLER STRING &rest _)
  - STRING will be the string to be styled.
  - SYNTAX should be a Mis Syntax Tree.
  - STYLE will be the current Mis Style Syntax Tree.
  - KEY will be the styling keyword encountered.
    - Allows one func to handle multiple keywords, like `:trim', `:trim:left'...
  - VALUE will be whatever the KEY's value is.")


(defun int<mis>:styler:register (style function)
  "Register FUNCTION as the styler for STYLE.

STYLE must be a keyword and a member of `int<mis>:keywords:style'.

Styler FUNCTION must have params: (CALLER STRING STYLE &optional KEY VALUE)
Or if function doesn't care about key/value: (CALLER STRING STYLE &rest _)
Or if function doesn't care about anything: (CALLER STRING &rest _)
  - STRING will be the string to be styled.
  - STYLE will be the current Mis Style Syntax Tree.
  - KEY will be the styling keyword encountered.
    - Allows one func to handle multiple keywords, like `:trim', `:trim:left'...
  - VALUE will be whatever the KEY's value is."
  ;; Just overwrite re-registrations.
  (setf (alist-get style int<mis>:stylers) function))


(defun int<mis>:styler:get (style)
  "Get registered styler function for STYLE.

STYLE must be a keyword and a member of `int<mis>:keywords:style'.

Styler FUNCTION must have params: (CALLER STRING STYLE &optional KEY VALUE)
Or if function doesn't care about key/value: (CALLER STRING STYLE &rest _)
Or if function doesn't care about anything: (CALLER STRING &rest _)
  - STRING will be the string to be styled.
  - STYLE will be the current Mis Style Syntax Tree.
  - KEY will be the styling keyword encountered.
    - Allows one func to handle multiple keywords, like `:trim', `:trim:left'...
  - VALUE will be whatever the KEY's value is."
  (or (alist-get style int<mis>:stylers)
      (int<mis>:error 'int<mis>:style:get
                      "No styler found for `%S'!"
                      style)))
;; (int<mis>:styler:get :align)


;;------------------------------------------------------------------------------
;; Styling
;;------------------------------------------------------------------------------

(defun int<mis>:style:styler/no-op (caller string &rest _)
  "A do-nothing-and-return-STRING-as-is styler.

STRING must be the string to not be styled and just return as-is.

Doesn't register as a styler directly; others register to use this if they have
no direct styling.
Example: `:width' is a directive for other stylers to use.

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)"
  string)


;; Register our users of the no-op styler:
(int<mis>:styler:register :width   #'int<mis>:style:styler/no-op)
(int<mis>:styler:register :padding #'int<mis>:style:styler/no-op)


(defun int<mis>:style/output-entry (caller entry syntax style/complete)
  "`int<mis>:style' helper: Style a single ENTRY from a Mis Output Tree.
For example, assuming Mis Output Tree is:
  '((:output ((:string . \"foo\")  (:metadata (:bar . baz)))
             ((:string . \"zort\") (:metadata (:poit . narf))))
OUTPUT should be:
  1. '((:string . \"foo\")  (:metadata (:bar . baz)))
or:
  2. '((:string . \"zort\") (:metadata (:poit . narf)))

SYNTAX should be nil or a Mis Syntax Tree. Its styling is ignored; only
STYLE/COMPLETE is used.

STYLE/COMPLETE should be nil or a `:style' Mis Syntax Tree.
Example: (mis:style :width 80) -> '((:style (:width . 80)))

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)"
  (let* ((caller        (list 'int<mis>:style/string caller))
         (string/styled (int<mis>:output:get/string caller entry)) ; Initial assumption: it's already styled or nothing more to do.
         (metadata      (int<mis>:output:get/metadata caller entry)))
    (int<mis>:debug caller
                    "entry:         %S"
                    entry)
    (int<mis>:debug caller
                    "string:         %S"
                    string/styled)
    (int<mis>:debug caller
                    "metadata:       %S"
                    metadata)
    (int<mis>:debug caller
                    "style/complete: %S"
                    style/complete)

    ;; No error checks; expect parent to provide valid params.

    ;;------------------------------
    ;; Style!
    ;;------------------------------
    ;; Check each styling keyword in `style/complete' to see if it wants to
    ;; mutate the output string any.
    (dolist (kvp (int<mis>:syntax:get/value caller :style style/complete))
      (int<mis>:debug caller
                      "styling kvp:    %S"
                      kvp)
      (let* ((key    (car kvp))
             (value  (cdr kvp))
             (styler (int<mis>:styler:get key)))
        (int<mis>:debug caller
                        "styling key:    %S"
                        key)
        (int<mis>:debug caller
                        "styling value:  %S"
                        value)
        (int<mis>:debug caller
                        "styler func:    %S"
                        styler)

        (unless (functionp styler)
          (int<mis>:error caller
                          '("No valid styler found! "
                            "keyword: %S, "
                            "value: %S, "
                            "styler: %S")
                          key
                          value
                          styler))
        ;; `key' & `value' should have already been validated during parsing, so
        ;; just use 'em as-is.
        (setq string/styled (funcall styler
                                     caller
                                     string/styled
                                     syntax
                                     style/complete
                                     key
                                     value))

        (int<mis>:debug caller
                        "<--string:      %S"
                        string/styled)))

    ;; Done; return a new Mis Output Tree (with pre-existing metadata).
    (int<mis>:output caller
                     string/styled
                     style/complete
                     metadata)))
;; (int<mis>:style/output-entry 'test
;;                              '((:string . "hello there") (:metadata (:foo . "bar")))
;;                              nil
;;                              '((:style (:width . 20) (:align . center))))


(defun int<mis>:style (caller output category syntax style/parent)
  "Style the Mis OUTPUT Tree with styling from SYNTAX & STYLE/PARENT.

OUTPUT should be a Mis OUTPUT Tree to be styled.

CATEGORY should be nil or a keyword from `int<mis>:keywords:category/internal'.
It is the category in SYNTAX that we will look under for styling.

SYNTAX should be nil or a Mis Syntax Tree. It can contain styling of its own,
which will override any styling in STYLE/PARENT.

STYLE/PARENT should be nil or a `:style' Mis Syntax Tree.
Example: (mis:style :width 80) -> '((:style (:width . 80)))

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)"
  (let ((caller (list 'int<mis>:style caller)))
    (int<mis>:debug caller
                    "output:         %S"
                    output)
    (int<mis>:debug caller
                    "category:       %S"
                    category)
    (int<mis>:debug caller
                    "syntax:         %S"
                    syntax)
    (int<mis>:debug caller
                    "style/parent:   %S"
                    style/parent)

    ;;------------------------------
    ;; Error Checks
    ;;------------------------------
    (cond ((not (int<mis>:style:exclusive? style/parent))
           (int<mis>:error caller
                           "STYLE/PARENT must be nil or exclusively styling. Got: %S"
                           style/parent))

          ((not (int<mis>:valid:output? caller 'output output))
           (int<mis>:error caller
                           "OUTPUT must be a Mis OUTPUT Tree. Got: %S"
                           output))

          (t
           nil))

    ;;------------------------------
    ;; Style!
    ;;------------------------------
    (let ((style/complete (int<mis>:syntax:merge/style caller
                                                       category
                                                       syntax
                                                       style/parent))
          outputs/styled)

      (int<mis>:debug caller
                      "style/complete: %S"
                      style/complete)

      ;; Style each entry in OUTPUT.
      (dolist (entry (int<mis>:output:get/outputs caller output))
        (let ((outputs/new (int<mis>:output:get/outputs
                            caller
                            (int<mis>:style/output-entry caller
                                                         entry
                                                         syntax
                                                         style/complete))))
          (if (> (length outputs/new) 1)
              (int<mis>:error caller
                              '("Expecting only one styled result from only one entry. "
                                "entry: %S, outputs/new: %S")
                              entry
                              outputs/new)
            (push (car outputs/new) outputs/styled))))

      (int<mis>:debug caller
                      "<--outputs:     %S"
                      outputs/styled)

      ;; Done; return a Mis Output Tree from our styled outputs.
      (int<mis>:output:from-entries caller
                                    (nreverse outputs/styled)))))
;; (int<mis>:style 'test '((:output ((:string . "hi") (:metadata)))) :format '((:format (:style (:width . 10) (:align . center)))) nil)


;;------------------------------------------------------------------------------
;; Style Helpers
;;------------------------------------------------------------------------------

(defun int<mis>:style:get-or-dne (caller keyword syntax)
  "Get KEYWORD's value from styling in SYNTAX.

KEYWORD should be a keyword in `int<mis>:keywords:style'.

If KEYWORD does exist, returns its value (even nil).
If KEYWORD does not exist, returns `:does-not-exist'.

STYLE should be nil or a Mis Syntax Tree of only `:style'.

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)"
  (let* ((caller (list 'int<mis>:style:get-or-dne caller))
         (style (int<mis>:syntax:get/value caller
                                           :style
                                           syntax)))
    ;; Get from SYNTAX first, then fallback to STYLE/PARENT.
    (if (int<mis>:syntax:has caller
                             style
                             keyword)
        (int<mis>:syntax:get/value caller
                                   keyword
                                   style)
      :does-not-exist)))
;; (int<mis>:style:get-or-dne 'test :width (mis:style :width 42))
;; (int<mis>:style:get-or-dne 'test :width (mis:style :padding "-"))


(defun int<mis>:style:get (caller keyword syntax style/parent)
  "Get KEYWORD's value from styling in SYNTAX (preferred) or STYLE/PARENT.

KEYWORD should be a keyword in `int<mis>:keywords:style'.

SYNTAX should be nil or a Mis Syntax Tree.

STYLE/PARENT should be nil or a Mis Syntax Tree of only `:style' from SYNTAX's
parent/ancestors.

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)"
  (let* ((caller (list 'int<mis>:style:get caller))
         (value  (int<mis>:style:get-or-dne caller
                                            keyword
                                            syntax)))
    (if (eq value :does-not-exist)
        ;; Wasn't in SYNTAX; return from STYLE/PARENT (value or nil).
        (int<mis>:syntax:find caller
                              style/parent
                              :style
                              keyword)
      value)))
;; (int<mis>:style:get 'test :width (mis:style :width 42) (mis:style :padding "-"))
;; (int<mis>:style:get 'test :width (mis:style :padding "-") (mis:style :width 42))
;; (int<mis>:style:get 'test :width (mis:style :width 42 :padding "-") (mis:style :width 11))


(defun int<mis>:style:exclusive? (syntax)
  "Return non-nil if SYNTAX is _only_ styling; return nil otherwise."
  ;; No styling is valid styling.
  (cond ((null syntax)
         t)

        ;; Must have valid syntax.
        ((not (int<mis>:valid:syntax? 'int<mis>:style:exclusive?
                                      'syntax
                                      syntax
                                      :no-error))
         (int<mis>:debug 'int<mis>:style:exclusive?
                         "Invalid mis SYNTAX! %S"
                         syntax)
         nil)

        ;; (Alist) syntax should contain only 1 element.
        ((not (eq 1 (length syntax)))
         (int<mis>:debug 'int<mis>:style:exclusive?
                         "Style SYNTAX should be length 1, got %d: %S"
                         (length syntax)
                         syntax)
         nil)

        ;; Key should be `:style'.
        ((int<mis>:syntax:has 'int<mis>:style:exclusive?
                               syntax
                               :style)
         (int<mis>:debug 'int<mis>:style:exclusive?
                         "Ok; style SYNTAX seems valid: %S"
                         syntax)
         t)

        ;; Fallthrough: not styling so return nil.
        (t
         (int<mis>:debug 'int<mis>:style:exclusive?
                         "Style SYNTAX doesn't seem to be a Mis Styling Syntax Tree? %S"
                         syntax)
         nil)))
;; (int<mis>:style:exclusive? nil)
;; (int<mis>:style:exclusive? (mis:style :width 80))
;; (int<mis>:style:exclusive? (mis:style))
;; (int<mis>:style:exclusive? '((:style (:align . center)) (:string . "hello")))


(defun int<mis>:style:width (caller style &optional default)
  "Return `:width' from STYLE, or default.

STYLE should be a Mis Syntax Tree of styling.
  - Optional:
    - `:width'
      - Must be a positive integer.
      - If it is not provided, will try to use DEFAULT.
        - If DEFAULT is not provided, will use buffer's `fill-column'.

Must be called in the context of the targeted output buffer so that
`fill-column' can be correct.

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)"
  ;;------------------------------
  ;; Get Width
  ;;------------------------------
  (let ((width (or (int<mis>:syntax:find caller
                                         style
                                         :style :width)
                   ;; Fallback to default if it's an integer > 0.
                   (and (integerp default)
                        (> default 0)
                        default)
                   ;; Fallback to buffer's `fill-column'.
                   fill-column)))
    ;;------------------------------
    ;; Error Check
    ;;------------------------------
    (when (or (not (integerp width))
              (< width 1))
      (int<mis>:error caller
                      "WIDTH must be a positive integer. Got a %S: %S"
                      (type-of width)
                      width))
    ;;------------------------------
    ;; Return
    ;;------------------------------
    width))


(defun int<mis>:style:padding (caller style &optional default)
  "Return `:padding' from STYLE, or DEFAULT.

STYLE should be a Mis Syntax Tree of styling.
  - Optional:
    - `:padding'
      - Must be a character or a string of length 1.
      - If not supplied, it will default to DEFAULT.
        - If not supplied, it will default to a space (\" \").

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)

Return padding as a string."
  ;;------------------------------
  ;; Get Padding
  ;;------------------------------
  (let ((padding (int<mis>:syntax:find caller
                                       style
                                       :style :padding)))

    ;;------------------------------
    ;; Error Checks & Fallbacks
    ;;------------------------------
    (cond ((and (not (null padding))
                (not (stringp padding))
                (not (characterp padding)))
           (int<mis>:error 'int<mis>:align
                           "PADDING must be nil, a character, or a string of length 1. Got a %S: %S"
                           (type-of padding)
                           padding))

          ((and (stringp padding)
                (not (= (length padding) 1)))
           (int<mis>:error 'int<mis>:align
                           '("PADDING must be nil, a character, or a string of length 1. "
                             "Got a string of length %S: %S")
                           (length padding)
                           padding))

          ;;---
          ;; Initial Fallback: DEFAULT
          ;;---
          ((and (null padding)
                (not (null default)))
           ;; Fallback to DEFAULT, and do the same error checks on it.
           (setq padding default)
           (cond ((and (not (null padding))
                       (not (stringp padding))
                       (not (characterp padding)))
                  (int<mis>:error 'int<mis>:align
                                  "PADDING must be nil, a character, or a string of length 1. Got a %S: %S"
                                  (type-of padding)
                                  padding))

                 ((and (stringp padding)
                       (not (= (length padding) 1)))
                  (int<mis>:error 'int<mis>:align
                                  '("PADDING must be nil, a character, or a string of length 1. "
                                    "Got a string of length %S: %S")
                                  (length padding)
                                  padding))

                 ;;---
                 ;; Valid `default'.
                 ;;---
                 (t
                  nil)))

          ;;---
          ;; Final Fallback: A Space.
          ;;---
          ((and (null padding)
                (null default))
           (setq padding " "))

          ;;---
          ;; Valid `padding'.
          ;;---
          (t
           nil))

    ;;------------------------------
    ;; Normalize & Return
    ;;------------------------------
    ;; `padding' is set to something valid: char or string.
    ;; Let's make that a string or a string.
    (if (characterp padding)
        (make-string 1 padding)
      padding)))
;; (int<mis>:style:padding 'test (mis:style :padding "?"))
;; (int<mis>:style:padding 'test nil)
;; (int<mis>:style:padding 'test nil "!")
;; (int<mis>:style:padding 'test nil ?!)
;; (int<mis>:style:padding 'test (mis:style :padding "?") "!")
;; (int<mis>:style:padding 'test (mis:style :width 10) "!")


;;------------------------------------------------------------------------------
;; API: Styling
;;------------------------------------------------------------------------------

(defun mis:style (&rest args)
  "Validate ARGS and return a Mis style list.

ARGS should start off with styling key/values before supplying
the format string and format args. Example:
  Valid styles:
    (mis:style :bold t \"hello world\")
    (mis:style :bold t :align 'center \"hello %s\" (get-greeted))
  Invalid styles:
    (mis:style \"hello %s\" :bold :align 'center (get-greeted))
    (mis:style \"hello %s\" (get-greeted) :bold :align 'center)

NOTE: The \"invalid styles\" will just be interpreted as having no styling and
extra message args.

NOTE: Styles must always have both a keyword and a value."
  (apply 'int<mis>:parse
         'mis:style
         :style
         :style ; Only allow style keywords.
         args))
;; (mis:style :width 80)
;; (mis:style :align 'center "hello")
;; (mis:style :align 'center "hello %s" "world")
;; (mis:style :align 'center :width 11 "hello %s" "world")
;; (mis:style :align 'center :width 11 "hello")
;; (mis:style :indent 'auto "hello")
;; (mis:style :padding "?")


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'mis-style)
;;; mis-style.el ends here

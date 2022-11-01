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
(require 'mis-tree-syntax)
(require 'mis-tree-output)
(require 'mis-parse)


;;------------------------------------------------------------------------------
;; Styling Registration
;;------------------------------------------------------------------------------

(defvar int<mis>:stylers
  nil
  "Alist of Mis keyword to styler/styling function.

Keyword must be a member of `int<mis>:keywords:style'.

Styler FUNCTION must (at least) have params:
     (CALLER STRING &rest _)
Stylers will be called with params:
     (CALLER STRING SYNTAX STYLE KEY VALUE)
  - STRING will be the string to be styled.
  - SYNTAX should be a Mis Syntax Tree.
  - STYLE will be the current Mis Style Syntax Tree.
  - KEY will be the styling keyword encountered.
    - So one func can handle multiple keywords, like `:trim', `:trim:left'...
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
      (int<mis>:error 'int<mis>:styler:get
                      "No styler found for `%S'!"
                      style)))
;; (int<mis>:styler:get :align)


;;------------------------------------------------------------------------------
;; Stylers
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


(defun int<mis>:style:newlines (caller string _ _ _ newline?)
  "Ensure STRING ends in a newline if NEWLINE? is truthy.

STRING must be a string.

NEWLINE? must be nil/non-nil.

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)"
  (if (and newline?
           (stringp string)
           (not (string-suffix-p "\n" string)))
      (concat string "\n")
    string))
;; (int<mis>:style:newlines 'test "hello" nil nil nil t)
;; (int<mis>:style:newlines 'test "hello" nil nil nil nil)
;; (int<mis>:style:newlines 'test "hello\n" nil nil nil t)
;; (int<mis>:style:newlines 'test "hello\n" nil nil nil nil)


(int<mis>:styler:register :newlines #'int<mis>:style:newlines)


(defun int<mis>:style:propertize (caller string _ _ _ properties)
  "Propertize STRING with PROPERTIES.

STRING should be the string to be styled.

Ignored are:
  - SYNTAX - a Mis Syntax Tree
  - STYLE  - a Mis Style Syntax Tree
  - KEY    - the styling keyword encountered

PROPERTIES should be something that function `propertize' understands.

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)"
  ;; Dunno any error checks we can do. Just propertize and return.
  (apply #'propertize
         string
         properties))
;; (int<mis>:style:propertize 'test "hello" nil nil :propertize '(face error))


(int<mis>:styler:register :propertize #'int<mis>:style:propertize)


(defun int<mis>:style:face (caller string syntax style key face)
  "Style STRING with FACE.

STRING should be the string to be styled.

Ignored are:
  - SYNTAX - a Mis Syntax Tree
  - STYLE  - a Mis Style Syntax Tree
  - KEY    - the styling keyword encountered

FACE should be a /defined/ face symbol name. That is, it should exist in return
value of function `face-list'.
  - To see all face names and sample text, call function `list-faces-display'.

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)"
  (let ((caller (list 'int<mis>:style:face caller)))
    ;;------------------------------
    ;; Error Checks
    ;;------------------------------
    (cond ((not (symbolp face))
           (int<mis>:error caller
                           "FACE must be a symbol! Got %S: `%S'"
                           (type-of face)
                           face))
          ((not (memq face (face-list)))
           (int<mis>:error caller
                           "FACE `%S' is not a currently defined face! See `list-faces-display' or `face-list'."
                           face)))

    ;;------------------------------
    ;; Style!
    ;;------------------------------
    (int<mis>:style:propertize caller
                               string
                               syntax
                               style
                               key
                               (list 'face face))))
;; (int<mis>:style:face 'test "hello" nil nil :face 'error)


(int<mis>:styler:register :face #'int<mis>:style:face)


(defun int<mis>:style:weight (caller string syntax style key value)
  "Set STRING's weight to a weight defined by KEY and VALUE.

STRING should be the string to be styled.

Ignored are:
  - SYNTAX - a Mis Syntax Tree
  - STYLE  - a Mis Style Syntax Tree

KEY should a keyword:
  - `:weight'
  - `:bold'

VALUE depends on KEY:
  - If KEY is `:weight', VALUE must be one of these symbols (from densest to
    faintest):
    - `ultra-bold'
    - `extra-bold'
    - `bold'
    - `semi-bold'
    - `normal'
    - `semi-light'
    - `light'
    - `extra-light'
    - `ultra-light'
  - If KEY is `:bold', VALUE must be:
    - one of the `:weight' symbols
    - t (for `bold')
    - nil (for `normal')

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)"
  (let* ((caller (list 'int<mis>:style:weight caller))
         ;; TODO: Check `font-weight-table' for valid weight name?
         (weights/valid '(ultra-bold extra-bold bold semi-bold normal semi-light light extra-light ultra-light))
         properties)
    ;;------------------------------
    ;; Error Checks & Normalization
    ;;------------------------------
    ;; We're accepting `:weight' and `:bold'. We need to normalize to a face property list of '(:weight <weight-keyword>)'.
    ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Face-Attributes.html
    (cond ((eq key :weight)
           (if (memq value weights/valid)
               (setq properties (list :weight value))
             (int<mis>:error caller
                             "VALUE must be a valid weight! Valid: %S, Got: %S"
                             weights/valid
                             face)))

          ((eq key :bold)
           (cond ((memq value weights/valid)
                  (setq properties (list :weight value)))
                 ((eq value t)
                  (setq properties '(:weight bold)))
                 ((eq value nil)
                  (setq properties '(:weight normal)))
                 (t
                  (int<mis>:error caller
                                  "VALUE must be a valid weight! Valid: %S, Got: %S"
                                  (append weights/valid '(t nil))
                                  face))))

          (t
           (int<mis>:error caller
                           "KEY must be `:weight' or `:bold'! Got %S: `%S'"
                           (type-of key)
                           key)))

    ;;------------------------------
    ;; Style!
    ;;------------------------------
    (int<mis>:style:propertize caller
                               string
                               syntax
                               style
                               key
                               (list 'face properties))))
;; (int<mis>:style:weight 'test "hello" nil nil :weight 'bold)
;; (int<mis>:style:weight 'test "hello" nil nil :bold t)


(int<mis>:styler:register :weight #'int<mis>:style:weight)
(int<mis>:styler:register :bold   #'int<mis>:style:weight)


(defun int<mis>:style:slant (caller string syntax style key value)
  "Set STRING's slant to a slant defined by KEY and VALUE.

STRING should be the string to be styled.

Ignored are:
  - SYNTAX - a Mis Syntax Tree
  - STYLE  - a Mis Style Syntax Tree

KEY should a keyword:
  - `:slant'
  - `:italic'

VALUE depends on KEY:
  - If KEY is `:slant', VALUE must be one of these symbols (from densest to
    faintest):
    - `italic'
    - `oblique'
    - `normal'
    - `reverse-italic'
    - `reverse-oblique'
  - If KEY is `:italic', VALUE must be:
    - one of the `:slant' symbols
    - t (for `italic')
    - nil (for `normal')

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)"
  (let* ((caller (list 'int<mis>:style:slant caller))
         ;; TODO: Check `font-slant-table' for valid slant name?
         (slants/valid '(italic oblique normal reverse-italic reverse-oblique))
         properties)
    ;;------------------------------
    ;; Error Checks & Normalization
    ;;------------------------------
    ;; We're accepting `:slant' and `:italic'. We need to normalize to a face property list of '(:slant <slant-keyword>)'.
    ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Face-Attributes.html
    (cond ((eq key :slant)
           (if (memq value slants/valid)
               (setq properties (list :slant value))
             (int<mis>:error caller
                             "VALUE must be a valid slant! Valid: %S, Got: %S"
                             slants/valid
                             face)))

          ((eq key :italic)
           (cond ((memq value slants/valid)
                  (setq properties (list :slant value)))
                 ((eq value t)
                  (setq properties '(:slant italic)))
                 ((eq value nil)
                  (setq properties '(:slant normal)))
                 (t
                  (int<mis>:error caller
                                  "VALUE must be a valid slant! Valid: %S, Got: %S"
                                  (append slants/valid '(t nil))
                                  face))))

          (t
           (int<mis>:error caller
                           "KEY must be `:slant' or `:italic'! Got %S: `%S'"
                           (type-of key)
                           key)))

    ;;------------------------------
    ;; Style!
    ;;------------------------------
    (int<mis>:style:propertize caller
                               string
                               syntax
                               style
                               key
                               (list 'face properties))))
;; (int<mis>:style:slant 'test "hello" nil nil :slant 'italic)
;; (int<mis>:style:slant 'test "hello" nil nil :italic t)


(int<mis>:styler:register :slant  #'int<mis>:style:slant)
(int<mis>:styler:register :italic #'int<mis>:style:slant)


(defun int<mis>:style:color (caller string syntax style key value)
  "Set STRING's color to a color defined by KEY and VALUE.

STRING should be the string to be styled.

Ignored are:
  - SYNTAX - a Mis Syntax Tree
  - STYLE  - a Mis Style Syntax Tree

KEY should a keyword:
  - `:foreground'
  - `:background'

VALUE should be a string, symbol, or keyword. If it is a symbol or keyword, it
is converted to a string and stripped of any leading colon.
  - a system-defined color name
  - a hexadecimal color specification
    - '#rgb' and 'RGB:r/g/b', where 'r', 'g', and 'b' are 1-4 hex digits long
  - see Info node `Color Names' for full info
  - basically things that `color-defined-p' returns t for

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)"
  (let* ((caller (list 'int<mis>:style:foreground caller))
         (keys/valid '(:foreground :background))
         ;; `unspecified' is a special case; otherwise we need a string.
         (value (if (eq value 'unspecified)
                    'unspecified
                  (int<mis>:valid:normalize->string caller 'value value)))
         properties)
    ;;------------------------------
    ;; Error Checks
    ;;------------------------------
    (unless (memq key keys/valid)
      (int<mis>:error caller
                      "KEY must be one of keywords: %S Got: %S"
                      keys/valid
                      key))
    (unless (color-defined-p value)
      (int<mis>:error caller
                      "VALUE must be a recognized/defined color! Not a color: %S"
                      value))

    ;;------------------------------
    ;; Style!
    ;;------------------------------
    (int<mis>:style:propertize caller
                               string
                               syntax
                               style
                               key
                               (list 'face (list key value)))))
;; (int<mis>:style:color 'test "hello" nil nil :foreground 'red)
;; (int<mis>:style:color 'test "hello" nil nil :background 'red)
;; (int<mis>:style:color 'test "hello" nil nil :middleground 'red)


(int<mis>:styler:register :foreground  #'int<mis>:style:color)
(int<mis>:styler:register :background  #'int<mis>:style:color)


;;------------------------------------------------------------------------------
;; Styling
;;------------------------------------------------------------------------------

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
  (let* ((caller        (list 'int<mis>:style/output-entry caller))
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
    (cond ((not (int<mis>:valid:style/exclusively? caller style/parent))
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
          outputs/styled
          output/return)

      (int<mis>:debug caller
                      "style/complete: %S"
                      style/complete)

      ;; Style each entry in OUTPUT.
      (dolist (entry (int<mis>:output:get/entries caller output))
        (let ((outputs/new (int<mis>:output:get/entries
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
                      "outputs:        %S"
                      outputs/styled)

      ;; Done; return a Mis Output Tree from our styled outputs.
      (setq output/return (int<mis>:output:from-entries caller (nreverse outputs/styled)))
      (int<mis>:debug caller
                      "<--output:      %S"
                      output/return)
      output/return)))
;; (int<mis>:style 'test '((:output ((:string . "hi") (:metadata)))) :format '((:format (:style (:width . 10) (:align . center)))) nil)


;;------------------------------------------------------------------------------
;; Style Helpers
;;------------------------------------------------------------------------------

;; TODO: syntax and/or output tree? Some styles can be in the output trees now (e.g. `:width').
(defun int<mis>:style:get-or-dne (caller keyword syntax)
  "Get KEYWORD's value from styling in SYNTAX.

KEYWORD should be a keyword in `int<mis>:keywords:style'.

If KEYWORD does exist, returns its value (even nil).
If KEYWORD does not exist, returns `:does-not-exist'.

SYNTAX should be nil or a Mis Syntax Tree of only `:style'.

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



;; TODO: syntax and/or output tree? Some styles can be in the output trees now (e.g. `:width').
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


(defun int<mis>:style:get/width (caller syntax output default)
  "Return `:width' from SYNTAX, OUTPUT, or DEFAULT.

SYNTAX should be nil or a Mis Syntax Tree of styling.
  - Optional:
    - `:width'
      - Must be a positive integer.

OUTPUT should be nil a Mis Output Tree.
  - Optional:
    - `:width'
      - Must be a positive integer.

DEFAULT should be nil or a positive integer. If DEFAULT is not provided, will
use buffer's `fill-column'.

Prefers SYNTAX, then OUTPUT, then DEFAULT/`fill-column'.

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
  (let* ((caller (list 'int<mis>:style:get/width caller))
         ;; Prefer: SYNTAX first...
         (width (or (int<mis>:syntax:find caller
                                          syntax
                                          :style :width)
                    ;; OUTPUT second...
                    (int<mis>:output/metadata:find caller
                                                   :width
                                                   output)

                    ;; DEFAULT third...
                    (and (integerp default)
                         (> default 0)
                         default)
                    ;; Or, finally, current buffer's `fill-column'.
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


(defun int<mis>:style:get/padding (caller syntax output default)
  "Return `:padding' from SYNTAX, or DEFAULT.

SYNTAX should be nil or a Mis Syntax Tree of styling.
  - Optional:
    - `:padding'
      - Must be a character or a string of length 1.

OUTPUT should be nil a Mis Output Tree.
  - Optional:
    - `:padding'
      - Must be a character or a string of length 1.

DEFAULT should be nil or a character or a string of length 1. If DEFAULT is not
provided, it will default to a space (\" \").

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)

Return padding as a string."
  ;;------------------------------
  ;; Error Checks
  ;;------------------------------
  (let* ((caller (list 'int<mis>:style:get/padding caller))
         (syntax/padding (int<mis>:valid:string1-char-nil?
                          caller
                          "syntax padding"
                          (int<mis>:syntax:find caller
                                                syntax
                                                :style :padding)
                          :invalid))
         (output/padding (int<mis>:valid:string1-char-nil?
                          caller
                          "output padding"
                          (int<mis>:output/metadata:find caller
                                                         :padding
                                                         output)
                          :invalid))
         (default (int<mis>:valid:string1-char-nil?
                   caller
                   'default
                   default
                   :invalid))
         (invalids '(:invalid nil))
         padding)

    ;;------------------------------
    ;; Determine padding from inputs/fallback.
    ;;------------------------------
    (setq padding (cond ((not (memq syntax/padding invalids))
                         syntax/padding)
                        ((not (memq output/padding invalids))
                         output/padding)
                        ((not (memq default invalids))
                         default)
                        (t
                         " ")))

    ;;------------------------------
    ;; Normalize & Return
    ;;------------------------------
    ;; `padding' is set to nil or something valid: char or string.
    ;; Let's make that a string or a string.
    (if (characterp padding)
        (make-string 1 padding)
      padding)))
;; (int<mis>:style:get/padding 'test (mis:style :padding "?") nil nil)
;; (int<mis>:style:get/padding 'test nil nil nil)
;; (int<mis>:style:get/padding 'test nil nil "!")
;; (int<mis>:style:get/padding 'test nil nil ?!)
;; (int<mis>:style:get/padding 'test (mis:style :padding "?") nil "!")
;; (int<mis>:style:get/padding 'test (mis:style :width 10) nil "!")


(defun int<mis>:style:get/newlines (caller &rest input)
  "Return `:newlines' from INPUT.

INPUT should each be one of:
  - a Mis Syntax Tree of styling (will look for `:newlines').
  - nil/non-nil default value

Checks INPUT in order presented, return first `:newlines' value found or default
or nil.

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)"
  (let ((caller (list 'int<mis>:style:newlines caller))
        found?
        newlines?)
    (while (and input
                (not found?))
      (let ((check (pop input)))
        ;;------------------------------
        ;; Mis Syntax Tree?
        ;;------------------------------
        (cond ((int<mis>:valid:syntax? caller
                                       'input
                                       check
                                       :no-error)
               (int<mis>:debug caller
                               "Found MST: %S"
                               check)
               ;; Have to check if it has it first, then get its value since we
               ;; need to check for a valid nil value.
               (if (int<mis>:syntax:has caller check :style :newlines)
                   (progn
                     (setq found?    t
                           newlines? (int<mis>:syntax:find caller
                                                           check
                                                           :style :newlines))
                     (int<mis>:debug caller
                                     "MST `:newlines': %S"
                                     newlines?))
                 (int<mis>:debug caller
                                 "No `:newlines' in MST.")))

              ;;------------------------------
              ;; ...default value I guess?
              ;;------------------------------
              (t
               (int<mis>:debug caller
                               "Found default value: %S"
                               check)
               (setq found?    t
                     newlines? check)))))

    ;; So, what'd we find out?
    (int<mis>:debug caller
                    "<--newlines?: %S"
                    newlines?)
    newlines?))
;; (int<mis>:style:newlines 'test (mis:style :newlines t))
;; (int<mis>:style:newlines 'test (mis:style :newlines 'yes))
;; (int<mis>:style:newlines 'test '((:style (:newlines . yes))))
;; (int<mis>:style:newlines 'test)


;;------------------------------------------------------------------------------
;; Compiler
;;------------------------------------------------------------------------------

(defun int<mis>:compile:style (caller syntax style/ancestors)
  "Compile Mis SYNTAX Tree using STYLE/ANCESTORS; return a Mis Output Tree.

SYNTAX should be a `:style' syntax tree. It can contain styling of its own,
which will override any styling in STYLE/ANCESTORS.
Example:
  '((:style (:align . center)
            (:children (:format (:formatter . string)
                                (:value . \"-=-\")))))

STYLE/ANCESTORS should be nil or a Mis Syntax Tree of only `:style'.
Example: (mis:style :width 80) -> '((:style (:width . 80)))

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)"
  (let* ((caller     (list 'int<mis>:compile:style caller))
         (style/this (int<mis>:syntax:filter/style caller syntax))
         (children   (int<mis>:syntax:find caller syntax :style :children))
         value
         value/metadata)
    (int<mis>:debug caller "syntax:          %S" syntax)
    (int<mis>:debug caller "style/ancestors: %S" style/ancestors)
    (int<mis>:debug caller "style/this:      %S" style/this)
    (int<mis>:debug caller "children:        %S" children)

    ;;------------------------------
    ;; Sanity Checks?
    ;;------------------------------
    ;; We should have some styling, probably, at least?
    (unless style/this
      (int<mis>:error caller
                      '("SYNTAX is expected to be a `:style' Mis Output Tree "
                        "with some styling. Found %S style kvps in: %S")
                      style/this
                      syntax))

    ;;------------------------------
    ;; No-op?
    ;;------------------------------
    (if (null children)
        ;; Sometimes a solo style tree is valid, I think?
        ;; So... not sure?
        ;; (progn
        ;;   (int<mis>:debug caller
        ;;                   "No children; returning filtered style: %S"
        ;;                   style/this)
        ;;   style/this)
        ;;
        ;; Actually... Let's start of erroring and see if it actually is/should
        ;; be valid...
        (int<mis>:error caller
                        '("Don't know what to compile for a `:style' Mis Output Tree "
                          "without children: %S")
                        syntax)

      ;;------------------------------
      ;; Compile children.
      ;;------------------------------
      ;; `int<mis>:compile:children' will handle merging our style with ancestors'.
      (int<mis>:debug caller
                      "Compile `:style' MST's children: %S"
                      syntax)
      (setq value (int<mis>:compile:children caller :style syntax style/ancestors))
      (int<mis>:debug caller "<--value:  %S" value)

      ;; Just return what `int<mis>:compile:children' gave us. It's a MOT and the string(s) is(/are) styled.
      value)))
;; (int<mis>:compile:style 'test (mis:line "-") (mis:style :width 80))


(int<mis>:compiler:register :style #'int<mis>:compile:style)


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
;; (mis:style :newlines t)


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'mis-style)
;;; mis-style.el ends here

;;; mis-art.el --- Lines and Pretty Things -*- lexical-binding: t; -*-
;;
;; Author: Cole Brown <code@brown.dev>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:  2019-10-23
;; Modified: 2022-08-11
;;
;;; Commentary:
;;
;;  Lines and Pretty Things
;;
;;; Code:

(require 'cl-lib)

(require 'mis-error)
(require 'mis-valid)
(require 'mis-parse)


;;------------------------------------------------------------------------------
;; Lines
;;------------------------------------------------------------------------------

(defun mis:line (&rest args)
  "Create a line by repeating a character/string.

ARGS should be a plist of:
  - Any valid `mis:style' args (e.g. `:width' for a max line width).
  - `:string' - A string or character to repeat.
    - Or if no `:string', keyword, just the string/character value.

Examples:
  (mis:line :width 10 :string \"xX\")
  (mis:line \"-\")"
  (let* ((ast/parsed (apply 'int<mis>:parse
                            'mis:line
                            '(:line :style)
                            args))
         (line/string (int<mis>:syntax:find 'mis:line ast/parsed :tmp:line :string)) ; Explicit string.
         (mis/string  (int<mis>:syntax:find 'mis:line ast/parsed :mis:string))       ; Implicit string.
         key
         value
         ast/out)

    ;;------------------------------
    ;; String Error Checking / Normalization
    ;;------------------------------
    ;; After this, `line/string' will be used and `mis/string' will be ignored.

    ;; Have an explicit `:string' in `:tmp:line'?
    (cond ((and (not (null line/string))
                (not (null mis/string)))
           ;; `:mis:string' not allowed in this case.
           (int<mis>:error 'mis:line
                           "Invalid ARGS; cannot have both `:string' and `:mis:string'."
                           "`:string': %S, `:mis:string': %S, ARGS: %S"
                           line/string
                           mis/string
                           args))

          ;; No `:string' but do have a `:mis:string'?
          ((and (null line/string)
                (not (null mis/string)))
           ;; Use `:mis:string' as our line's `:string'.
           (if (or (stringp mis/string)
                   (characterp mis/string))
               (setq line/string mis/string)
             (int<mis>:error 'mis:line
                             '("Invalid ARGS; line string can only be a "
                               "string or character. "
                               "Got %S %S, ARGS: %S")
                             (type-of mis/string)
                             mis/string
                             args)))

          ;; `:string' and no `:mis:string'? Ok as-is.
          ((and (not (null line/string))
                (null mis/string))
           nil)

          ;; No `:string', no `:mis:string'...?
          (t
           (int<mis>:error 'mis:line
                           '("Invalid ARGS; line must have a string/character to repeat. "
                             "Expected `%s' or `%s'. "
                             "Found nothing in ARGS: %S -> %S")
                           :string
                           :mis:string
                           args
                           ast/parsed)))

    ;;------------------------------
    ;; Build & return the AST.
    ;;------------------------------
    ;; Convert `:tmp:line' from parsing into `:mis:format'.
    (setq ast/out (int<mis>:syntax:create 'mis:line
                                          :mis:format
                                          (cons :formatter 'repeat)
                                          (cons :string    line/string)))

    ;; Add `:style', etc to our output syntax (but ignore message/string).
    (int<mis>:syntax:merge 'mis:line
                           ast/out
                           ast/parsed
                           :tmp:line :mis:string :mis:message)))
;; (mis:line :width 10 :string "xX")
;;   -> '((:mis:style (:width . 10)) (:mis:format (:formatter . repeat) (:string . "xX")))
;; (mis:line "-")
;;   -> '((:mis:format (:formatter . repeat) (:string . "-")))
;; (mis:line :width 10 :string "hi")
;; (mis:line :string "hi")


;;------------------------------------------------------------------------------
;; Boxes
;;------------------------------------------------------------------------------

;; TODO: this?    (defun mis:box (&key width corner:top/left corner:top/right corner:bottom/left corner:bottom/right horizontal vertical)
;; TODO: or this? (defun mis:box (&key width corners horizontal vertical)
;; (mis:box :width 80
;;          :corner:top/left "┌"
;;          :corner:top/right "┐"
;;          :corner:bottom/left "└"
;;          :corner:bottom/right "┘"
;;          :horizontal "─"
;;          :vertical "│")
;; (mis:box :width 80
;;          :corners '("┌" "┐" "└" "┘")
;;          :horizontal "─"
;;          :vertical "│")
;; TODO: Actually I think this:
;; (mis:box :width 80 :border 'hypen) ;; or `ascii'?
;; (mis:box :width 80 :border 'line)
;; (mis:box :width 80 :border 'double)


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'mis-art)
;;; mis-art.el ends here

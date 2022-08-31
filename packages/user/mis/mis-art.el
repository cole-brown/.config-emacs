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
  (let* ((syntax/parsed (apply #'int<mis>:parse
                               'mis:line
                               :line
                               '(:line :style)
                               args))
         (line/string (int<mis>:format:syntax 'mis:line
                                              syntax/parsed))
         syntax/out)

    ;;------------------------------
    ;; Error Checking
    ;;------------------------------

    ;; Have an explicit `:string' in `:tmp:line'?
    (unless line/string
      (int<mis>:error 'mis:line
                      '("Invalid ARGS; line must have a string/character to repeat. "
                        "Found nothing in ARGS: %S -> %S")
                      args
                      syntax/parsed))

    ;;------------------------------
    ;; Build & return the SYNTAX.
    ;;------------------------------
    ;; Convert `:tmp:line' from parsing into `:mis:format'.
    (setq syntax/out (int<mis>:syntax:create 'mis:line
                                             :mis:format
                                             (cons :formatter 'repeat)
                                             (cons :string    line/string)))

    ;; Add `:style', etc to our output syntax (but ignore message/string).
    (int<mis>:syntax:merge 'mis:line
                           syntax/out
                           syntax/parsed
                           :tmp:line :mis:char :mis:string :mis:message)))
;; (mis:line "-")
;; (mis:line :width 10 :string "xX")
;;   -> '((:mis:style (:width . 10)) (:mis:format (:formatter . repeat) (:string . "xX")))
;; (mis:line "-")
;;   -> '((:mis:format (:formatter . repeat) (:string . "-")))
;; (mis:line ?-)
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

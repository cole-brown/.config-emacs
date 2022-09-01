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
  (let* ((syntax (apply #'int<mis>:parse
                        'mis:line
                        :line
                        '(:line :style)
                        args))
         (syntax/line (int<mis>:syntax:get/value 'mis:line
                                                 :line
                                                 syntax)))

    ;;------------------------------
    ;; Error checking.
    ;;------------------------------
    (unless syntax/line
      (int<mis>:error 'mis:line
                      '("Invalid ARGS; did not find a `:line' in parsed syntax! "
                        "ARGS: %S -> %S")
                      args
                      syntax))

    ;;------------------------------
    ;; Create output syntax.
    ;;------------------------------
    ;; Convert `:line' from parsing into `:format'.
    (setq syntax/out (apply #'int<mis>:syntax:create
                            'mis:line
                            :format
                            (cons :formatter 'repeat)
                            ;; Just assume there's a string somewhere in `:children'; will error at compile time.
                            (cons :value     :child)
                            ;; And just stuff the rest of what was parsed into our output.
                            syntax/line))))
;; (mis:line "-")
;; (mis:line :width 10 :string "xX")
;;   -> '((:style (:width . 10)) (:format (:formatter . repeat) (:string . "xX")))
;; (mis:line "-")
;;   -> '((:format (:formatter . repeat) (:string . "-")))
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

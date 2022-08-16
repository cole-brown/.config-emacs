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


;;------------------------------------------------------------------------------
;; Lines
;;------------------------------------------------------------------------------

(cl-defun mis:line (&key width string)
  "Create a line with a maximum WIDTH by repeating STRING characters.

WIDTH should be a positive integer.

STRING should be a character or a string."
  ;; TODO: switch to `int<mis>:parse'?

  ;;------------------------------
  ;; Error Checking
  ;;------------------------------
  ;; Also collapse the synonym key params while we're at it.
  (let ((width (int<mis>:valid:positive-integer? 'mis:line
                                                 'width
                                                 width))
        (string (int<mis>:valid:string-or-char? 'mis:line
                                                'string
                                                string)))

    ;;------------------------------
    ;; Build & return normalized list.
    ;;------------------------------
    (list :format 'repeat
          :width  width
          :string string)))
;; (mis:line :width 10 :string "hi")
;; (mis:line :length 10 :string "hi")


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

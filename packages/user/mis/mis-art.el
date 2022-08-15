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
  "Create a line of characters width.

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


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'mis-art)
;;; mis-art.el ends here

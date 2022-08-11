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


;;------------------------------------------------------------------------------
;; Lines
;;------------------------------------------------------------------------------

(cl-defun mis:line (&key width length string str character char)
  "Create a line of characters width.

WIDTH (or LENGTH) should be a positive integer.
  - WIDTH and LENGTH are synonyms; WIDTH is preferred.

STRING (or STR, CHAR, CHARACTER) should be a character or a string.
  - They are synonyms; STRING is preferred."
  ;;------------------------------
  ;; Error Checking
  ;;------------------------------
  ;; Also collapse the synonym key params while we're at it.
  (let ((width (int<mis>:valid:positive-integer? 'mis:line
                                                 (if width 'width 'length)
                                                 (if width width length)))
        (char (int<mis>:valid:string-or-char? 'mis:line
                                              (if width 'width 'length)
                                              (if width width length))))

    ;;------------------------------
    ;; Build & return normalized list.
    ;;------------------------------
    (list :format 'repeat
          :width  width
          :string string)))
;; (mis:line :width 10 :string "hi")


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'mis-art)
;;; mis-art.el ends here

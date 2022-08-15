;;; mis-align.el --- String Alignment for Mis -*- lexical-binding: t; -*-
;;
;; Author: Cole Brown <code@brown.dev>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:  2019-10-23
;; Modified: 2022-08-08
;;
;;; Commentary:
;;
;;  Align strings.
;;
;;; Code:


(require 'cl-lib)

(require 'mis-error)
(require 'mis-valid)


;;------------------------------------------------------------------------------
;; Alignment
;;------------------------------------------------------------------------------

(cl-defun int<mis>:align (string &key align width padding)
  "Align STRING based on ALIGN.

STRING must be a string.

ALIGN is required and must be one of:
  `:center', `:left', `:right', `center', `left', `right'

WIDTH is required and must be a positive integer.

PADDING is optional. If not supplied/nil, it will default to a space. Else it
should be a character or a string of length 1.

Return a string of length WIDTH, aligned with PADDING characters. If STRING is
too long, returns it as-is (un-truncated)."
  (declare (pure t) (side-effect-free t))

  ;;------------------------------
  ;; Error Checks
  ;;------------------------------
  (cond ((int<mis>:valid:string? 'int<mis>:align 'string string))

        ;; Must be a keyword or its symbol equivalent.
        ((int<mis>:valid:member? 'int<mis>:align
                                 'align
                                 align
                                 int<mis>:valid:align/types))

        ;; Must be positive integer
        ((or (not (integerp width))
             (< width 1))
         (int<mis>:error 'int<mis>:align
                         "WIDTH must be a positive integer. Got a %S: %S"
                         (type-of width)
                         width))

        ((and (not (null padding))
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

        (t
         nil))

  ;;------------------------------
  ;; Alignment
  ;;------------------------------
  (let ((padding (cond ((stringp padding)
                        (string-to-char padding))
                       ((characterp padding)
                        padding)
                       (t
                        ;; Default: A space character.
                        ?\s))))
    ;; Choose the proper alignment function for the ALIGN keyword.
    (cond ((> (length string) width)
           ;; STRING is too long to align; just return as-is.
           string)
          ((memq align '(:center center))
           (int<mis>:align/center string width padding))
          ((memq align '(:left left))
           (int<mis>:align/left string width padding))
          ((memq align '(:right right))
           (int<mis>:align/right string width padding))
          (t
           (int<mis>:error 'int<mis>:align
                           "Unhandled ALIGN of %S!"
                           align)))))


(defun int<mis>:align/center (string width padding)
  "Do not use this; use `int<mis>:align' instead.
This function has no error checking.

Pad string to WIDTH with PADDING character so that it is centered.

If STRING is too long, just return it (as-is/un-truncated)."
  (declare (pure t) (side-effect-free t))
  (let ((pad-amt (max 0 (- width (length string)))))
    (concat
     (make-string (ceiling pad-amt 2) (string-to-char padding))
     string
     (make-string (floor pad-amt 2) (string-to-char padding)))))


(defun int<mis>:align/left (string width padding)
  "Do not use this; use `int<mis>:align' instead.
This function has no error checking.

Pad STRING with PADDING on the left up to WIDTH.

If STRING is too long, just return it (as-is/un-truncated)."
  (declare (pure t) (side-effect-free t))
  (let ((pad-amt (max 0 (- width (length string)))))
    (concat (make-string pad-amt (string-to-char padding))
            string)))


(defun int<mis>:align/right (string width padding)
  "Do not use this; use `int<mis>:align' instead.
This function has no error checking.

Pad STRING with PADDING on the right up to WIDTH.

If STRING is too long, just return it (as-is/un-truncated)."
  (declare (pure t) (side-effect-free t))
  (let ((pad-amt (max 0 (- width (length string)))))
    (concat string
            (make-string pad-amt (string-to-char padding)))))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'mis-align)
;;; mis-align.el ends here

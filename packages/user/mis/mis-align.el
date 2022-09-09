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


(require 'mis-error)
(require 'mis-valid)


;;------------------------------------------------------------------------------
;; Alignment
;;------------------------------------------------------------------------------


(defun int<mis>:style:align (caller string style &optional _ alignment)
  "Align STRING based ALIGNMENT.

STRING should be the string to be aligned.

STYLE should be a Mis Syntax Tree of styling.
  - Optional:
    - `:width'
      - Must be a positive integer. If not provided, will default to
        `fill-column'.
    - `:padding'
      - Must be a character or a string of length 1.
      - If not supplied, it will default to a space (\" \").
      - Used to pad/fill STRING to correct ALIGNMENT.

ALIGNMENT should be one of these symbols: `center', `left', `right'

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)

Return an aligned string. If STRING is too long, returns it as-is
\(un-aligned, un-truncated)."
  (declare (pure t) (side-effect-free t))

  (let* ((caller        (list 'int<mis>:style:align caller))
         (width         (int<mis>:style:width caller style))
         (padding       (int<mis>:style:padding caller style " ")))

    ;;------------------------------
    ;; Error Checks
    ;;------------------------------
    ;; `width' & `padding' error checked in their getters.

    ;; These will signal an error if invalid.
    (int<mis>:valid:string? caller
                            'string
                            string)
    (int<mis>:valid:member? caller
                            'alignment
                            alignment
                            int<mis>:valid:align/types)

    ;;------------------------------
    ;; Align String & Return
    ;;------------------------------
    ;; Choose the proper alignment function for the ALIGNMENT type.
    (cond ((>= (length string) width)
           ;; STRING is too long to align; just return as-is.
           string)
          ((eq alignment 'center)
           (int<mis>:align/center string width padding))
          ((eq alignment 'left)
           (int<mis>:align/left string width padding))
          ((eq alignment 'right)
           (int<mis>:align/right string width padding))
          (t
           (int<mis>:error caller
                           "Unhandled ALIGNMENT of %S!"
                           alignment)))))
;; (int<mis>:style:align 'test "hello" (mis:style :width 20) :align 'center)


;; Register our users of the no-op styler:
(int<mis>:styler:register :align #'int<mis>:style:align)


(defun int<mis>:align/center (string width padding)
  "Do not use this; use `int<mis>:align' instead.
This function has no error checking.

Pad string to WIDTH with PADDING character so that it is centered.

If STRING is too long, just return it (as-is/un-truncated)."
  (declare (pure t) (side-effect-free t))
  (let ((pad-amt (max 0 (- width (length string)))))
    (concat
     (make-string (floor pad-amt 2) (string-to-char padding))
     string
     (make-string (ceiling pad-amt 2) (string-to-char padding)))))


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

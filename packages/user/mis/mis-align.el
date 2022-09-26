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
(require 'mis-tree-syntax)
(require 'mis-tree-output)
(require 'mis-style)


;;------------------------------------------------------------------------------
;; Alignment
;;------------------------------------------------------------------------------

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


(defun int<mis>:style:align (caller string _ style &optional _ alignment)
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

Ignoring:
  1) Mis Syntax Tree parameter
  2) `:align' keyword parameter

Return an aligned string. If STRING is too long, returns it as-is
\(un-aligned, un-truncated)."
  (declare (pure t) (side-effect-free t))

  (let* ((caller  (list 'int<mis>:style:align caller))
         (width   (int<mis>:style:width caller style))
         (padding (int<mis>:style:padding caller style " ")))


    (int<mis>:debug caller
                    "alignment: %S"
                    alignment)
    (int<mis>:debug caller
                    "string:    %S"
                    string)
    (int<mis>:debug caller
                    "style:    %S"
                    style)
    (int<mis>:debug caller
                    "width:    %S"
                    width)
    (int<mis>:debug caller
                    "padding:    %S"
                    padding)

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
;; (int<mis>:style:align 'test "hello" nil (mis:style :width 20) :align 'center)
;; (int<mis>:style:align 'test "hello" nil nil :align 'center)


;; Register our users of the no-op styler:
(int<mis>:styler:register :align #'int<mis>:style:align)


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'mis-align)
;;; mis-align.el ends here

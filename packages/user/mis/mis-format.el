;;; mis-format.el --- String Formatting for Mis -*- lexical-binding: t; -*-
;;
;; Author: Cole Brown <code@brown.dev>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:  2019-10-23
;; Modified: 2022-08-08
;;
;;; Commentary:
;;
;;  Make (That String Formatted Just) So.
;;
;;; Code:


(require 'cl-lib)
(require 'mis-error)


;;------------------------------------------------------------------------------
;; Constants
;;------------------------------------------------------------------------------

(defconst int<mis>:format:keywords
  '(:trim
    :indent)
  "Valid 'mis' formatting keywords.")


;;------------------------------------------------------------------------------
;; Basic Formatting
;;------------------------------------------------------------------------------

(defun int<mis>:format:message (caller message &rest args)
  "Format MESSAGE & ARGS in preparation for 'mis' formatting operations.

CALLER should be calling function's name. It can be either:
  - a string
  - a quoted symbol
  - or a function-quoted symbol

MESSAGE should be a string that `format' understands.

ARGS should be the `format' ARGS for MESSAGE."
  (cond ((not (stringp message))
         (int<mis>:error caller
                         "MESSAGE must be a string; got %S: %S"
                         (type-of message)
                         message))

        ;; Ok; format and return.
        (t
         (apply #'format message args))))


;;------------------------------------------------------------------------------
;; Trim / Truncate
;;------------------------------------------------------------------------------

(cl-defun int<mis>:format:trim (caller string &key trim trim:left trim:right)
  "Trim STRING of leading and trailing strings matching TRIM-LEFT and TRIM-RIGHT.

TRIM, TRIM:LEFT, and TRIM:RIGHT should be nil or regex strings. They default to
\"[ \\t\\n\\r]+\". TRIM is for both left and right, and will raise an error if
specified with TRIM:LEFT and/or TRIM:RIGHT.

CALLER should be calling function's name. It can be either:
  - a string
  - a quoted symbol
  - or a function-quoted symbol"
  ;;------------------------------
  ;; Error Checks
  ;;------------------------------
  (cond ((and trim trim:left trim:right)
         (int<mis>:error caller
                         '("TRIM:LEFT & TRIM:RIGHT cannot be specified with TRIM. "
                           "Got TRIM: %S, TRIM:LEFT: %S, TRIM:RIGHT: %S")
                         trim
                         trim:left
                         trim:right))
        ((and trim trim:left)
         (int<mis>:error caller
                         '("TRIM:LEFT cannot be specified with TRIM. "
                           "Got TRIM: %S, TRIM:LEFT: %S")
                         trim
                         trim:left))
        ((and trim trim:right)
         (int<mis>:error caller
                         '("TRIM:RIGHT cannot be specified with TRIM. "
                           "Got TRIM: %S, TRIM:RIGHT: %S")
                         trim
                         trim:right))
        ((not (stringp string))
         (int<mis>:error caller
                         "STRING to trim must be a string. Got a %S: %S"
                         (type-of string)
                         string))

        ;;------------------------------
        ;; Trimming
        ;;------------------------------
        ;; Trim left & right differently.
        ((and trim:left
              trim:right)
         (string-trim string trim:left trim:right))

        (trim:left
         (string-trim-left string trim:left))

        (trim:right
         (string-trim-right string trim:right))

        ;; Trim both sides the same. Either both default or both with supplied
        ;; TRIM regex.
        (t
         (string-trim string trim trim))))
;; (int<mis>:format:trim "bob" "   hello?   ")
;; (int<mis>:format:trim "bob" "   hello?   " :trim (rx (one-or-more (or " " "?"))))
;; (int<mis>:format:trim "bob" "   hello?   " :trim:left (rx (one-or-more (or " " "?"))))
;; (int<mis>:format:trim "bob" "   hello?   " :trim:right (rx (one-or-more (or " " "?"))))
;; (int<mis>:format:trim "bob" "   hello?   " :trim:left (rx (one-or-more (or " " "h"))) :trim:right (rx (one-or-more (or " " "?"))))
;; (int<mis>:format:trim "bob" 'hello)


;;------------------------------------------------------------------------------
;; Indentation
;;------------------------------------------------------------------------------

(defun int<mis>:format:indent:amount (buffer position type)
  "Figure out indentation for BUFFER at POSITION according to indent TYPE.

BUFFER should be a buffer object or a buffer name string.

POSITION should be an integer or buffer marker.

TYPE can be:
  :fixed     -> indent to `current-column'
  :existing  -> do not indent; use `current-column' as indent amount.
  :auto      -> indent according to `indent-according-to-mode'
  an integer -> that number of spaces

Return indentation as an integer (number of spaces)."
  (let (buffer/curr
        indent/amount)
    ;;------------------------------
    ;; Error Checks
    ;;------------------------------
    (cond ((and (not (stringp buffer))
                (not (bufferp buffer)))
           (int<mis>:error 'int<mis>:format:indent:amount
                           "BUFFER must be a string or buffer object. Got a %S: %S"
                           (type-of buffer)
                           buffer))

          ((not (integer-or-marker-p position))
           (int<mis>:error 'int<mis>:format:indent:amount
                           "POSITION must be an integer or marker. Got a %S: %S"
                           (type-of position)
                           position))

          ((and (not (memq type '(:fixed :existing :auto)))
                (not (integerp type)))
           (int<mis>:error 'int<mis>:format:indent:amount
                           "TYPE must be an integer or one of: %S. Got a %S: %S"
                           '(:fixed :existing :auto)
                           (type-of type)
                           type))

          (t
           (setq buffer/curr (get-buffer buffer))
           (unless buffer/curr
             (int<mis>:error 'int<mis>:format:indent:amount
                             "Could not find BUFFER: %S"
                             buffer))))

    ;;------------------------------
    ;; Determine Indentation
    ;;------------------------------
    (if (integerp type)
        (setq indent/amount type)
      ;; Else need to ask the buffer about indentation.
      (with-current-buffer buffer/curr
        (save-excursion
          ;; Want to back out our changes as we're only interested in the indent
          ;; _amount_ right now.
          (let ((change-group (prepare-change-group)))
            (goto-char position)
            (beginning-of-line-text)

            ;; `:fixed' and `:existing' have nothing to do; they're already at the
            ;; correct column, presumably. `:auto' needs to figure out what the
            ;; buffer's mode wants.
            (when (eq indent :auto)
              (indent-according-to-mode)
              (beginning-of-line-text))

            ;; Save indent amount.
            (setq indent/amount (current-column))

            ;; Unilaterally cancel any changes made.
            (cancel-change-group change-group)))))

    ;; Return discovered indent amount.
    indent/amount))


;;------------------------------------------------------------------------------
;; Alignment
;;------------------------------------------------------------------------------

(cl-defun int<mis>:format:align (string &key align width padding)
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
  (cond ((not (stringp string))
         (int<mis>:error 'int<mis>:format:align
                         "STRING must be a string. Got a %S: %S"
                         (type-of string)
                         string))

        ;; Must be a keyword or its symbol equivalent.
        ((not (memq align '(:center center :left left :right right)))
         (int<mis>:error 'int<mis>:format:align
                         "ALIGN must be one of: %S. Got a %S: %S"
                         '(:center center :left left :right right)
                         (type-of align)
                         align))

        ;; Must be positive integer
        ((or (not (integerp width))
             (< width 1))
         (int<mis>:error 'int<mis>:format:align
                         "WIDTH must be a positive integer. Got a %S: %S"
                         (type-of width)
                         width))

        ((and (not (null padding))
              (not (stringp padding))
              (not (characterp padding)))
         (int<mis>:error 'int<mis>:format:align
                         "PADDING must be nil, a character, or a string of length 1. Got a %S: %S"
                         (type-of padding)
                         padding))

        ((and (stringp padding)
              (not (= (length padding) 1)))
         (int<mis>:error 'int<mis>:format:align
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
           (int<mis>:format:align/center string width padding))
          ((memq align '(:left left))
           (int<mis>:format:align/left string width padding))
          ((memq align '(:right right))
           (int<mis>:format:align/right string width padding))
          (t
           (int<mis>:error 'int<mis>:format:align
                           "Unhandled ALIGN of %S!"
                           align)))))


(defun int<mis>:format:align/center (string width padding)
  "Do not use this; use `int<mis>:format:align' instead.
This function has no error checking.

Pad string to WIDTH with PADDING character so that it is centered.

If STRING is too long, just return it (as-is/un-truncated)."
  (declare (pure t) (side-effect-free t))
  (let ((pad-amt (max 0 (- width (length string)))))
    (concat
     (make-string (ceiling pad-amt 2) (string-to-char padding))
     string
     (make-string (floor pad-amt 2) (string-to-char padding)))))


(defun int<mis>:format:align/left (string width padding)
  "Do not use this; use `int<mis>:format:align' instead.
This function has no error checking.

Pad STRING with PADDING on the left up to WIDTH.

If STRING is too long, just return it (as-is/un-truncated)."
  (declare (pure t) (side-effect-free t))
  (let ((pad-amt (max 0 (- width (length string)))))
    (concat (make-string pad-amt (string-to-char padding))
            string)))


(defun int<mis>:format:align/right (string width padding)
  "Do not use this; use `int<mis>:format:align' instead.
This function has no error checking.

Pad STRING with PADDING on the right up to WIDTH.

If STRING is too long, just return it (as-is/un-truncated)."
  (declare (pure t) (side-effect-free t))
  (let ((pad-amt (max 0 (- width (length string)))))
    (concat string
            (make-string pad-amt (string-to-char padding)))))


;;------------------------------------------------------------------------------
;; Lines
;;------------------------------------------------------------------------------

(cl-defun int<mis>:format:repeat (character length)
  "Create a line separators or what have you, like:
--------

CHARACTER should be a character or string of length 1.

LENGTH should be an integer greater than zero."

  (cond ((and (not (stringp character))
              (not (characterp character)))
         (int<mis>:error caller
                         '("CHARACTER must be a string or character. "
                           "Got a %S: %S")
                         (type-of character)
                         character))

        ((and (stringp character)
              (not (= (length character) 1)))
         (int<mis>:error 'int<mis>:format:align
                         '("CHARACTER must be a character or a string of length 1. "
                           "Got a string of length %S: %S")
                         (length character)
                         character))

        ((not (integerp length))
         (int<mis>:error caller
                         '("LENGTH must be an integer. "
                           "Got a %S: %S")
                         (type-of length)
                         length))

        (t
         (make-string length
                      (if (stringp character)
                          (string-to-char character)
                        character)
                      :multibyte))))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'mis-format)
;;; mis-format.el ends here

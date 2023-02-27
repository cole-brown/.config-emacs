;;; elisp/utils/units.el --- Nicer units? -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2023-02-24
;; Modified:   2023-02-24
;; URL:        https://github.com/cole-brown/.config-emacs
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Kilobytes, hogsheads, nautical miles...
;;
;;; Code:
;;; elisp/utils/types.el -*- lexical-binding: t; -*-


;;------------------------------------------------------------------------------
;; Bytes
;;------------------------------------------------------------------------------

(defconst int<units>:bytes:multiplier/decimal (expt 10 3)
  "Decimal / IEC / SI units are multiplied by 1000.")


(defconst int<units>:bytes:multiplier/binary  (expt 2 10)
  "Binary units are multiplied by the closest power of 2 to 1000.")


(defconst int<units>:bytes
  (list
   (cons 'b           1)
   (cons 'B           1)
   (cons 'byte        1)
   (cons 'bytes       1)

   (cons 'kb          (expt int<units>:bytes:multiplier/decimal 1))
   (cons 'kB          (expt int<units>:bytes:multiplier/decimal 1))
   (cons 'kilobyte    (expt int<units>:bytes:multiplier/decimal 1))
   (cons 'kilobytes   (expt int<units>:bytes:multiplier/decimal 1))
   (cons 'kib         (expt int<units>:bytes:multiplier/binary  1))
   (cons 'kiB         (expt int<units>:bytes:multiplier/binary  1))
   (cons 'kibibyte    (expt int<units>:bytes:multiplier/binary  1))
   (cons 'kibibytes   (expt int<units>:bytes:multiplier/binary  1))

   (cons 'mb          (expt int<units>:bytes:multiplier/decimal 2))
   (cons 'MB          (expt int<units>:bytes:multiplier/decimal 2))
   (cons 'megabyte    (expt int<units>:bytes:multiplier/decimal 2))
   (cons 'megabytes   (expt int<units>:bytes:multiplier/decimal 2))
   (cons 'mib         (expt int<units>:bytes:multiplier/binary  2))
   (cons 'MiB         (expt int<units>:bytes:multiplier/binary  2))
   (cons 'mebibyte    (expt int<units>:bytes:multiplier/binary  2))
   (cons 'mebibytes   (expt int<units>:bytes:multiplier/binary  2))

   (cons 'gb          (expt int<units>:bytes:multiplier/decimal 3))
   (cons 'GB          (expt int<units>:bytes:multiplier/decimal 3))
   (cons 'gigabyte    (expt int<units>:bytes:multiplier/decimal 3))
   (cons 'gigabytes   (expt int<units>:bytes:multiplier/decimal 3))
   (cons 'gib         (expt int<units>:bytes:multiplier/binary  3))
   (cons 'GiB         (expt int<units>:bytes:multiplier/binary  3))
   (cons 'gibibyte    (expt int<units>:bytes:multiplier/binary  3))
   (cons 'gibibytes   (expt int<units>:bytes:multiplier/binary  3))

   (cons 'tb          (expt int<units>:bytes:multiplier/decimal 4))
   (cons 'TB          (expt int<units>:bytes:multiplier/decimal 4))
   (cons 'terabyte    (expt int<units>:bytes:multiplier/decimal 4))
   (cons 'terabytes   (expt int<units>:bytes:multiplier/decimal 4))
   (cons 'tib         (expt int<units>:bytes:multiplier/binary  4))
   (cons 'TiB         (expt int<units>:bytes:multiplier/binary  4))
   (cons 'tebibyte    (expt int<units>:bytes:multiplier/binary  4))
   (cons 'tebibytes   (expt int<units>:bytes:multiplier/binary  4))

   (cons 'pb          (expt int<units>:bytes:multiplier/decimal 5))
   (cons 'PB          (expt int<units>:bytes:multiplier/decimal 5))
   (cons 'petabyte    (expt int<units>:bytes:multiplier/decimal 5))
   (cons 'petabytes   (expt int<units>:bytes:multiplier/decimal 5))
   (cons 'pib         (expt int<units>:bytes:multiplier/binary  5))
   (cons 'PiB         (expt int<units>:bytes:multiplier/binary  5))
   (cons 'pebibyte    (expt int<units>:bytes:multiplier/binary  5))
   (cons 'pebibytes   (expt int<units>:bytes:multiplier/binary  5))

   (cons 'eb          (expt int<units>:bytes:multiplier/decimal 6))
   (cons 'EB          (expt int<units>:bytes:multiplier/decimal 6))
   (cons 'exabyte     (expt int<units>:bytes:multiplier/decimal 6))
   (cons 'exabytes    (expt int<units>:bytes:multiplier/decimal 6))
   (cons 'eib         (expt int<units>:bytes:multiplier/binary  6))
   (cons 'EiB         (expt int<units>:bytes:multiplier/binary  6))
   (cons 'exbibyte    (expt int<units>:bytes:multiplier/binary  6))
   (cons 'exbibytes   (expt int<units>:bytes:multiplier/binary  6))

   (cons 'zb          (expt int<units>:bytes:multiplier/decimal 7))
   (cons 'ZB          (expt int<units>:bytes:multiplier/decimal 7))
   (cons 'zettabyte   (expt int<units>:bytes:multiplier/decimal 7))
   (cons 'zettabytes  (expt int<units>:bytes:multiplier/decimal 7))
   (cons 'zib         (expt int<units>:bytes:multiplier/binary  7))
   (cons 'ZiB         (expt int<units>:bytes:multiplier/binary  7))
   (cons 'zebibyte    (expt int<units>:bytes:multiplier/binary  7))
   (cons 'zebibytes   (expt int<units>:bytes:multiplier/binary  7))

   (cons 'yb          (expt int<units>:bytes:multiplier/decimal 8))
   (cons 'YB          (expt int<units>:bytes:multiplier/decimal 8))
   (cons 'yottabyte   (expt int<units>:bytes:multiplier/decimal 8))
   (cons 'yottabytes  (expt int<units>:bytes:multiplier/decimal 8))
   (cons 'yib         (expt int<units>:bytes:multiplier/binary  8))
   (cons 'YiB         (expt int<units>:bytes:multiplier/binary  8))
   (cons 'yobibyte    (expt int<units>:bytes:multiplier/binary  8))
   (cons 'yobibytes   (expt int<units>:bytes:multiplier/binary  8)))
  "Alist of byte units in terms of how many bytes they are.")


(defun unit:byte (value unit)
  "Return integer of bytes for VALUE of UNITS bytes.

Example:
  (unit:byte 2 'kb)
    -> 2000
  (unit:byte 2 'kib)
    -> 2048"
  (declare (pure t) (side-effect-free t))
  (let ((func/name "unit:byte")
        (unit/multiplier (alist-get unit int<units>:bytes)))
    ;;------------------------------
    ;; Error Checks
    ;;------------------------------
    (unless unit/multiplier
      (nub:error
          :innit
          func/name
        "Unknown unit name '%S'! See `int<units>:bytes' for allowed units."
        unit))
    (unless (integerp value)
      (nub:error
          :innit
          func/name
        "VALUE should be an integer. Got %S: %S"
        (type-of value)
        value))

    ;;------------------------------
    ;; Unit Conversion
    ;;------------------------------
    (* value unit/multiplier)))
;; (unit:byte 100 'kb)




;; TODO: Put/get the unit in this symbol's `unit' slot?

;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :elisp 'utils 'units)

;;; core/modules/emacs/buffer/narrow.el --- Narrow/Widen Buffer -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2022-12-20
;; Modified:   2022-12-20
;; URL:        https://github.com/cole-brown/.config-emacs
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;  Narrow/Widen Buffer
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Buffer Searching Functions
;;------------------------------------------------------------------------------

(defmacro buffer:with-widened (&rest body)
  "Execute BODY while temporarily widening the buffer.

Save mark, excursion, and restriction before widening buffer and executing BODY."
  ;; Like `org-with-wide-buffer' but:
  ;;   1) `save-mark-and-excursion' instead of `save-excursion'.
  ;;   2) Doesn't depend on `org-mode'.
  (declare (debug (body)))
  `(save-mark-and-excursion
     (save-restriction
       (widen)
       ,@body)))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :buffer 'narrow)

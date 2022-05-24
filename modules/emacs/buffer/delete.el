;;; emacs/buffer/delete.el -*- lexical-binding: t; -*-



(defun int<buffer>:delete:word (arg)
  "Delete ARG number of words without putting them in the kill ring.

Kill characters forward until encountering the end of a word."
  (delete-region (point) (progn (forward-word arg) (point))))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :emacs 'buffer 'delete)

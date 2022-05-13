;;; mode/org/link.el -*- lexical-binding: t; -*-


;;------------------------------------------------------------------------------
;; Commands
;;------------------------------------------------------------------------------

;; Easy paste of e.g. URLs.
(defun mode:cmd:org:here/yank ()
  "Insert item from kill ring as an org-mode link with description 'here'."
  (interactive)
  (insert "[[")
  (yank)
  (insert "][here]]"))


(defun mode:cmd:org:here/link ()
  "Insert stored org link as an org-mode link with description 'here'."
  (interactive)
  (org-insert-link nil (car (car org-stored-links)) "here"))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :module 'mode 'org 'link)

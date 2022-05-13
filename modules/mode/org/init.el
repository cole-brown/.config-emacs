;; -*- mode: emacs-lisp; lexical-binding: t -*-


;;------------------------------------------------------------------------------
;; Org-Mode Stuff
;;------------------------------------------------------------------------------

(imp:timing
    '(:module mode org)
    (imp:file:current)
    (imp:path:current:dir)

  (imp:load :feature  '(:module mode org keyword)
            :filename "keyword")
  (imp:load :feature  '(:module mode org link)
            :filename "link"))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :module 'mode 'org)

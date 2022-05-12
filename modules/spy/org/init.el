;; -*- mode: emacs-lisp; lexical-binding: t -*-


;;------------------------------------------------------------------------------
;; Org-Mode Stuff
;;------------------------------------------------------------------------------

(imp:timing
    '(:modules spy org)
    (imp:file:current)
    (imp:path:current:dir)

  (imp:load :feature  '(:modules spy org keyword)
            :filename "keyword")
  (imp:load :feature  '(:modules spy org link)
            :filename "link"))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :modules 'spy 'org)

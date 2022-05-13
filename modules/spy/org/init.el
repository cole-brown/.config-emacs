;; -*- mode: emacs-lisp; lexical-binding: t -*-


;;------------------------------------------------------------------------------
;; Org-Mode Stuff
;;------------------------------------------------------------------------------

(imp:timing
    '(:module spy org)
    (imp:file:current)
    (imp:path:current:dir)

  (imp:load :feature  '(:module spy org keyword)
            :filename "keyword")
  (imp:load :feature  '(:module spy org link)
            :filename "link"))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :module 'spy 'org)

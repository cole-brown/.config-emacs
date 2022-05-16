;; -*- mode: emacs-lisp; lexical-binding: t -*-


;;------------------------------------------------------------------------------
;; Org-Mode Stuff
;;------------------------------------------------------------------------------

(let ((path/parent (imp:path:current:dir)))

  (imp:timing
      '(:module mode org)
      (imp:file:current)
      path/parent


      (imp:load :feature  '(:module mode org keyword)
                :path     path/parent
                :filename "keyword")
      (imp:load :feature  '(:module mode org link)
                :path     path/parent
                :filename "link")))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :module 'mode 'org)

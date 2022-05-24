;; -*- mode: emacs-lisp; lexical-binding: t -*-


;;------------------------------------------------------------------------------
;; Org-Mode Stuff
;;------------------------------------------------------------------------------

(let ((path/parent (imp:path:current:dir)))

  (imp:timing
      '(:mode org)
      (imp:file:current)
      path/parent


      (imp:load :feature  '(:mode org keyword)
                :path     path/parent
                :filename "keyword")
      (imp:load :feature  '(:mode org link)
                :path     path/parent
                :filename "link")))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mode 'org)

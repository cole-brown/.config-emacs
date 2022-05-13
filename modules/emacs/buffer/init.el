;;; emacs/buffer/init.el -*- lexical-binding: t; -*-


;;------------------------------------------------------------------------------
;; Load our sub-module thingies.
;;------------------------------------------------------------------------------

(imp:timing
    '(:module emacs buffer)
    (imp:file:current)
    (imp:path:current:dir)

  (imp:load :feature  '(:module emacs buffer delete)
            :filename "delete")
  (imp:load :feature  '(:module emacs buffer eval)
            :filename "eval")
  (imp:load :feature  '(:module emacs buffer line)
            :filename "line")
  (imp:load :feature  '(:module emacs buffer manage)
            :filename "manage")
  (imp:load :feature  '(:module emacs buffer name)
            :filename "name")
  (imp:load :feature  '(:module emacs buffer point)
            :filename "point")
  (imp:load :feature  '(:module emacs buffer search)
            :filename "search"))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :module 'emacs 'buffer)

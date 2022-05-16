;;; emacs/buffer/init.el -*- lexical-binding: t; -*-


;;------------------------------------------------------------------------------
;; Load our sub-module thingies.
;;------------------------------------------------------------------------------

(let ((path/parent (imp:path:current:dir)))

  (imp:timing
      '(:module emacs buffer)
      (imp:file:current)
      path/parent

    (imp:load :feature  '(:module emacs buffer delete)
              :path     path/parent
              :filename "delete")
    (imp:load :feature  '(:module emacs buffer eval)
              :path     path/parent
              :filename "eval")
    (imp:load :feature  '(:module emacs buffer line)
              :path     path/parent
              :filename "line")
    (imp:load :feature  '(:module emacs buffer manage)
              :path     path/parent
              :filename "manage")
    (imp:load :feature  '(:module emacs buffer name)
              :path     path/parent
              :filename "name")
    (imp:load :feature  '(:module emacs buffer point)
              :path     path/parent
              :filename "point")
    (imp:load :feature  '(:module emacs buffer search)
              :path     path/parent
              :filename "search")))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :module 'emacs 'buffer)

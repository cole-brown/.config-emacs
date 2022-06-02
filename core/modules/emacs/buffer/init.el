;;; emacs/buffer/init.el -*- lexical-binding: t; -*-


;;------------------------------------------------------------------------------
;; Set up imp.
;;------------------------------------------------------------------------------

(imp:path:root :buffer
               (imp:path:current:dir)
               "init.el")


;;------------------------------------------------------------------------------
;; Set up custom vars.
;;------------------------------------------------------------------------------

(defgroup buffer:group nil
  "Group namespace for the `:buffer' defcustoms."
  :prefix "buffer:"
  ;; Not really sure where to stick it..?
  :group 'files)


;;------------------------------------------------------------------------------
;; Load our sub-module thingies.
;;------------------------------------------------------------------------------

(let ((path/parent (imp:path:current:dir)))

  (imp:timing
      '(:buffer)
      (imp:file:current)
      path/parent

    (imp:load :feature  '(:buffer delete)
              :path     path/parent
              :filename "delete")
    (imp:load :feature  '(:buffer eval)
              :path     path/parent
              :filename "eval")
    (imp:load :feature  '(:buffer line)
              :path     path/parent
              :filename "line")
    (imp:load :feature  '(:buffer manage)
              :path     path/parent
              :filename "manage")
    (imp:load :feature  '(:buffer name)
              :path     path/parent
              :filename "name")
    (imp:load :feature  '(:buffer point)
              :path     path/parent
              :filename "point")
    (imp:load :feature  '(:buffer search)
              :path     path/parent
              :filename "search")))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :buffer)

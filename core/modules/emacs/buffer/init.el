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
;; Load files.
;;------------------------------------------------------------------------------

(imp:timing
    '(:buffer)
    (imp:file:current)
    (imp:path:current:dir)

  (imp:load :feature  '(:buffer delete)
            :filename "delete")
  (imp:load :feature  '(:buffer eval)
            :filename "eval")
  (imp:load :feature  '(:buffer line)
            :filename "line")
  (imp:load :feature  '(:buffer manage)
            :filename "manage")
  (imp:load :feature  '(:buffer name)
            :filename "name")
  (imp:load :feature  '(:buffer point)
            :filename "point")
  (imp:load :feature  '(:buffer search)
            :filename "search"))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :buffer)

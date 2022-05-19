;;; elisp/utils/init.el -*- lexical-binding: t; -*-

;;------------------------------------------------------------------------------
;; Set-Up.
;;------------------------------------------------------------------------------

(imp:require :path)

(imp:path:root :elisp
               (path:parent (imp:path:current:dir))
               (imp:file:current))


;;------------------------------------------------------------------------------
;; Load our sub-module thingies.
;;------------------------------------------------------------------------------

(imp:timing
    '(:elisp utils)
    (imp:file:current)
    (imp:path:current:dir)

  (imp:load :feature  '(:elisp utils functions)
            :path     (imp:path:current:dir/relative :elisp)
            :filename "functions")
  (imp:load :feature  '(:elisp utils types)
            :path     (imp:path:current:dir/relative :elisp)
            :filename "types")
  (imp:load :feature  '(:elisp utils test)
            :path     (imp:path:current:dir/relative :elisp)
            :filename "test"))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :elisp 'utils)

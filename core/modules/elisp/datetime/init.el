;;; elisp/datetime/init.el -*- lexical-binding: t; -*-


;;------------------------------------------------------------------------------
;; Set-Up.
;;------------------------------------------------------------------------------

(imp:path:root :datetime
               (imp:path:current:dir)
               (imp:file:current))


;;------------------------------------------------------------------------------
;; Load our sub-module thingies.
;;------------------------------------------------------------------------------

(imp:load :feature  '(:datetime format)
          :filename "format")
(imp:load :feature  '(:datetime timestamp)
          :filename "timestamp")


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide:with-emacs :datetime)

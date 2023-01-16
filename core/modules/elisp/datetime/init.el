;;; elisp/datetime/init.el -*- lexical-binding: t; -*-


;;------------------------------------------------------------------------------
;; Set-Up.
;;------------------------------------------------------------------------------

(imp:path:root/set :datetime
               (imp:path:current:dir)
               (imp:file:current))


;;------------------------------------------------------------------------------
;; Load our sub-module thingies.
;;------------------------------------------------------------------------------

(imp:timing
    '(:datetime)
    (imp:file:current)
    (imp:path:current:dir)

  (imp:load :feature  '(:datetime format)
            :filename "format")
  (imp:load :feature  '(:datetime timestamp)
            :filename "timestamp")


  ;;----------------------------------------------------------------------------
  ;; Initialize our Common Formats
  ;;----------------------------------------------------------------------------
  (imp:timing
      '(:datetime init)
      (imp:file:current)
      (imp:path:current:dir)

    (datetime:init)))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide:with-emacs :datetime)

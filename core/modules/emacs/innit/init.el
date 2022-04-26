;;; init.el --- Complicating, innit? -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2022-04-13
;; Modified:   2022-04-25
;; URL:        https://github.com/cole-brown/.config-emacs
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;  Adding structure to the uncomplicated, innit?
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Set up imp.
;;------------------------------------------------------------------------------
(imp:path:root :innit
               (imp:path:current:dir)
               "init.el")


;;------------------------------------------------------------------------------
;; Load files.
;;------------------------------------------------------------------------------

(imp:timing
    :innit
    "init.el"
    (imp:path:current:dir)

  (imp:load :feature  '(:innit vars)
            :filename "vars")
  (imp:load :feature  '(:innit nub)
            :filename "nub")
  (imp:load :feature  '(:innit status)
            :filename "status")
  (imp:load :feature  '(:innit debug)
            :filename "debug")
  (imp:load :feature  '(:innit load)
            :filename "load")
  (imp:load :feature  '(:innit feature)
            :filename "feature")
  ;; End load timing.
  )


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide:with-emacs :innit)

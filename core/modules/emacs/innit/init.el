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

(imp:path:root/set :innit
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
  (imp:load :feature  '(:innit error)
            :filename "error")
  (imp:load :feature  '(:innit debug)
            :filename "debug")
  (imp:load :feature  '(:innit nub)
            :filename "nub")
  (imp:load :feature  '(:innit time)
            :filename "time")
  (imp:load :feature  '(:innit optimize)
            :filename "optimize")
  (imp:load :feature  '(:innit package)
            :filename "package")
  (imp:load :feature  '(:innit squelch)
            :filename "squelch")
  (imp:load :feature  '(:innit hook)
            :filename "hook")
  (imp:load :feature  '(:innit advice)
            :filename "advice")
  (imp:load :feature  '(:innit theme)
            :filename "theme")

  ;; End load timing.
  )


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide:with-emacs :innit)

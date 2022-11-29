;;; init.el --- Emacs Windows -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2022-07-15
;; Modified:   2022-07-15
;; URL:        https://github.com/cole-brown/.config-emacs
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Functions for Emacs Windows
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Set up imp.
;;------------------------------------------------------------------------------

(imp:path:root :window
               (imp:path:current:dir)
               "init.el")


;;------------------------------------------------------------------------------
;; Load files.
;;------------------------------------------------------------------------------

(imp:timing
    '(:window)
    (imp:file:current)
    (imp:path:current:dir)

  (imp:load :feature  '(:window manage)
            :filename "manage")

  (imp:load :feature  '(:window commands)
            :filename "commands"))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :window)

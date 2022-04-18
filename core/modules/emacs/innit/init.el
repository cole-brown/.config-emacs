;;; init.el --- Complicating, innit? -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2022-04-13
;; Modified:   2022-04-14
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
;; Set up custom vars.
;;------------------------------------------------------------------------------

;; TODO: Put defgroup, defcustom here? Or in settings.el or customs.el or vars.el...


;;------------------------------------------------------------------------------
;; Load files.
;;------------------------------------------------------------------------------

(imp:load :feature  '(:innit feature)
          :filename "feature")


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide:with-emacs :innit)

;;; taskspace/init.el --- Per-task notes & files. -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2019-04-24
;; Modified:   2022-07-01
;; Version: 2.2
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Per-task notes & files.
;;
;; Initialize the taskspace module.
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Set imp Root.
;;------------------------------------------------------------------------------

(imp:path:root :taskspace
               (imp:path:current:dir)
               "init.el")


;;------------------------------------------------------------------------------
;; Load Files
;;------------------------------------------------------------------------------

(imp:timing
    '(:taskspace)
    (imp:file:current)
    path/parent

  (imp:load :feature  '(:taskspace taskspace)
            :filename "taskspace")

  ;; End load timing.
  )


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide:with-emacs :taskspace)

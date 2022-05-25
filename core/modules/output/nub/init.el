;;; init.el --- Multi-'user' output message helper. -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2021-11-30
;; Modified:   2022-04-21
;;
;;; Commentary:
;;
;; Output messages with output levels and output sinks settings per user.
;;
;;
;;
;;; Code:

;;                                 ──────────                                 ;;
;; ╔════════════════════════════════════════════════════════════════════════╗ ;;
;; ║               Nub: /noun/ A small lump or protuberance.                ║ ;;
;; ╚════════════════════════════════════════════════════════════════════════╝ ;;
;;                                   ──────                                   ;;
;;                       Hey... Naming things is hard.                        ;;
;;                                 ──────────                                 ;;


;;------------------------------------------------------------------------------
;; Set imp Root
;;------------------------------------------------------------------------------

(imp:path:root :nub
               (imp:path:current:dir)
               "init.el")


;;------------------------------------------------------------------------------
;; Load Files
;;------------------------------------------------------------------------------

(imp:timing
    :nub
    "init.el"
    (imp:path:current:dir)
  (imp:load :feature  '(:nub internal)
            :filename "internal")
  (imp:load :feature  '(:nub alist)
            :filename "alist")
  (imp:load :feature  '(:nub utils)
            :filename "utils")
  (imp:load :feature  '(:nub variables)
            :filename "variables")
  (imp:load :feature  '(:nub output)
            :filename "output")
  (imp:load :feature  '(:nub debug)
            :filename "debug")
  (imp:load :feature  '(:nub debug format)
            :filename "debug-format")
  (imp:load :feature  '(:nub error)
            :filename "error")

  ;; End load timing.
  )


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide:with-emacs :nub)

;;; init.el --- Emacs Vim Input Layer -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2022-07-11
;; Modified:   2022-07-11
;; Version:    0.1
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Emacs Vim Input Layer
;;
;; Evil Initialization & Set-Up
;;
;;; Code:

;;------------------------------------------------------------------------------
;; Set Root of All Evil?
;;------------------------------------------------------------------------------

;; Load ../../init.el to set up `:input' root.


;;------------------------------------------------------------------------------
;; Initialize Evilness...
;;------------------------------------------------------------------------------

(imp:timing
    '(:input keyboard evil init)
    (imp:file:current)
    (imp:path:current:dir)





  ;; End load timing.
  )


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :input 'keyboard 'evil 'init)

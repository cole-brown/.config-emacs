;;; 00-init.el --- Last chance to do things! -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <code@brown.dev>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2022-10-24
;; Modified:   2022-10-24
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; normal init's final steps
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Load Files
;;------------------------------------------------------------------------------

(imp:timing
    '(:core boot finalize)
    (imp:file:current)
    (imp:path:current:dir)

  (let ((path (imp:path:current:dir/relative :core)))
    (imp:load :feature  '(:core boot finalize output)
              :path     (imp:path:current:dir/relative :core)
              :filename "80-output.el")))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :core 'boot 'finalize)

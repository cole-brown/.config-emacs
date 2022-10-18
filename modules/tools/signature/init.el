;;; tools/signature/init.el --- Signatures, Emails, and Such -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <code@brown.dev>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2020-11-16
;; Modified:   2022-10-18
;;
;;; Commentary:
;;
;; Signatures, Emails, and Such
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Loading
;;------------------------------------------------------------------------------

(imp:timing
    '(:tools signature)
    (imp:file:current)
    (imp:path:current:dir)


  (imp:load :feature  '(:tools signature signature)
            :path     (imp:path:current:dir/relative :tools)
            :filename "signature"))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :tools 'signature)

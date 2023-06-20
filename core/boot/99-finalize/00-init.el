;;; core/boot/99-finalize/00-init.el --- Last chance to do things! -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2022-10-24
;; Timestamp:  2023-06-20
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; NOTE: This is during 'init.el'!
;;
;; 'init.el' setup happens in two distinct stages:
;;   1. 'init'
;;   2. 'config'
;;   3. 'finalize'
;;
;; This is the third and final part of that two stage process.
;;
;; After this, Emacs will run wild and free, like the majestic GNU/wildebeest.
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
;; Hopefully there are no GNU-compitable crocodiles nearby...
(imp:provide :core 'boot 'finalize)

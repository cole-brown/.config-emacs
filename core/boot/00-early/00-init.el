;;; core/boot/00-early/00-init.el --- Early Init: Load Stuff -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <code@brown.dev>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2022-10-24
;; Timestamp:  2023-06-20
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; NOTE: This is during 'early-init.el'!
;;
;; Load all our 'early-init.el' settings and set-up and stuff
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Load Files
;;------------------------------------------------------------------------------

(imp:timing
    '(:core boot early)
    (imp:file:current)
    (imp:path:current:dir)

  (let ((path (imp:path:current:dir/relative :core)))
    (imp:load :feature  '(:core boot early bootstrap)
              :path     (imp:path:current:dir/relative :core)
              :filename "01-bootstrap.el")

     (imp:load :feature  '(:core boot early settings)
              :path     (imp:path:current:dir/relative :core)
              :filename "20-settings.el")

     (imp:load :feature  '(:core boot early package)
              :path     (imp:path:current:dir/relative :core)
              :filename "80-package.el")))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :core 'boot 'early)

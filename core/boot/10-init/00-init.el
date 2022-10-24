;;; 00-init.el --- normal init stuff -*- lexical-binding: t; -*-
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
;; normal init settings and set-up and stuff
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Load Files
;;------------------------------------------------------------------------------

(imp:timing
    '(:core boot init)
    (imp:file:current)
    (imp:path:current:dir)

  (let ((path (imp:path:current:dir/relative :core)))
    (imp:load :feature  '(:core boot init bootstrap)
              :path     (imp:path:current:dir/relative :core)
              :filename "01-bootstrap.el")

    (imp:load :feature  '(:core boot init settings)
              :path     (imp:path:current:dir/relative :core)
              :filename "10-settings.el")

    (imp:load :feature  '(:core boot init mantle)
              :path     (imp:path:current:dir/relative :core)
              :filename "20-mantle.el")))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :core 'boot 'init)

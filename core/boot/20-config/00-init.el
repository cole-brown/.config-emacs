;;; 00-init.el --- configure stuff -*- lexical-binding: t; -*-
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
;; normal init's configuration & set-up steps
;;
;; Importantly, this is where user's init/config in 'mantle' is called.
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Load Files
;;------------------------------------------------------------------------------

(imp:timing
    '(:core boot config)
    (imp:file:current)
    (imp:path:current:dir)

  (let ((path (imp:path:current:dir/relative :core)))
    (imp:load :feature  '(:core boot config system)
              :path     (imp:path:current:dir/relative :core)
              :filename "10-system.el")

    (imp:load :feature  '(:core boot config mantle)
              :path     (imp:path:current:dir/relative :core)
              :filename "50-mantle.el")))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :core 'boot 'config)

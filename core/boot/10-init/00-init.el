;;; core/boot/10-init/00-init.el --- Normal Init: Load Stuff -*- lexical-binding: t; -*-
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
;; NOTE: This is during 'init.el'!
;;
;; 'init.el' setup happens in two distinct stages:
;;   1. 'init'
;;   2. 'config'
;;   3. 'finalize'
;;
;; All of the 'init' runs stage happens before any 'config' runs, in order to
;; give ourselves and the user a way to orginize any rough dependencies.
;;
;; Basically, initialize some things so everyone's all agreed that we're
;; definitely ready to configure all the things with `use-package' and such.
;;
;; Importantly, this is where user's init (aka 'mantle/init') is called.
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

    (imp:load :feature  '(:core boot init packages)
              :path     (imp:path:current:dir/relative :core)
              :filename "20-packages.el")

    (imp:load :feature  '(:core boot init mantle)
              :path     (imp:path:current:dir/relative :core)
              :filename "80-mantle.el")))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :core 'boot 'init)

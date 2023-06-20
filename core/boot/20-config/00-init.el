;;; core/boot/20-config/00-init.el --- Normal Init: Load Stuff -*- lexical-binding: t; -*-
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
;; All of the 'init' runs stage happens before any 'config' runs, in order to
;; give ourselves and the user a way to orginize any rough dependencies.
;;
;; Basically, initialize some things so everyone's all agreed that we're
;; definitely ready to configure all the things with `use-package' and such.
;;
;; Importantly, this is where user's config (aka 'mantle/config') is called.
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

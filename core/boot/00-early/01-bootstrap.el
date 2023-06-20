;;; core/boot/00-early/01-bootstrap.el --- Early Init: Bootstrap -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2022-03-23
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
;; Bootstrapping for 'early-init.el', which is bootstrapping for 'init.el'...
;; Sorta... bootstraps all the way down.
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Custom Variables
;;------------------------------------------------------------------------------
;; Create a group for any custom variables that will be created?


;;------------------------------------------------------------------------------
;; Bootstrap
;;------------------------------------------------------------------------------

;;------------------------------
;; [Speed]: Garbage Collection
;;------------------------------

;; A big contributor to startup times is garbage collection. We up the gc
;; threshold to temporarily prevent it from running, then reset it later by
;; enabling `gcmh-mode'. Not resetting it will cause stuttering/freezes.
;; See GCMH: https://github.com/emacsmirror/gcmh
(setq gc-cons-threshold most-positive-fixnum)


;;------------------------------
;; [Init]: Packages
;;------------------------------

;; In Emacs 27+, package initialization occurs before `user-init-file' is
;; loaded, but after `early-init-file'. So if we want to handle package init
;; ourselves, set this:
;; (setq package-enable-at-startup nil)


;;------------------------------------------------------------------------------
;; The End
;;------------------------------------------------------------------------------
(imp:provide :core 'boot 'early 'bootstrap)

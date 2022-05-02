;;; core/boot/00-early/00-bootstrap.el -*- lexical-binding: t; -*-


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
(imp:provide :dot-emacs 'core 'boot '00-early 'bootstrap)

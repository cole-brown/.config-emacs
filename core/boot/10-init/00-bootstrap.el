;;; 00-bootstrap.el --- Super early stuff requried for "all" of init. -*- lexical-binding: t; -*-
;;
;; Author: Cole Brown <code@brown.dev>
;; URL:    https://github.com/cole-brown/.config-emacs
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;; Code:


;;------------------------------------------------------------------------------
;; Bootstrap
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; Required Packages & Modules
;;------------------------------------------------------------------------------
;; Load a few "must exist ASAP" things that we'll use for the rest of Emacs
;; start-up.


;;------------------------------
;; `imp'
;;------------------------------
(load (expand-file-name "imp/init.el" init:path:core/modules))

;; From here on, `imp' should be used instead of `load', `require', etc.


;;------------------------------
;; `no-littering'
;;------------------------------
;; TODO: this


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :dot-emacs 'core 'boot '10-init 'bootstrap)

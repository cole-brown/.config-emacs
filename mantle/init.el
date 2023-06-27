;;; mantle/init.el --- User-Level Emacs Initialization -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2022-05-04
;; Timestamp:  2023-06-27
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; User-Level Emacs Initialization
;;
;; Just the bare minimum init or things that must happen before configuring
;; Emacs & packages. The user's `use-package', settings, etc. should happen
;; under 'mantle/config.el' instead.
;;
;;; Code:


;;------------------------------------------------------------------------------
;; User Inits: Run in This Order
;;------------------------------------------------------------------------------

;;------------------------------
;; [EARLY] Prereq Packages: Dash, s.el...
;;------------------------------

(imp:timing
    '(:mantle packages early)
    (imp:file:current)
    (imp:path:current:dir)

  ;; https://github.com/magnars/dash.el
  (imp:use-package dash
    ;; Make sure this loads ASAP. It's used for init/config of things just after this.
    :demand t

    ;;--------------------
    :config
    ;;--------------------

    ;;---
    ;; Fontification of Special Variables
    ;;---
    ;; Font lock of special Dash variables (it, acc, etc.) in Emacs Lisp buffers
    ;; can optionally be enabled with the autoloaded minor mode
    ;; `dash-fontify-mode'.
    ;;
    ;; Automatically enable the minor mode in all Emacs Lisp buffers:
    (global-dash-fontify-mode)

    ;;---
    ;; Info Symbol Lookup
    ;;---
    ;; While editing Elisp files, you can use C-h S (info-lookup-symbol) to look
    ;; up Elisp symbols in the relevant Info manuals (see (emacs) Info Lookup).
    ;;
    ;; Enable this for Dash symbols:
    (with-eval-after-load 'info-look
      (dash-register-info-lookup))))


;;------------------------------
;; User Init: Utils, etc for use in rest of `mantle'.
;;------------------------------
;; Give user a folder & files to do with whatever they want.
(imp:load :feature  '(:mantle init user)
          :path     (imp:path:join innit:path:mantle "init")
          :filename "init")


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'init 'base)

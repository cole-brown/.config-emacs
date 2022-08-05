;;; mantle/config/dev-env.el --- Development Environment -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2022-07-28
;; Modified:   2022-08-05
;; URL:        https://github.com/cole-brown/.config-emacs
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;  Development Environment
;;
;;; Code:


(imp:require :keybind)


;;------------------------------------------------------------------------------
;; Emacs Lisp
;;------------------------------------------------------------------------------
;; TODO:
;;   1. Pull out into "elisp.el"? Or probably better: "dev-env/elisp.el"?
;;   2. In which case rename "dev-env.el" to "dev-env/common.el"?
;;   3. And I guess have a "dev-env/init.el"?

;; Provides a very helpful elisp macro debugging tool: `macrostep-expand'
(imp:use-package macrostep

  ;;--------------------
  :general
  ;;--------------------

  ;; Bind to `emacs-lisp' local leader
  (:prefix  (keybind:leader :local "")
   :states  keybind:leader/local:states
   :keymaps keybind:leader/local:keymaps

   ;;---
   ;; Macroexpand Commands
   ;;---
   "m" '(macrostep-expand :which-key "Expand Macro")))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'config 'user 'dev-env 'emacs-lisp)

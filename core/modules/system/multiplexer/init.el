;; system/multiplexer/init.el -*- mode: emacs-lisp; lexical-binding: t -*-
;;
;; Author:   Cole Brown <code@brown.dev>
;; URL:      https://github.com/cole-brown/.config-emacs
;; Created:  2020-08-28
;; Modified: 2022-06-05
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Allow multiple systems (computers) to use the same init with small
;; differences.
;;
;;; Code:


;;------------------Init & Config Help for Multiple Systems.--------------------
;;--                     What computer is this anyways?                       --
;;--------------------------(probably the wrong one)----------------------------


;;------------------------------------------------------------------------------
;; Set up custom vars.
;;------------------------------------------------------------------------------

(defgroup system:multiplexer:group nil
  "Group namespace for the `:system/multiplexer' defcustoms."
  :prefix "system:multiplexer:"
  :group 'environment)


;;------------------------------------------------------------------------------
;; Load our files.
;;------------------------------------------------------------------------------

(imp:timing
    '(:system multiplexer)
    (imp:file:current)
    (imp:path:current:dir)

  ;; Debugging!
  (imp:load :feature  '(:system multiplexer debug)
            :path     (imp:path:current:dir/relative :system)
            :filename "debug")
  ;; Initialize debugging before going any further.
  (int<system/multiplexer>:nub:init)

  ;; Set-up Jerky namespaces for systems.
  (imp:load :feature  '(:system multiplexer namespaces)
            :path     (imp:path:current:dir/relative :system)
            :filename "namespaces")

  ;; Multiple systems (computers) able to use this same init.
  (imp:load :feature  '(:system multiplexer multiplex)
            :path     (imp:path:current:dir/relative :system)
            :filename "multiplex")

  ;; Directory Local Variables
  (imp:load :feature  '(:system multiplexer dlv)
            :path     (imp:path:current:dir/relative :system)
            :filename "dlv"))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :system 'multiplexer)

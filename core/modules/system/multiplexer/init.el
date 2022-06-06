;; -*- mode: emacs-lisp; lexical-binding: t -*-
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
;; Set-Up.
;;------------------------------------------------------------------------------

(imp:path:root :system
               (imp:path:join (imp:path:current:dir) ".."))


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
            :filename "debug")
  ;; Initialize debugging before going any further.
  (int<system/multiplexer>:nub:init)

  ;; Set-up Jerky namespaces for systems.
  (imp:load :feature  '(:system multiplexer namespaces)
            :filename "namespaces")

  ;; Multiple systems (computers) able to use this same init.
  (imp:load :feature  '(:system multiplexer multiplex)
            :filename "multiplex")

  ;; Directory Local Variables
  (imp:load :feature  '(:system multiplexer dlv)
            :filename "dlv"))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :system 'multiplexer)

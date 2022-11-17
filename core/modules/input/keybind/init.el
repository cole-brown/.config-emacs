;;; modules/input/keybind/init.el --- Bind Keys to Your Will -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2022-07-21
;; Modified:   2022-07-21
;; URL:        https://github.com/cole-brown/.config-emacs
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Bind Keys to Your Will
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Set imp Root.
;;------------------------------------------------------------------------------

(imp:path:root :keybind
               (imp:path:current:dir)
               "init.el")


;;------------------------------------------------------------------------------
;; Load Files
;;------------------------------------------------------------------------------

(imp:timing
    '(:keybind)
    (imp:file:current)
    (imp:path:current:dir)

  ;;----------------------------------------------------------------------------
  ;; Standard Keybind Stuff
  ;;----------------------------------------------------------------------------


  ;;----------------------------------------------------------------------------
  ;; `general' Dependencies
  ;;----------------------------------------------------------------------------
  ;; All this stuff needs `general' and won't be loaded until after it has been.
  ;; So if you use it, guard your code like so:
  ;;   (imp:eval:after (:keybind general ready)
  ;;     ...)
  ;; Or if you don't use `imp' for loading, the Emacs feature symbol is:
  ;;  (imp:feature:normalize:imp->emacs :keybind 'general 'ready)
  ;;    -> `keybind:general:ready'
  (imp:load :feature  '(:keybind general)
            :path     "general"
            :filename "init")

  ;; End load timing.
  )


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide:with-emacs :keybind)

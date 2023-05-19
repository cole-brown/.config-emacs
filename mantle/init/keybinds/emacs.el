;;; mantle/init/keybinds/emacs.el --- Keybinds for Emacs in General -*- lexical-binding: t; -*-
;;
;; Author: Cole Brown <code@brown.dev>
;; URL:    https://github.com/cole-brown/.config-emacs
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;  Keybinds for Emacs in General
;;
;;  ...Not like... `general' the keybinds package.
;;  Just in general. Like for not any particular thing better covered elsewhere.
;;
;;; Code:


(imp:eval:after bind-key

  ;;------------------------------------------------------------------------------
  ;; `special-mode'
  ;;------------------------------------------------------------------------------

  (bind-keys :map special-mode-map
             ;; "q" is `quit-window' by default, which is "leave this buffer but don't delete it".
             ;; Add a keybind for "delete this buffer (or maybe just leave it)".
             ("k" . window:kill-or-quit))


  ;;------------------------------------------------------------------------------
  ;; `imp-timing-mode'
  ;;------------------------------------------------------------------------------

  ;; ;; TODO:imp: Does `imp-timing-mode' need this bind explicitly, or did it inherit it?
  ;; ;; If it doesn't just have it after start up (no going out of & back into `imp-timing-mode'), then explicitly bind it:
  ;;            ;; "q" is `quit-window' by default, which is "leave this buffer but don't delete it".
  ;;            ;; Add a keybind for "delete this buffer (or maybe just leave it)".
  ;;            ("k" . window:kill-or-quit))
  )


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'init 'keybinds 'emacs)

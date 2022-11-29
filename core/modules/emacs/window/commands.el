;;; core/modules/emacs/window/commands.el --- Window Commands -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2022-11-29
;; Modified:   2022-11-29
;; URL:        https://github.com/cole-brown/.config-emacs
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Window Commands
;;
;;; Code:


(require 'window)


(imp:require :elisp 'utils 'predicates)


;;------------------------------------------------------------------------------
;; Cycle Buffers
;;------------------------------------------------------------------------------

(defun window:cmd:buffer:previous ()
  "Switch to current window's previous buffer."
  (interactive)
  (if (elisp:evil?)
      (evil-switch-to-windows-last-buffer)
    (previous-buffer)))



(defun window:cmd:buffer:next ()
  "Switch to current window's next buffer."
  (interactive)
  ;; Annnnd... `evil' doesn't have a mirror of
  ;; `evil-switch-to-windows-last-buffer'... so... ok then?
  (next-buffer))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :window 'commands)

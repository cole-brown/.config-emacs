;;; manage.el --- Manage windows: kill, quit, etc. -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2022-07-15
;; Modified:   2022-07-15
;; URL:        https://github.com/cole-brown/.config-emacs
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Manage windows: kill, quit, etc..
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Kill Functions
;;------------------------------------------------------------------------------
;; See `:buffer:manage' for similar functions.
;;   git-root://core/modules/emacs/buffer/manage.el

;; Like `kill-buffer-ask' but no confirmation for unmodified buffers.
(defun window:kill-or-quit (&optional quit window)
  "Default to kill, will quit instead on non-nil prefix arg (QUIT).

The inverse of `quit-window' / `window:quit-or-kill' - defaults to kill, will
quit (bury) on prefix arg QUIT.

WINDOW must be a live window and defaults to the current/selected one."
  (interactive "P")
  (quit-restore-window window (if quit 'bury 'kill)))
;; (window:kill-or-quit)
;; (window:kill-or-quit t)


(defun window:quit-or-kill (&optional kill window)
  "Default to quit, will kill instead on non-nil prefix arg (KILL).

The inverse of `window:kill-or-quit' - defaults to quit (bury), will
kill on prefix arg KILL.

WINDOW must be a live window and defaults to the current/selected one."
  (interactive "P")
  (quit-window kill window))
;; (window:quit-or-kill)
;; (window:quit-or-kill t)


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :window 'manage)

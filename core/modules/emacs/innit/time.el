;;; time.el --- Bout that time, innit? -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2022-04-26
;; Modified:   2022-04-26
;; URL:        https://github.com/cole-brown/.config-emacs
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;  Bout that time, innit?
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Customs, Constants, & Variables
;;------------------------------------------------------------------------------

(defcustom innit:time:benchmark? t
  "Boolean flag for enabling/disabling Emacs start-up benchmark message."
  :group 'innit:group
  :type  '(boolean))


(defvar innit:time nil
  "The time it took, in seconds, for Emacs & `innit' to finish set-up.")


;;------------------------------------------------------------------------------
;; Functions
;;------------------------------------------------------------------------------

(defun innit:time:benchmark (&optional string?)
  "Display a benchmark including number of packages and modules loaded.

If STRING? is non-nil, return the message as a string instead of displaying it."
  ;; If string is desired instead of message, ignore `innit:time:benchmark?' flag.
  (unless (and (not innit:time:benchmark?)
               (not string?))
    (let ((time (or innit:time
                    (setq innit:time
                          (float-time (time-subtract (current-time) before-init-time)))))
          (features/imp (when (functionp #'imp:feature:count)
                          (imp:feature:count)))
          (features/emacs (length features))
          message)

      (setq message
            (mapconcat #'identity
                       (-filter #'stringp
                                (list "innit:"
                                      (format "       init time: %8.03fs" time)
                                      (when features/imp
                                        (format "    imp features: %4d" features/imp))
                                      (format "  emacs features: %4d" features/emacs)))
                       "\n"))

      ;; Where to output?
      (if string?
          message
        (message message)))))
;; (innit:time:benchmark)



;;------------------------------------------------------------------------------
;; Set-Up
;;------------------------------------------------------------------------------

(defun innit:time:init ()
  "Set up innit/Emacs start-up benchmark hook."
  ;; Always set up the hook, it will decide when run if it should do anything.
  (add-hook 'window-setup-hook #'innit:time:benchmark))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :innit 'time)

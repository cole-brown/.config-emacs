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

    ;; TODO: A correct count for innit loads?
    ;;   - Will be more complicated than this...
    ;;   - There's stuff that `innit' loaded that may not have any `provide'.
    ;;     - May be fixing this?
    ;;   - There's stuff `imp' loaded that is only in `imp'.
    ;;   - There's stuff `imp' loaded that is also in Emacs' feature list.
    ;; (funcall (if string? #'format #'message)
    ;;          "`innit' loaded %d packages across %d modules in %.03fs"
    ;;          (- (length load-path) (length (get 'load-path 'initial-value)))
    ;;          (if doom-modules (hash-table-count doom-modules) 0)
    ;;          (or innit:time
    ;;              (setq innit:time
    ;;                    (float-time (time-subtract (current-time) before-init-time)))))
    (funcall (if string? #'format #'message)
             "`innit' loaded x things across y places in %.03fs"
             (or innit:time
                 (setq innit:time
                       (float-time (time-subtract (current-time) before-init-time)))))))
;; (innit:hook:benchmark)


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

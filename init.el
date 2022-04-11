;;; init.el --- Normal Init. -*- lexical-binding: t; -*-
;;
;; Author: Cole Brown <code@brown.dev>
;; URL:    https://github.com/cole-brown/.config-emacs
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;; Code:

(innit:debug:hack "[innit] init.el: hello?")


;;------------------------------------------------------------------------------
;; Sanity Check: Has "early-init" run?
;;------------------------------------------------------------------------------
;; If a check fails, error out of init.

;; 1) Does Emacs pass the minimum version check?
(when (version< emacs-version "28.1") ;; Emacs should be 28.1 or newer.
  (error "[ERROR] 'init.el': Failed 'core/boot' init. Emacs should be version 28.1 or newer, running Emacs %S"
         emacs-version))

;; 2) Do we have our `innit` functions?
(unless (or (fboundp 'innit:status:get)
            (functionp 'innit:status:get))
  (error "[ERROR] 'init.el': 'early-init.el' did not run? `innit:status:get' function is not defined"))

;; 3) Do we have a successful status for early-init?
(unless (innit:status:get "00-early")
  (error "[ERROR] 'init.el': 'early-init.el' failed 'core/boot' init. `innit:status' for '00-early' is: %S\ninnit:statuses:\n%S"
         (innit:status:get "00-early")
         (pp-to-string innit:status)))


;;------------------------------------------------------------------------------
;; Load Ordered Init Directories
;;------------------------------------------------------------------------------


;; This was done in "early-init.el":
;;   (innit:load "early-init.el" "00-early")
;; Now we need to load the rest:
(let ((caller "init.el")
      progress-reporter)

  ;;------------------------------
  ;; Init
  ;;------------------------------
  ;; All the "must happen so Emacs can be set-up" stuff.
  ;;   - Load & set-up packages that must go early, like:
  ;;     - `imp' - file loading library
  ;;     - `no-littering' - direct other Emacs packages to not litter the `user-emacs-directory'.
  ;;     - etc
  (setq progress-reporter (make-progress-reporter (format "[innit] %s: %s..."
                                                          caller
                                                          "10-init")
                                                  0
                                                  100))
  (innit:load caller "10-init")
  (progress-reporter-done progress-reporter)


  ;;------------------------------
  ;; Config
  ;;------------------------------
  ;; Standard Emacs set-up.
  (setq progress-reporter (make-progress-reporter (format "[innit] %s: %s..."
                                                          caller
                                                          "10-init")
                                                  0
                                                  100))
  (innit:load caller "20-config")
  (progress-reporter-done progress-reporter)


  ;;------------------------------
  ;; Finalize
  ;;------------------------------
  ;; Last chance to do things.
  (setq progress-reporter (make-progress-reporter (format "[innit] %s: %s..."
                                                          caller
                                                          "10-init")
                                                  0
                                                  100))
  (innit:load caller "99-finalize")
  (progress-reporter-done progress-reporter))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :dot-emacs 'init)

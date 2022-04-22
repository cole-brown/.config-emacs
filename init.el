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

(let ((file/this (imp:file:current))
      progress-reporter)
  (nub:out :innit
           :debug
           file/this
           "Sanity Checks, Etc.")

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
  ;; Now we need to load the rest.


  ;;------------------------------
  ;; Init
  ;;------------------------------
  ;; All the "must happen so Emacs can be set-up" stuff.
  ;;   - Load & set-up packages that must go early, like:
  ;;     - `imp' - file loading library
  ;;     - `no-littering' - direct other Emacs packages to not litter the `user-emacs-directory'.
  ;;     - etc

  (nub:out :innit
           :debug
           file/this
           "Boot Loader: 10 Early")
  (setq progress-reporter (make-progress-reporter (format "[innit] %s: %s..."
                                                          file/this
                                                          "10-init")
                                                  0
                                                  100))
  (innit:load file/this "10-init")
  (progress-reporter-done progress-reporter)


  ;;------------------------------
  ;; Config
  ;;------------------------------
  ;; Standard Emacs set-up.
  (nub:out :innit
           :debug
           file/this
           "Boot Loader: 20 Config")
  (setq progress-reporter (make-progress-reporter (format "[innit] %s: %s..."
                                                          file/this
                                                          "20-config")
                                                  0
                                                  100))
  (innit:load file/this "20-config")
  (progress-reporter-done progress-reporter)


  ;;------------------------------
  ;; Finalize
  ;;------------------------------
  ;; Last chance to do things.
  (nub:out :innit
           :debug
           file/this
           "Boot Loader: 99 Finalize")
  (setq progress-reporter (make-progress-reporter (format "[innit] %s: %s..."
                                                          file/this
                                                          "99-finalize")
                                                  0
                                                  100))
  (innit:load file/this "99-finalize")
  (progress-reporter-done progress-reporter)


  ;;------------------------------------------------------------------------------
  ;; The End.
  ;;------------------------------------------------------------------------------
  (nub:out :innit
           :debug
           file/this
           '("Boot Loader: 100 Done"
             :newline
             "Now witness the firepower of this full armed and operational operating system.")))
(imp:provide :dot-emacs 'init)

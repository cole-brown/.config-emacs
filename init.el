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

;;------------------------------------------------------------------------------
;; Sanity Check: Has "early-init" run?
;;------------------------------------------------------------------------------
;; If a check fails, error out of init.

;; 1) Does Emacs pass the minimum version check?
(unless (version< emacs-version "28.0") ;; Emacs should be 28.1 or newer.
  (error "[ERROR] 'init.el': Failed 'core/boot' init. Emacs should be version 28.1 or newer, running Emacs %S"
         emacs-version))

;; 2) Do we have our `init` functions?
(unless (or (fboundp 'init:status:get)
            (functionp 'init:status:get))
  (error "[ERROR] 'init.el': 'early-init.el' did not run? `init:status:get' function is not defined"))

;; 3) Do we have a successful status for early-init?
(unless (init:status:get "00-early")
  (error "[ERROR] 'init.el': 'early-init.el' failed 'core/boot' init. `init:status' for '00-early' is: %S"
         (init:status:get "00-early")))


;;------------------------------------------------------------------------------
;; Load Ordered Init Directories
;;------------------------------------------------------------------------------


;; This was done in "early-init.el":
;;   (init:load "early-init.el" "00-early")
;; Now we need to load the rest:
(let ((caller "init.el"))
  ;;------------------------------
  ;; Init
  ;;------------------------------
  ;; All the "must happen so Emacs can be set-up" stuff.
  ;;   - Load & set-up packages that must go early, like:
  ;;     - `imp' - file loading library
  ;;     - `no-littering' - direct other Emacs packages to not litter the `user-emacs-directory'.
  ;;     - etc
  (init:load caller "10-init")


  ;;------------------------------
  ;; Config
  ;;------------------------------
  ;; Standard Emacs set-up.
  (init:load caller "20-config")


  ;;------------------------------
  ;; Finalize
  ;;------------------------------
  ;; Last chance to do things.
  (init:load caller "99-finalize"))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :dot-emacs 'init)

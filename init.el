;;; init.el --- Normal Init -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2023-06-20
;; Timestamp:  2023-06-20
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; 1. Verify basic requirements:
;;    - Minimum Emacs version.
;;    - Ensure 'early-init.el' has run.
;;    - etc.
;;
;; 2. Run through the 'core/boot/...' folders in sequential order.
;;    - These are where init actually happens.
;;
;;; Code:

(let ((file/this (imp:file:current))
      (tags/this '(:innit :standard))
      progress-reporter)
  (nub:debug
      :innit
      file/this
      tags/this
    "Sanity Checks, Etc.")

  ;;------------------------------------------------------------------------------
  ;; Sanity Check: Has "early-init" run?
  ;;------------------------------------------------------------------------------
  ;; If a check fails, error out of init.

  ;; 1) Does Emacs pass the minimum version check?
  (when (version< emacs-version "28.1") ;; Emacs should be 28.1 or newer.
    (error "[ERROR] 'init.el': Failed 'core/boot' init. Emacs should be version 28.1 or newer, running Emacs %S"
           emacs-version))

  ;; 2) Do we have our `imp` functions?
  (unless (or (fboundp 'imp:feature?)
              (functionp 'imp:feature?))
    (error "[ERROR] 'init.el': 'early-init.el' did not run? `imp:feature?' function is not defined"))

  ;; 3) Do we have the early-init feature? If not, we never finished loading "early-init.el".
  (unless (imp:feature? :root 'init 'early)
    (error (concat "[ERROR] 'init.el': 'early-init.el' failed to provide imp feature `%S'. "
                   "Check `imp:cmd:features:print' for whatever it did manage to provide?")
           '(:root init early)))


  ;;------------------------------------------------------------------------------
  ;; Load Ordered Init Directories
  ;;------------------------------------------------------------------------------

  ;;------------------------------
  ;; Early Init
  ;;------------------------------
  ;; "early-init.el" loaded the "00-early" init directory files.
  ;; Now we need to load the rest.


  ;;------------------------------
  ;; Init
  ;;------------------------------
  ;; All the "must happen so Emacs can be set-up" stuff.
  ;;   - Load & set-up packages that must go early, like:
  ;;     - `imp' - file loading library
  ;;     - `no-littering' - direct other Emacs packages to not litter the `user-emacs-directory'.
  ;;     - etc

  (nub:debug
      :innit
      file/this
      tags/this
    "Boot Loader: 10 Early")
  (setq progress-reporter (make-progress-reporter (format "[innit] %s: %s..."
                                                          file/this
                                                          "10-init")
                                                  0
                                                  100))
  (imp:load :feature  '(:core boot init)
            :path     (imp:path:join innit:path:core/boot "10-init")
            :filename "00-init.el")
  (progress-reporter-done progress-reporter)


  ;;------------------------------
  ;; Config
  ;;------------------------------
  ;; Standard Emacs set-up.
  (nub:debug
      :innit
      file/this
      tags/this
    "Boot Loader: 20 Config")
  (setq progress-reporter (make-progress-reporter (format "[innit] %s: %s..."
                                                          file/this
                                                          "20-config")
                                                  0
                                                  100))
  (imp:load :feature  '(:core boot config)
            :path     (imp:path:join innit:path:core/boot "20-config")
            :filename "00-init.el")
  (progress-reporter-done progress-reporter)


  ;;------------------------------
  ;; Finalize
  ;;------------------------------
  ;; Last chance to do things.
  (nub:debug
      :innit
      file/this
      tags/this
    "Boot Loader: 99 Finalize")
  (setq progress-reporter (make-progress-reporter (format "[innit] %s: %s..."
                                                          file/this
                                                          "99-finalize")
                                                  0
                                                  100))
  (imp:load :feature  '(:core boot finalize)
            :path     (imp:path:join innit:path:core/boot "99-finalize")
            :filename "00-init.el")
  (progress-reporter-done progress-reporter)


  ;;------------------------------------------------------------------------------
  ;; The End is Nigh!
  ;;------------------------------------------------------------------------------
  (nub:debug
      :innit
      file/this
      tags/this
    '("Boot Loader: 100 Done"
      :line:new
      "Now witness the firepower of this full armed and operational operating system.")))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :root 'init 'standard)

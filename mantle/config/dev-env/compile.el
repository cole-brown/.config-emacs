;;; mantle/config/compile.el --- Build Your Things -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2023-01-26
;; Modified:   2023-01-26
;; URL:        https://github.com/cole-brown/.config-emacs
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Build Your Things
;;
;;; Code:


(imp:require :jerky)


;;------------------------------------------------------------------------------
;; Compile
;;------------------------------------------------------------------------------

(imp:use-package compile

  ;;------------------------------
  :init
  ;;------------------------------

  ;;---
  ;; Create a hook for enabling this minor mode.
  ;;---
  ;; Creates a func called `mantle:hook:rainbow-mode/enable'.
  ;;   (innit:hook:func/name:symbol "rainbow-mode/enable" nil)
  (innit:hook:defun
      (:name "compile:ansi-color:enable"
       :docstr (mapconcat #'identity
                          '("Apply ANSI codes (color) in compilation buffers."
                            ""
                            "Meant for `compilation-filter-hook'."
                            ""
                            "Originally from Doom's `doom-apply-ansi-color-to-compilation-buffer-h'"
                            "in \"core/autoload/ui.el\".")
                          "\n"))
    (with-silent-modifications
      (ansi-color-apply-on-region compilation-filter-start (point))))


  ;;------------------------------
  :hook
  ;;------------------------------

  ((compilation-filter-hook . mantle:hook:compile:ansi-color:enable)
   (compilation-filter-hook . comint-truncate-buffer))


  ;;------------------------------
  :custom
  ;;------------------------------

  (compilation-always-kill t)       ; kill compilation process before starting another
  (compilation-ask-about-save nil)  ; save all buffers on `compile' without asking
  (compilation-scroll-output 'first-error)


  ;;------------------------------
  :config
  ;;------------------------------

  ;; Automatically truncate compilation buffers so they don't accumulate too
  ;; much data and bog down the rest of Emacs.
  (autoload 'comint-truncate-buffer "comint" nil t))


;;--------------------------------------------------------------------------------
;; Keybinds : Meow
;;--------------------------------------------------------------------------------

(imp:use-package compile
  :when  (imp:flag? :keybinds +meow)

  ;;------------------------------
  :init
  ;;------------------------------

  (defcustom mantle:meow/key:local/compile "c"
    "Keybind for buffer's 'compile...' menu of commands, if any.

Example: `terraform-mode' can use this to create its compile commands for
`terraform apply' et al. See \"./terraform.el\" for details."
    :group 'innit:group
    :type '(string)))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'config 'user 'dev-env 'compile)

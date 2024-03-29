;;; mantle/config/dev-env/toml.el --- Tom's Obvious, Minimal Language -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2023-01-03
;; Timestamp:  2023-06-29
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Tom's Obvious, Minimal Language
;;
;; We needed another markup language, alright? YAML didn't quite cut it.
;;
;;; Code:


;;------------------------------------------------------------------------------
;; TOML Mode
;;------------------------------------------------------------------------------

;; https://github.com/dryman/toml-mode.el
(imp:use-package toml-mode
  :mode "/Pipfile\\'"

  ;;------------------------------
  :init
  ;;------------------------------

  (innit:hook:defun
      (:name   'toml:settings
       :docstr "Settings for TOML mode. Non-LSP stuff.")

    (setq fill-column (jerky:get 'fill-column 'wide))

    ;; TODO: Disable highlighting of long lines in whitespace-mode?

    (setq tab-width (jerky:get 'code 'tab 'standard)))


  ;;------------------------------
  :hook
  ;;------------------------------
  (toml-mode-hook . mantle:hook:toml:settings))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'config 'dev-env 'languages 'toml)

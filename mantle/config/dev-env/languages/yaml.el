;;; mantle/config/dev-env/yaml.el --- Yet Another Markup Language -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2023-01-03
;; Timestamp:  2023-06-28
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;  Yet Another Markup Language
;;
;;; Code:


;;------------------------------------------------------------------------------
;; YAML Mode
;;------------------------------------------------------------------------------

(imp:use-package yaml-mode
  ;;------------------------------
  :init
  ;;------------------------------

  (innit:hook:defun
      (:name   'yaml:settings
       :docstr "Settings for YAML mode. Non-LSP stuff.")

    (setq fill-column (jerky:get 'fill-column 'wide))

    ;; TODO: Disable highlighting of long lines in whitespace-mode?

    ;; TODO: Does YAML use `tab-width'? Looks like not...
    ;; ;; Use smaller indents than is standard for code.
    ;; (setq tab-width yaml-indent-offset)
    )


  ;;------------------------------
  :hook
  ;;------------------------------
  (yaml-mode-hook . mantle:hook:yaml:settings)


  ;; ;;------------------------------
  ;; :custom
  ;; ;;------------------------------
  ;;
  ;; `yaml-indent-offset' defaults to 2, so don't really need to set it to `(code tab short)'.
  ;; ;; Use smaller indents than is standard for code.
  ;; (yaml-indent-offset (jerky:get 'code 'tab 'short))
  )


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'config 'user 'dev-env 'languages 'yaml)

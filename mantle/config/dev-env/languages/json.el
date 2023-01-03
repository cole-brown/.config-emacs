;;; mantle/config/dev-env/json.el --- JSON... JSON everywhere. -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2023-01-03
;; Modified:   2023-01-03
;; URL:        https://github.com/cole-brown/.config-emacs
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;  JSON... JSON everywhere.
;;
;;; Code:


(imp:require :keybind)


;;------------------------------------------------------------------------------
;; JSON Mode
;;------------------------------------------------------------------------------

;; https://github.com/joshwnj/json-mode
(imp:use-package json-mode
  :mode "\\.js\\(?:on\\|[hl]int\\(?:rc\\)?\\)\\'"

  ;;------------------------------
  :init
  ;;------------------------------

  (innit:hook:defun
      (:name   'json:settings
       :file   macro<imp>:path/file
       :docstr "Settings for JSON mode. Non-LSP stuff.")

    (setq fill-column (jerky:get 'fill-column 'wide))

    ;; TODO: Disable highlighting of long lines in whitespace-mode?

    (setq tab-width (jerky:get 'code 'tab 'standard)))


  ;;------------------------------
  :hook
  ;;------------------------------
  (json-mode-hook . mantle:hook:json:settings)


  ;;------------------------------
  :general
  ;;------------------------------
  (:prefix  (keybind:prefix :local)
   :states  keybind:leader/local:states
   :keymaps (list 'json-mode-map keybind:leader/local:keymaps)

   "p" (list #'json-mode-show-path :which-key "Copy JSON Path")
   "d" (list #'json-mode-kill-path :which-key "Kill JSON Path")
   "t" #'json-toggle-boolean
   "x" #'json-nullify-sexp
   "+" #'json-increment-number-at-point
   "-" #'json-decrement-number-at-point
   "f" #'json-mode-beautify))


;; TODO: Does `json-mode-show-path' not work? Or what does it do?
;; ;;------------------------------------------------------------------------------
;; ;; JSON Paths
;; ;;------------------------------------------------------------------------------
;;
;; ;; https://github.com/Sterlingg/json-snatcher
;; (imp:use-package json-snatcher
;;   ;;------------------------------
;;   :general
;;   ;;------------------------------
;;   (:prefix  (keybind:prefix :local)
;;    :states  keybind:leader/local:states
;;    :keymaps (list 'json-mode-map keybind:leader/local:keymaps)
;;
;;    ;; TODO: Replace `json-mode-show-path'?
;;    "p" (list #'jsons-print-path :which-key "JSON Path")))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'config 'user 'dev-env 'json)

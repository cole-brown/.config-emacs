;;; mantle/config/dev-env/json.el --- JSON... JSON everywhere. -*- lexical-binding: t; -*-
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
;;  JSON... JSON everywhere.
;;
;;; Code:


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
       :docstr "Settings for JSON mode. Non-LSP stuff.")

    (setq fill-column (jerky:get 'fill-column 'wide))

    ;; TODO: Disable highlighting of long lines in whitespace-mode?

    (setq tab-width (jerky:get 'code 'tab 'standard)))


  ;;------------------------------
  :hook
  ;;------------------------------
  ((json-mode-hook . mantle:hook:json:settings)
   (json-mode-hook . rainbow-delimiters-mode)))


;;------------------------------
;; Keybinds : Meow
;;------------------------------

(imp:use-package json-mode
  :when  (imp:flag? :keybinds +meow)
  :after meow

  ;;------------------------------
  :config
  ;;------------------------------
  ;;------------------------------
  ;; `General'
  ;;------------------------------

  (defun mantle:meow/keybind/general:elisp-mode ()
    "Create the `elisp-mode' keybinds in `general' for `meow'."
    (keybind:meow:leader/local:bind-keys
        'json-mode-map
      "p" (list #'json-mode-show-path :which-key "Copy JSON Path")
      "d" (list #'json-mode-kill-path :which-key "Kill JSON Path")
      "t" #'json-toggle-boolean
      "x" #'json-nullify-sexp
      "+" #'json-increment-number-at-point
      "-" #'json-decrement-number-at-point
      "f" #'json-mode-beautify))

  ;;------------------------------
  ;; `Transient'
  ;;------------------------------

  (defun mantle:meow/keybind/transient:elisp-mode ()
    "Create the `elisp-mode' keybinds in `transient' for `meow'."
    (mantle:meow:leader/local:keys json-mode-map
                                   "p" #'json-mode-show-path ; Copy JSON Path
                                   "d" #'json-mode-kill-path ; Kill JSON Path
                                   "t" #'json-toggle-boolean
                                   "x" #'json-nullify-sexp
                                   "+" #'json-increment-number-at-point
                                   "-" #'json-decrement-number-at-point
                                   "f" #'json-mode-beautify))


  ;;------------------------------
  ;; Actually Create Keybinds:
  ;;------------------------------

  (if (imp:provided? :keybinds 'user 'general 'meow)
      (mantle:meow/keybind/general:elisp-mode)
    (mantle:meow/keybind/transient:elisp-mode)))


;;------------------------------
;; Keybinds : Evil
;;------------------------------

(imp:use-package json-mode
  :when  (imp:flag? :keybinds +evil)
  :after (:and evil evil-collection)

  ;;------------------------------
  :general ; evil
  ;;------------------------------
  (:prefix  (keybind:prefix :local)
   :states  keybind:leader/local:states
   :keymaps 'json-mode-map

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
;; (imp:use-package json-snatcher)
;;
;;
;; ;;------------------------------
;; ;; Keybinds : Meow
;; ;;------------------------------
;;
;; (imp:use-package json-snatcher
;;   :when  (imp:flag? :keybinds +meow)
;;   :after meow
;;
;;   ;;------------------------------
;;   :config
;;   ;;------------------------------
;;
;;   ;; TODO: Replace `json-mode-show-path' with this?
;;   (mantle:meow:leader/local:keys json-mode-map
;;                                  "p" #'jsons-print-path ; JSON Path
;;                                  ))
;;
;;
;;;; ;;------------------------------
;; ;; Keybinds : Evil
;; ;;------------------------------
;;
;; (imp:use-package json-snatcher
;;   :when  (imp:flag? :keybinds +evil)
;;   :after (:and evil evil-collection)
;;
;;   ;;------------------------------
;;   :general ; evil
;;   ;;------------------------------
;;   (:prefix  (keybind:prefix :local)
;;    :states  keybind:leader/local:states
;;    :keymaps 'json-mode-map
;;
;;    ;; TODO: Replace `json-mode-show-path'?
;;    "p" (list #'jsons-print-path :which-key "JSON Path")))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'config 'dev-env 'languages 'json)

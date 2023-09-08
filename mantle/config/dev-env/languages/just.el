;;; mantle/config/dev-env/just.el --- Project-Specific Command Runner -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2023-01-03
;; Timestamp:  2023-09-08
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Project-Specific Command Runner
;;
;; `just' like `make' except for commands in general, and also a heapful of
;; lessons learned, hopefully.
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Just Some Syntax Highlighting
;;------------------------------------------------------------------------------

(imp:use-package just-mode
  ;;------------------------------
  :init
  ;;------------------------------

  (innit:hook:defun
      (:name   'just:settings
       :docstr "Settings for `just-mode'. Non-LSP stuff.")

    (setq fill-column (jerky:get 'fill-column 'wide))

    ;; TODO: Disable highlighting of long lines in whitespace-mode?
    )


  ;;------------------------------
  :hook
  ;;------------------------------
  (just-mode-hook . mantle:hook:just:settings))


;;--------------------------------------------------------------------------------
;; Just Some Recipe Runner
;;--------------------------------------------------------------------------------

(use-package justl

  ;;------------------------------
  :custom
  ;;------------------------------

  ;; Recipe Name Field (default: 20)
  (justl-recipe-width 30))


;; TODO: Keybinds?
;;   - `justl'
;;   - `justl-exec-recipe-in-dir'
;;   - `justl-exec-recipe'
;;   - Possibly more; docs aren't great...
;;
;;------------------------------
;; Keybinds : Meow
;;------------------------------

(use-package justl
  :when  (imp:flag? :keybinds +meow)
  :after meow

  ;;------------------------------
  :config
  ;;------------------------------

  (keybind:leader/global:def
    :infix (keybind:infix "d j") ; "dev-env" -> "just"
    "" '(nil :which-key "Just Recipe Runner...") ; infix title

    "j" (list #'justl-exec-recipe-in-dir :which-key "Just run a recipe...")
    "o" (list #'justl                    :which-key "Open `just` buffer...")
    "r" (list #'justl-recompile          :which-key "Rerun last `just` recipe")))



;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'config 'dev-env 'languages 'yaml)

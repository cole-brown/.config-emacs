;;; mantle/config/dev-env.el --- Development Environment -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2022-07-28
;; Modified:   2022-07-28
;; URL:        https://github.com/cole-brown/.config-emacs
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;  Development Environment
;;
;;; Code:


(imp:require :keybind)


;;------------------------------------------------------------------------------
;; Color Codes
;;------------------------------------------------------------------------------

;; Set the background color of color strings to the color they represent.
;; e.g.:
;;   - #ff1100
;;   - #abc
;;   - rgb(100, 100, 100)
;;   - YellowGreen
;;   - etc
;; https://github.com/emacsmirror/rainbow-mode/blob/master/rainbow-mode.el
;; https://elpa.gnu.org/packages/rainbow-mode.html
(imp:use-package rainbow-mode

  ;;--------------------
  :init
  ;;--------------------

  ;;---
  ;; Create a hook for enabling this minor mode.
  ;;---
  ;; Creates a func called `mantle:hook:rainbow-mode/enable'.
  ;;   (innit:hook:func/name:symbol "rainbow-mode/enable" nil)
  (innit:hook:defun
      (list :name "rainbow-mode/enable"
            :file (path:current:file)
            :docstr "Enable `rainbow-mode' (colorize color codes) for this buffer."
            :quiet t)
    (rainbow-mode +1))

  ;;--------------------
  :hook
  ;;--------------------

  ((org-mode    . mantle:hook:rainbow-mode/enable)
   (csharp-mode . mantle:hook:rainbow-mode/enable)
   (css-mode    . mantle:hook:rainbow-mode/enable)
   (php-mode    . mantle:hook:rainbow-mode/enable)
   (html-mode   . mantle:hook:rainbow-mode/enable)))


;;------------------------------------------------------------------------------
;; Emacs Lisp
;;------------------------------------------------------------------------------
;; TODO:
;;   1. Pull out into "elisp.el"? Or probably better: "dev-env/elisp.el"?
;;   2. In which case rename "dev-env.el" to "dev-env/common.el"?
;;   3. And I guess have a "dev-env/init.el"?

;; Provides a very helpful elisp macro debugging tool: `macrostep-expand'
(imp:use-package macrostep

  ;;--------------------
  :general
  ;;--------------------

  ;; Bind to `emacs-lisp' local leader
  (:prefix  (keybind:leader :local "")
   :states  keybind:leader/local:states
   :keymaps keybind:leader/local:keymaps

   ;;---
   ;; Macroexpand Commands
   ;;---
   "m" '(macrostep-expand :which-key "Expand Macro")))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'config 'user 'dev-env)

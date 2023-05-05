;;; mantle/config/ai.el --- ChatGPT and Friends -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2023-05-02
;; Modified:   2023-05-02
;; URL:        https://github.com/cole-brown/.config-emacs
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;  I, for one, welcome our new AI overlords.
;;
;;; Code:


(imp:require :secret 'key)


;;------------------------------------------------------------------------------
;; ChatGPT
;;------------------------------------------------------------------------------

;; https://github.com/karthink/gptel
(imp:use-package gptel

  ;;------------------------------
  :init
  ;;------------------------------

  (innit:hook:defun
      (:name    "gptel:org:settings"
       :file    macro<imp>:path/file
       :docstr  "Settings for ChatGPT buffers.")
    (visual-line-mode +1))


  ;;------------------------------
  :hook
  ;;------------------------------
  (gptel-mode-hook . mantle:hook:gptel:org:settings)


  ;;------------------------------
  :custom
  ;;------------------------------

  (gptel-api-key (plist-get secret:key:openai :key))

  ;; Default: `markdown-mode' if available, else `text-mode'
  ;; ...why would you ever not use org?
  (gptel-default-mode 'org-mode))


;;------------------------------------------------------------------------------
;; Keybinds : Meow
;;------------------------------------------------------------------------------

(imp:use-package gptel
  :when  (imp:flag? :keybinds +meow)
  :after meow

  ;;------------------------------
  :general
  ;;------------------------------

  (keybind:leader/global:def
    :infix (keybind:infix "u c") ; "apps & stuff" -> ChatGPT
    "" '(nil         :which-key "ChatGPT...")

    "c" '(gptel      :which-key "Start/Switch to ChatGPT buffer...")
    "s" '(gptel-send :which-key "Send Region to ChatGPT...")
    "S" '((elisp:cmd/prefix #'gptel-send :prefix/always) :which-key "ChatGPT Settings...")))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'config 'user 'dev-env 'ai)

;;; mantle/config/dev-env/ai.el --- ChatGPT and Friends -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2023-05-02
;; Timestamp:  2024-10-02
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
       :docstr  "Settings for ChatGPT buffers.")
    (visual-line-mode +1))

  (defvar mantle:gptel:directive/default-with-examples
    (str:join
     " "
     "You are a large language model living in Emacs and a helpful assistant."
     "Respond concisely and provide reference links or code examples.")
    "For adding to alist `gptel-directives' and/or var `gptel--system-message'.")


  ;;------------------------------
  :hook
  ;;------------------------------
  (gptel-mode-hook . mantle:hook:gptel:org:settings)


  ;;------------------------------
  :custom
  ;;------------------------------

  (gptel-api-key (plist-get secret:key:openai :key))

  ;; 4o is much better than 3.5 turbo.
  (gptel-model "gpt-4o")

  ;; `default' doesn't provide examples as often as I want.
  (gptel--system-message mantle:gptel:directive/default-with-examples)

  ;; Default: `markdown-mode' if available, else `text-mode'
  ;; ...why would you ever not use org?
  (gptel-default-mode 'org-mode)


  ;;------------------------------
  :config
  ;;------------------------------

  ;; `default' doesn't provide examples as often as I want.
  (push (cons 'mantle:gptel:directive/default-with-examples
                                mantle:gptel:directive/default-with-examples)
                          gptel-directives))


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
(imp:provide :mantle 'config 'dev-env 'ai)

;;; mantle/config/keybinds/window.el --- Window Keybindings -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2024-04-17
;; Timestamp:  2024-04-17
;;
;;; Commentary:
;;
;;  Window Keybindings
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Keybinds : Meow
;;------------------------------------------------------------------------------

(imp:use-package emacs
  :when  (imp:flag? :keybinds +meow)
  :after  meow

  ;;------------------------------
  :init
  ;;------------------------------

  ;;------------------------------
  ;; `General'
  ;;------------------------------
  (defun mantle:meow/keybind/general:window ()
    "Create the \"Window...\" keybinds in `general' for `meow'."
    (keybind:leader/global:def
      :infix (keybind:infix "w")        ; window
      "" '(nil :which-key "Window...")) ; infix title

    ;;------------------------------
    ;; Movement - ↑←↓→
    ;;------------------------------
    (keybind:leader/global:def
      :infix (keybind:infix "w") ; window

      ;; "ww" for default desired move: "other" window
      "w" (list #'other-window   :which-key "↔ Move: Other")

      ;; ↑←↓→
      "." (list #'windmove-up    :which-key "↑ Move: Up")
      "o" (list #'windmove-left  :which-key "← Move: Left")
      "e" (list #'windmove-down  :which-key "↓ Move: Down")
      "u" (list #'windmove-right :which-key "→ Move: Right"))

    ;;------------------------------
    ;; Split
    ;;------------------------------
    (keybind:leader/global:def
      :infix (keybind:infix "w") ; window

      "v" (list #'split-window-below :which-key "◒ Split: Vertical")
      "h" (list #'split-window-right :which-key "◑ Split: Horizontal")
      ;; "w s" for default desired split: vertical
      "s" (list #'split-window-right :which-key "◑ Split: Horizontal"))

    ;;------------------------------
    ;; Resize
    ;;------------------------------
    ;; In main "Window" shortcuts
    (keybind:leader/global:def
      :infix (keybind:infix "w") ; window

      "=" (list #'balance-windows :which-key "Size: Balance"))

    ;; In "Window -> Resize" shortcuts
    (keybind:leader/global:def
      :infix (keybind:infix "w r")     ; window -> resize
      "" '(nil :which-key "Resize...") ; infix title

      "=" (list #'balance-windows     :which-key "Resize: Balance")

      ;; ↑←↓→
      "." (list #'enlarge-window              :which-key "↑ Resize: Taller")
      "o" (list #'shrink-window-horizontally  :which-key "← Resize: Skinnier")
      "e" (list #'shrink-window               :which-key "↓ Resize: Shorter")
      "u" (list #'enlarge-window-horizontally :which-key "→ Resize: Wider"))

    ;;------------------------------
    ;; Delete
    ;;------------------------------
    (keybind:leader/global:def
      :infix (keybind:infix "w d")     ; window -> delete
      "" '(nil :which-key "Delete...") ; infix title

      ;; "w dd" for default desired delete: this window
      "d" (list #'delete-window        :which-key "Delete: This")
      "o" (list #'delete-other-windows :which-key "Delete: Others")))


  ;;------------------------------
  ;; Actually Create Keybinds:
  ;;------------------------------

  (when (imp:provided? :keybinds 'general 'meow)
    (mantle:meow/keybind/general:window)))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'config 'keybinds 'window)

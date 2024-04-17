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
    ;; Split
    ;;------------------------------
    (keybind:leader/global:def
      :infix (keybind:infix "w" "s")  ; window -> split
      "" '(nil :which-key "Split...") ; infix title

      "h" (list #'split-window-right :which-key "◑ Split: Horizontal ")
      "v" (list #'split-window-below :which-key "◒ Split: Vertical")
      ;; "w ss" for default desired split: vertical
      "s" (list #'split-window-below :which-key "◒ Split: Vertical"))

    ;;------------------------------
    ;; Movement - ↑←↓→
    ;;------------------------------
    (keybind:leader/global:def
      :infix (keybind:infix "w")  ; window

      ;; "ww" for default desired move: "other" window
      "w" (list #'other-window   :which-key "↔ Move: Other")

      ;; ↑←↓→
      "." (list #'windmove-up    :which-key "↑ Move: Up")
      "o" (list #'windmove-left  :which-key "← Move: Left")
      "e" (list #'windmove-down  :which-key "↓ Move: Down")
      "u" (list #'windmove-right :which-key "→ Move: Right"))

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

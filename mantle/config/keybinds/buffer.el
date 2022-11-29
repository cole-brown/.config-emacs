;;; mantle/config/keybinds/buffer.el --- Buffer Keybinds -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2022-11-14
;; Modified:   2022-11-14
;; URL:        https://github.com/cole-brown/.config-emacs
;;
;;; Commentary:
;;
;;  Buffer Keybinds
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Keybinds
;;------------------------------------------------------------------------------

;; TODO: Evil vs Emacs keybinds?

(imp:eval:after (:and evil evil-collection)

  ;;------------------------------
  ;; Buffer
  ;;------------------------------
  (keybind:leader/global:def
   :infix (keybind:infix "b")       ; buffer
   "" '(nil :which-key "Buffer...") ; infix title

   ;;---
   ;; Change Buffers
   ;;---
   ;; TODO: We should have the primary (lowercase) for persp-mode buffer switching via
   ;; `persp-switch-to-buffer', and the alternate for any buffer via `switch-to-buffer'.
   ;; "b" (list #'persp-switch-to-buffer :which-key "Switch Workspace Buffer")
   ;; TODO: But no `persp-mode' yet, so just... both the same?
   "b" (list #'switch-to-buffer    :which-key "Switch Buffer")
   "B" (list #'switch-to-buffer    :which-key "Switch Buffer")

   "k" (list #'kill-current-buffer :which-key "Kill Buffer")

   "z" (list #'bury-buffer         :which-key "Bury Buffer")

   "[" (list #'previous-buffer     :which-key "Previous Buffer")
   "]" (list #'next-buffer         :which-key "Next Buffer")

   "x" (list (elisp:cmd/args #'switch-to-buffer "*scratch*")
             :which-key "Switch to '*scratch*'")

   ;;---
   ;; Indirect Buffers
   ;;---
   "i" (list #'clone-indirect-buffer :which-key "Indirect Buffer")


   ;;---
   ;; Narrow / Widen
   ;;---
   "-" (list #'buffer:cmd:narrow/toggle :which-key "Narrow / Widen")

   ;;---
   ;; Saving & Naming
   ;;---
   "R" (list #'rename-buffer :which-key "Rename Buffer")

   "s" (list #'save-buffer   :which-key "💾 Save"))

  ;;------------------------------
  ;; Bookmarks
  ;;------------------------------
  ;; Not sure if these should be here, in "File..." or in their own keybinds. Doom has
  ;; them in with the "Buffer..." keybinds but you can't set a bookmark on a
  ;; buffer with no backing file/directory so that feels like the wrong place
  ;; other than "b" == "bookmark"?
  ;;
  ;; ...On the other hand, "b" == "bookmark" is a decent reason?
  (keybind:leader/global:def
   :infix (keybind:infix "b") ; buffer

   "m" (list #'bookmark-set    :which-key "Set Bookmark")
   "M" (list #'bookmark-delete :which-key "Delete Bookmark")
   "j" (list #'bookmark-jump   :which-key "Jump to Bookmark")))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'config 'user 'keybinds 'buffer)

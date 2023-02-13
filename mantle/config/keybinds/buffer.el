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

;;------------------------------
;; Meow
;;------------------------------

(imp:use-package emacs
  :when  (imp:flag? :keybinds +meow)
  :after  meow

  ;;------------------------------
  :init
  ;;------------------------------

  (defvar mantle:meow/keymap/leader:buffer
    (let ((map (make-sparse-keymap)))
      ;; TOOD-meow: this
      ;; (define-key map "/" #'deadgrep) ; "`rg' @ project root"
      ;; (define-key map "." #'mantle:user:deadgrep:default-directory) ; "`rg' @ default-directory")
      ;; ;; TODO: A deadgrep search that lets me choose the starting dir?
      ;; ;; (define-key map "?" #'mantle:user:deadgrep:default-directory) ; "`rg' @...")
      ;; (define-key map "k" #'mantle:user:deadgrep:buffer:kill) ; "Kill All 'deadgrep' Buffers"

      map)
    "Keymap for `deadgrep' commands that should be available globally.")

  ;; ;;------------------------------
  ;; :config
  ;; ;;------------------------------
  ;; TOOD-meow: this

  )


;;------------------------------
;; Evil
;;------------------------------

(imp:eval:after (:and evil evil-collection)

  ;;------------------------------
  ;; Buffer
  ;;------------------------------
  (keybind:leader/global:def
   :infix "b"                       ; buffer
   "" '(nil :which-key "Buffer...") ; infix title

   ;;---
   ;; Change Buffers
   ;;---
   ;; NOTE: The primary (lowercase) keybind for buffer switching should be
   ;; perspective/workspace/whatever-aware. It should be replaced in a
   ;; `use-package' for e.g. `persp-mode'.
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
   :infix "b"                 ; buffer

   "m" (list #'bookmark-set    :which-key "Set Bookmark")
   "M" (list #'bookmark-delete :which-key "Delete Bookmark")
   "j" (list #'bookmark-jump   :which-key "Jump to Bookmark")))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'config 'user 'keybinds 'buffer)

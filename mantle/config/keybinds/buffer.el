;;; mantle/config/keybinds/buffer.el --- Buffer Keybinds -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2022-11-14
;; Timestamp:  2023-06-29
;;
;;; Commentary:
;;
;;  Buffer Keybinds
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

  (defalias 'mantle:meow/transient:buffer:switch-to/scratch
    (elisp:cmd/args #'switch-to-buffer "*scratch*")
    "Command to switch buffer to '*scratch*'.")

  ;;------------------------------
  ;; `General'
  ;;------------------------------

  (defun mantle:meow/keybind/general:buffer ()
    "Create the \"Buffer...\" keybinds in `general' for `meow'."
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
                :which-key "'*scratch*' Buffer")

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

    ;;---
    ;; Bookmarks
    ;;---
    ;; Not sure if these should be here, in "File...", or in their own keybinds. Doom has
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


  ;;------------------------------
  ;; `Transient'
  ;;------------------------------

  (defun mantle:meow/keybind/transient:buffer ()
    "Create the \"Buffer...\" keybinds in `transient' for `meow'."
    ;;---
    ;; Create Transient
    ;;---
    (transient-define-prefix mantle:meow/transient:buffer ()
      "Buffer commands that should be available globally."
      ["Buffer..."
       ["Change Buffers"
        ;; NOTE: The primary (lowercase) keybind for buffer switching should be
        ;; perspective/workspace/whatever-aware. It should be replaced in a
        ;; `use-package' for e.g. `persp-mode'.
        ("b" "Switch Buffer"   switch-to-buffer)
        ("B" "Switch Buffer"   switch-to-buffer)

        ("k" "Kill Buffer"     kill-current-buffer)

        ("z" "Bury Buffer"     bury-buffer)

        ("[" "Previous Buffer" previous-buffer)
        ("]" "Next Buffer"     next-buffer)

        ("x" "Switch to '*scratch*'" mantle:meow/transient:buffer:switch-to/scratch)]

       ["Indirect Buffers"
        ("i" "Indirect Buffer" clone-indirect-buffer)]

       ["Narrow / Widen"
        ("-" "Narrow / Widen" buffer:cmd:narrow/toggle)]

       ["Saving & Naming"
        ("R" "Rename Buffer" rename-buffer)
        ("s" "💾 Save"      save-buffer)]]

      ;; Not sure if these should be here, in "File...", or in their own keybinds. Doom has
      ;; them in with the "Buffer..." keybinds but you can't set a bookmark on a
      ;; buffer with no backing file/directory so that feels like the wrong place
      ;; other than "b" == "bookmark"?
      ;;
      ;; ...On the other hand, "b" == "bookmark" is a decent reason?
      ["Bookmarks..."
       ("m" "Set Bookmark"     bookmark-set)
       ("M" "Delete Bookmark"  bookmark-delete)
       ("j" "Jump to Bookmark" bookmark-jump)])

    ;;---
    ;; Bind Transient
    ;;---

    ;; TODO-meow: Better at `SPC b' or at `b'? `b' already taken in certain mode maps...
    ;; "SPC b [...]" ; :which-key "Buffer..."
    (meow-leader-define-key
     '("b" . mantle:meow/transient:buffer)) ; :which-key "Buffer..."

    ;; "b [...]"
    (meow-normal-define-key
     '("b" . mantle:meow/transient:buffer))
    ;; (mantle:meow/transient:buffer)
    )


  ;;------------------------------
  ;; Actually Create Keybinds:
  ;;------------------------------

  (if (imp:provided? :keybinds 'general 'meow)
      (mantle:meow/keybind/general:buffer)
    (mantle:meow/keybind/transient:buffer)))


;;------------------------------------------------------------------------------
;; Keybinds : Evil
;;------------------------------------------------------------------------------

(imp:use-package emacs
  :when  (imp:flag? :keybinds +evil)
  :after (:and evil evil-collection)

  ;;------------------------------
  :general ; evil
  ;;------------------------------

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
  ;; Not sure if these should be here, in "File...", or in their own keybinds. Doom has
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
(imp:provide :mantle 'config 'keybinds 'buffer)

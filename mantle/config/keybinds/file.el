;;; file.el --- Files & Dirs Keybindings -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2022-11-14
;; Modified:   2022-11-14
;; URL:        https://github.com/cole-brown/.config-emacs
;;
;;; Commentary:
;;
;;  Files & Dirs Keybindings
;;
;;; Code:


(require 'hydra)
(require 'dired)


(imp:require :path)
(imp:require :elisp 'utils 'functions)
(imp:require :system)
(imp:require :secret)


;;------------------------------------------------------------------------------
;; Keybinds
;;------------------------------------------------------------------------------

;; TODO: Evil vs Emacs keybinds?

(imp:eval:after (:and evil evil-collection)
  ;; NOTE: /mantle/config/keyboard.el has the new general defs

  ;;------------------------------
  ;; Copy
  ;;------------------------------
  (keybind:leader/global:def
   :infix (keybind:infix "f y")          ; file -> yank
   "" '(nil :which-key "Yank / Copy...") ; infix title

   ;;---
   ;; Buffer / File
   ;;---
   "C" (list #'file:cmd:copy/this-buffer-file :which-key "File: Copy")

   ;;---
   ;; Names / Paths
   ;;---
   "y" (list #'path:cmd:buffer:copy:absolute :which-key "Path: Copy")
   "Y" (list (elisp:cmd/prefix #'path:cmd:buffer:copy:absolute '(4)) ;; Call with simulated C-u prefix arg.
             :which-key "Path: Copy Parent")

   "r" (list #'path:cmd:buffer:copy:relative :which-key "Path, Relative: Copy")
   "R" (list (elisp:cmd/prefix #'path:cmd:buffer:copy:relative '(4)) ;; Call with simulated C-u prefix arg.
             :which-key "Path, Relative: Copy Parent"))

  ;;------------------------------
  ;; File / Path
  ;;------------------------------
  (keybind:leader/global:def
   :infix (keybind:infix "f") ; file

   "d" (list #'path:cmd:dired :which-key "Find Directory")
   "f" (list #'file:cmd:find  :which-key "Find File")
   ;; "F" (list #'file:cmd:find :which-key "Find File Under Here")

   "r" (list
        ;; Using `consult'; could use others (e.g. `consel-recentf').
        (cond ((functionp #'consult-recent-file)
               #'consult-recent-file)
              ((and (bound-and-true-p ivy-mode)
                    (functionp #'counsel-recentf))
               #'counsel-recentf)
              ;; Default: No keybind?
              (t
               nil))
        :which-key "Recent Files")

   "R" (list #'file:cmd:move/this      :which-key "Rename/Move This File")
   "X" (list #'file:cmd:delete         :which-key "File: Delete")

   "s" (list #'save-buffer             :which-key "Save")
   "S" (list #'write-file              :which-key "Save As...")

   "u" (list #'file:cmd:find/sudo      :which-key "SUDO: Find File")
   "U" (list #'file:cmd:find/sudo/this :which-key "SUDO: This File"))


  ;;------------------------------
  ;; File / Path (Specific)
  ;;------------------------------
  (keybind:leader/global:def
   :infix (keybind:infix "f .")          ; file -> ...dotfiles?
   "" '(nil :which-key "File at...") ; infix title

   ;;---
   ;; Emacs Config
   ;;---
   ;; .emacs.d aka public config
   "e" (list (elisp:cmd/args #'file:cmd:project:find-file user-emacs-directory)
             :which-key "Find file in `.emacs.d'...")
   "E" (list (elisp:cmd/args #'file:cmd:find user-emacs-directory)
             :which-key "Browse `.emacs.d'..."))

   ;; .secret.d aka private config
  (when (system:secret:has)
    (keybind:leader/global:def
     :infix (keybind:infix "f .")          ; file -> ...dotfiles?

     "s" (list (elisp:cmd/args #'file:cmd:project:find-file
                               (system:multiplexer:get :hash 'this
                                                       :key '(path secret emacs)))
               :which-key "Find file in `.secret.d'...")
     "S" (list (elisp:cmd/args #'file:cmd:find
                               (system:multiplexer:get :hash 'this
                                                       :key '(path secret emacs)))
               :which-key "Browse `.secret.d'..."))))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'config 'user 'keybinds 'file)

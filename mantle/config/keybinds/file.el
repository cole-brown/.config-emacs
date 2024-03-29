;;; mantle/config/keybinds/file.el --- Files & Dirs Keybindings -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2022-11-14
;; Timestamp:  2023-06-29
;;
;;; Commentary:
;;
;;  Files & Dirs Keybindings
;;
;;; Code:


(imp:require :path)
(imp:require :elisp 'utils 'functions)
(imp:require :system)
(imp:require :secret)


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
  ;; Lil Helpers
  ;;------------------------------

  (defun mantle:meow/keybind:file/path:file/recent ()
    "Proper function to use to open recent files."
    (interactive)
    (cond ((functionp #'consult-recent-file)
           (funcall-interactively #'consult-recent-file))
          ((and (bound-and-true-p ivy-mode)
                (functionp #'counsel-recentf))
           (funcall-interactively #'counsel-recentf))
          ((featurep 'recentf)
           (funcall-interactively #'recentf-openfiles))
          ;; Default: No keybind?
          (t
           nil)))


  ;;------------------------------
  ;; `General'
  ;;------------------------------
  (defun mantle:meow/keybind/general:file ()
    "Create the \"File...\" keybinds in `general' for `meow'."
    (keybind:leader/global:def
      :infix (keybind:infix "f")      ; file
      "" '(nil :which-key "File...")) ; infix title

    ;;------------------------------
    ;; Copy
    ;;------------------------------
    (keybind:leader/global:def
      :infix (keybind:infix "f" "y")        ; file -> yank
      "" '(nil :which-key "Yank / Copy...") ; infix title

      ;;---
      ;; Buffer / File
      ;;---
      "C" (list #'file:cmd:copy/this-buffer-file :which-key "File: Copy")

      ;;---
      ;; Names / Paths
      ;;---
      "y" (list #'path:cmd:buffer:copy:absolute :which-key "Path: Copy")
      "Y" (list (elisp:cmd/prefix #'path:cmd:buffer:copy:absolute :prefix/always) ;; Call with simulated C-u prefix arg.
                :which-key "Path: Copy Parent")

      "r" (list #'path:cmd:buffer:copy:project :which-key "Path @ Project: Copy")
      "R" (list (elisp:cmd/prefix #'path:cmd:buffer:copy:project :prefix/always) ;; Call with simulated C-u prefix arg.
                :which-key "Path @ Project: Copy Parent"))

    ;;------------------------------
    ;; File / Path
    ;;------------------------------
    (keybind:leader/global:def
      :infix (keybind:infix "f") ; file

      "d" (list #'path:cmd:dired :which-key "Find Directory")
      "f" (list #'file:cmd:find  :which-key "Find File")
      ;; TODO-meow: Fix to correct command or delete?
      ;; "F" (list #'file:cmd:find :which-key "Find File Under Here")

      "r" (list #'mantle:meow/keybind:file/path:file/recent :which-key "Recent Files")

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
      :infix (keybind:infix "f .")      ; file -> "." (means "this dir" in shell-speak)
      "" '(nil :which-key "File at...") ; infix title

      ;;---
      ;; This Project
      ;;---
      "p" (list #'projectile-dired :which-key "Dired: Project Root")

      ;;---
      ;; Emacs Configs
      ;;---
      ;; .emacs.d aka public config
      ;; TODO-meow: Use `consult' (directly) or whatever the proper package is?
      "e" (list (elisp:cmd/args #'file:cmd:project:find-file user-emacs-directory)
                :which-key "Find file in `.emacs.d'...")
      ;; TODO-meow: Use `consult' (directly) or whatever the proper package is?
      "E" (list (elisp:cmd/args #'file:cmd:find user-emacs-directory)
                :which-key "Browse `.emacs.d'..."))

    ;; .secret.d aka private config
    (when (system:secret:has)
      (keybind:leader/global:def
        :infix (keybind:infix "f .") ; Same menu as above; don't add an infix title or you'll delete above.

        ;; TODO-meow: Use `consult' (directly) or whatever the proper package is?
        "s" (list (elisp:cmd/args #'file:cmd:project:find-file
                                  (system:multiplexer:get :hash 'this
                                                          :key '(path secret emacs)))
                  :which-key "Find file in `.secret.d'...")
        ;; TODO-meow: Use `consult' (directly) or whatever the proper package is?
        "S" (list (elisp:cmd/args #'file:cmd:find
                                  (system:multiplexer:get :hash 'this
                                                          :key '(path secret emacs)))
                  :which-key "Browse `.secret.d'..."))))


  ;;------------------------------
  ;; `Transient'
  ;;------------------------------

  (defun mantle:meow/keybind/transient:file ()
    "Create the \"File...\" keybinds in `transient' for `meow'."
    ;;------------------------------
    ;; Copy
    ;;------------------------------

    (defalias 'mantle:meow/transient:file/copy:path/copy/parent/absolute
      (elisp:cmd/prefix #'path:cmd:buffer:copy:absolute :prefix/always)
      "Copy parent's absolute path.")


    (defalias 'mantle:meow/transient:file/copy:path/copy/parent/relative
      (elisp:cmd/prefix #'path:cmd:buffer:copy:project :prefix/always)
      "Copy parent's relative path.")


    (transient-define-prefix mantle:meow/transient:file/copy ()
      "File yank/copy commands that should be available globally."
      ["Copy..."
       ;;---
       ;; Buffer / File
       ;;---
       ["File"
        ("C" "File: Copy" file:cmd:copy/this-buffer-file)]

       ;;---
       ;; Names / Paths
       ;;---
       ["Path"
        ("y" "Path: Copy" path:cmd:buffer:copy:absolute)
        ("Y" "Path: Copy Parent" mantle:meow/transient:file/copy:path/copy/parent/absolute)

        ("r" "Path, Relative: Copy" path:cmd:buffer:copy:project)
        ("R" "Path, Relative: Copy Parent" mantle:meow/transient:file/copy:path/copy/parent/relative)]])


    ;;------------------------------
    ;; File / Path
    ;;------------------------------

    (transient-define-prefix mantle:meow/transient:file/path ()
      "File / path commands that should be available globally."
      [ ;; No title?
       ["File..."
        ;; Using `consult'; could use others (e.g. `consel-recentf').
        ("r" "Recent Files"          mantle:meow/keybind:file/path:file/recent)
        ("R" "Rename/Move This File" file:cmd:move/this)
        ("X" "File: Delete"          file:cmd:delete)
        ("s" "Save"                  save-buffer)
        ("S" "Save As..."            write-file)]

       ["Find..."
        ("d" "Find Directory"  path:cmd:dired)
        ("f" "Find File"       file:cmd:find)
        ;; TODO-meow: Fix to correct command or delete?
        ;; ("F" "Find File Under Here" file:cmd:find)
        ("u" "SUDO: Find File" file:cmd:find/sudo)
        ("U" "SUDO: This File" file:cmd:find/sudo/this)]])
    ;; (mantle:meow/transient:file/path)


    ;;------------------------------
    ;; File / Path At 'specific-place'
    ;;------------------------------
    ;; "SPC f . [...]" ; :which-key "File / Path at..." aka "File at..."

    ;; TODO-meow: Use `consult' (directly) or whatever the proper package is?
    (defalias 'mantle:meow/transient:file/path/at:find-project-file/dot-emacs
      (elisp:cmd/args #'file:cmd:project:find-file user-emacs-directory)
      "Find file in 'user-emacs-directory' via `file:cmd:project:find-file'.")


    ;; TODO-meow: Use `consult' (directly) or whatever the proper package is?
    (defalias 'mantle:meow/transient:file/path/at:find-file/dot-emacs
      (elisp:cmd/args #'file:cmd:find-file user-emacs-directory)
      "Find file in 'user-emacs-directory' via `file:cmd:find-file'.")


    ;; TODO-meow: Use `consult' (directly) or whatever the proper package is?
    (defalias 'mantle:meow/transient:file/path/at:find-project-file/dot-secret
      (elisp:cmd/args #'file:cmd:project:find-file
                      (system:multiplexer:get :hash 'this
                                              :key '(path secret emacs)))
      "Find file in user's secrets emacs repo via `file:cmd:project:find-file'.")


    ;; TODO-meow: Use `consult' (directly) or whatever the proper package is?
    (defalias 'mantle:meow/transient:file/path/at:find-file/dot-secret
      (elisp:cmd/args #'file:cmd:find-file
                      (system:multiplexer:get :hash 'this
                                              :key '(path secret emacs)))
      "Find file in user's secrets emacs repo via `find-file'.")


    (transient-define-prefix mantle:meow/transient:file/path/at ()
      "File-at / path-at commands that should be available globally."
      ["File/Path ___"
       ["in this project"
        ("p" "Dired: Project Root" projectile-dired)]

       ["in Emacs Configs" ; .emacs.d aka public config
        ("e" "Find file in `.emacs.d'..." mantle:meow/transient:file/path/at:find-project-file/dot-emacs)
        ("E" "Browse `.emacs.d'..."       mantle:meow/transient:file/path/at:find-file/dot-emacs)]])
    ;; (mantle:meow/transient:file/path/at)

    ;;---
    ;; .secret.d aka private config
    ;;---
    (when (system:secret:has)
      (transient-append-suffix 'mantle:meow/transient:file/path/at
        '(0 -1) ; Append after last group/suffix in the first group.
        ["in Secrets config"
         ("s" "Find file in `.secret.d'..." mantle:meow/transient:file/path/at:find-project-file/dot-secret)
         ("S" "Browse `.secret.d'..."       mantle:meow/transient:file/path/at:find-file/dot-secret)]))


    ;;------------------------------
    ;; Entrypoint
    ;;------------------------------

    (transient-define-prefix mantle:meow/transient:file ()
      "File commands that should be available globally."
      ["Files & Paths..."
       ("f" "File / Path..."    mantle:meow/transient:file/path)
       ("." "File / Path at..." mantle:meow/transient:file/path/at)
       ("y" "Yank / Copy..."    mantle:meow/transient:file/copy)])

    ;; TODO-meow: Better at `SPC f' or at `f'?
    ;; "SPC f [...]" ; :which-key "File..."
    (meow-leader-define-key
     '("f" . mantle:meow/transient:file)) ; :which-key "File..."

    ;; "f [...]"
    (meow-normal-define-key
     '("f" . mantle:meow/transient:file))) ; :which-key "File..."


  ;;------------------------------
  ;; Actually Create Keybinds:
  ;;------------------------------

  (if (imp:provided? :keybinds 'general 'meow)
      (mantle:meow/keybind/general:file)
    (mantle:meow/keybind/transient:file)))



;;------------------------------------------------------------------------------
;; Keybinds : Evil
;;------------------------------------------------------------------------------

(imp:eval:after (:and evil evil-collection)

  ;;------------------------------
  ;; Copy
  ;;------------------------------
  (keybind:leader/global:def
   :infix (keybind:infix "f" "y")        ; file -> yank
   "" '(nil :which-key "Yank / Copy...") ; infix title

   ;;---
   ;; Buffer / File
   ;;---
   "C" (list #'file:cmd:copy/this-buffer-file :which-key "File: Copy")

   ;;---
   ;; Names / Paths
   ;;---
   "y" (list #'path:cmd:buffer:copy:absolute :which-key "Path: Copy")
   "Y" (list (elisp:cmd/prefix #'path:cmd:buffer:copy:absolute :prefix/always) ;; Call with simulated C-u prefix arg.
             :which-key "Path: Copy Parent")

   "r" (list #'path:cmd:buffer:copy:project :which-key "Path, Relative: Copy")
   "R" (list (elisp:cmd/prefix #'path:cmd:buffer:copy:project :prefix/always) ;; Call with simulated C-u prefix arg.
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
   :infix (keybind:infix "f .")      ; file -> "." (means "this dir" in shell-speak)
   "" '(nil :which-key "File at...") ; infix title

   ;;---
   ;; This Project
   ;;---
   "p" (list #'projectile-dired :which-key "Dired: Project Root")

   ;;---
   ;; Emacs Configs
   ;;---
   ;; .emacs.d aka public config
   "e" (list (elisp:cmd/args #'file:cmd:project:find-file user-emacs-directory)
             :which-key "Find file in `.emacs.d'...")
   "E" (list (elisp:cmd/args #'file:cmd:find user-emacs-directory)
             :which-key "Browse `.emacs.d'..."))

   ;; .secret.d aka private config
  (when (system:secret:has)
    (keybind:leader/global:def
     :infix (keybind:infix "f .") ; Same menu as above; don't add an infix title or you'll delete above.

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
(imp:provide :mantle 'config 'keybinds 'file)

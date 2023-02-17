;;; mantle/config/meow.el --- 'Less is More' Modal Editing -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2022-07-12
;; Modified:   2023-02-09
;; URL:        https://github.com/cole-brown/.config-emacs
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Meow: 'Less is More' Modal Editing
;;
;; https://github.com/meow-edit/meow
;;
;;; Code:


(imp:require :nub)


;;------------------------------------------------------------------------------
;; Meow
;;------------------------------------------------------------------------------

;; Code: https://github.com/meow-edit/meow
;; Docs: https://github.com/meow-edit/meow#documents
(imp:use-package meow
  :when  (imp:flag? :keybinds +meow)

  ;;------------------------------
  :init
  ;;------------------------------

  (defconst mantle:meow:leader/local:prefix '(:emacs "C-c M-m"
                                              :meow  "m")
    "A prefix to bind commands to for Meow \"local\" leader.")


  (defmacro mantle:meow:keymap (symbol docstr &rest keybind)
    "Macro to create a (sparse) keymap named SYMBOL populated with KEYS.

SYMBOL should be an unquoted symbol.

DOCSTR should be a document string for SYMBOL's `defvar'.

Each KEYBIND should be a list of args to pass to `define-key'."
    (declare (indent 2) (doc-string 2))
    `(defvar ,(elisp:unquote symbol)
       (let ((map (make-sparse-keymap)))
         (dolist (each ',keybind)
           (apply #'define-key map (elisp:unquote each)))
         map)
       docstr))
  ;; (mantle:meow:keymap
  ;;     'test:keymap:foo
  ;;     "bar"
  ;;   '("a" ignore)
  ;;   '("b" identity))
  ;; (mantle:meow:keymap
  ;;     test:keymap:foo
  ;;     "bar"
  ;;   ("a" ignore)
  ;;   ("b" identity))


  (defun mantle:meow:leader/local:key (map key func)
    "Bind prefix + KEY to FUNC for Meow \"local \" leader.

MAP should be a keymap or nil(ish). Add to the global keymap if MAP is:
  - nil
  - `global'
  - `:global'

KEY shoud be a string. It will be appended to `mantle:meow:leader/local:prefix'
and then passed through `kbd'.

FUNC should be the function/command to bind."
    (let ((func/name "mantle:meow:leader/local:key/emacs"))

      ;;------------------------------
      ;; Error Checks
      ;;------------------------------
      (unless (or (null map)
                  (eq map :global)
                  (eq map 'global)
                  ;; (keymapp map) ;; Can't check for keymap yet as it's not loaded in `use-package` `:config` section??
                  (symbolp map))
        (nub:error
            :innit
            func/name
          "MAP must be a keymap (or nil for global keymap)! Got %S: %S"
          (type-of map)
          map))

      (unless (stringp key)
        (nub:error
            :innit
            func/name
          "KEY must be a string! Got %S: %S"
          (type-of key)
          key))

      (unless (commandp func)
        (nub:error
            :innit
            func/name
          "FUNC must be a command (interactive function)! Got %S: %S"
          (type-of func)
          func))

      ;;------------------------------
      ;; Create Keybind
      ;;------------------------------
      (if (keymapp map)
          ;; Local keybind.
          (define-key map
            (kbd (concat (plist-get mantle:meow:leader/local:prefix :emacs)
                         key))
            func)

        ;; Global keybind.
        (global-set-key (kbd (concat (plist-get mantle:meow:leader/local:prefix :emacs)
                                     key))
                        func))))


  (defun mantle:meow:leader/local:keys (map &rest keybind)
    "Bind prefix + key to func for Meow \"local \" leader.

MAP should be a keymap or nil(ish). Add to the global keymap if MAP is:
  - nil
  - `global'
  - `:global'

KEYBINDs should be should be a sequence of keys and functions:
  - key      : string fit for `kbd'
  - function : quote function
See `mantle:meow:leader/local:key'."
    (let ((func/name "mantle:meow:leader/local:keys"))
    (unless keybind
        (nub:error
            :innit
            func/name
          "Must have one or more KEYBIND list/cons! Got: %S"
          keybind))
    (let (key)
      (dolist (each keybind)
        ;; Is this the key or the function?
        (if (not key)
            ;; Save key; continue on to next in KEYBINDs for func...
            (setq key each)
          ;; Create the keybind in the map.
          (mantle:meow:leader/local:key map key each)
          (setq key nil))))))


  (defun mantle:meow:leader/local:init ()
    "Create a \"local\" leader entry in Meow for \"mode-specific\" keybinds.

Must be used with `mantle:meow:leader/local:key' or
`mantle:meow:leader/local:keys' to populate the entry created.
Example:
  ;; Bind keys (globally or locally) for \"toggle stuff\" that we want in our Meow
  ;; leader \"toggle stuff\" entry.
  (mantle:meow:leader/local:key 'global
                                \"l\" #'display-line-numbers-mode)
  (mantle:meow:leader/local:key 'markdown-mode-map
                                \"i\" #'markdown-toggle-inline-images)
  (mantle:meow:leader/local:key 'org-mode-map
                                \"i\" #'org-toggle-inline-images)
  ;; Define Meow \"Local\" Leader entrypoint keybind.
  (mantle:meow:leader/local:init)
  ;; Now Meow has a global toggle \"SPC m l\" and two modes have a local toggle
  ;; \"SPC m i\".

From: \"add mode and meow state specific keymaps\"
 │ So my recommendation is to bind keys in vanilla Emacs. For example:
 │
 │ You want a consistent keybinding SPC t i to toggle images in both org-mode and
 │ markdown-mode. You should make a consistent keybinding in vanilla Emacs, and
 │ tell meow to use that.
 │
 │   ;; assume C-x M-t the prefix for 'toggle'
 │   ;; global 'toggle' commands
 │   (global-set-key (kbd \"C-x M-t l\") 'display-line-numbers-mode)
 │   ;; major mode specific 'toggle' commands
 │   (define-key markdown-mode-map (kbd \"C-x M-t i\")
 │                                 #'markdown-toggle-inline-images)
 │   (define-key org-mode-map (kbd \"C-x M-t i\")
 │                                 #'org-toggle-inline-images)
 │   ;; add entry for 'toggle' in leader
 │   (meow-leader-define-key '(\"t\" . \"C-x M-t\"))
 │
 │ Then you have consistent keybinding SPC t i in leader and C-x M-t i in vanilla Emacs.
 │
 └── https://github.com/meow-edit/meow/pull/126#issuecomment-992004368"
    ;; Add entry to the (global) leader.
    (meow-leader-define-key (cons (plist-get mantle:meow:leader/local:prefix :meow)
                                  (plist-get mantle:meow:leader/local:prefix :emacs))))


  ;;------------------------------
  :custom
  ;;------------------------------

  ;; Don't aggregate all actions while in insert state into a single undo action.
  ;; Act like normal Emacs instead.
  (meow-cheatsheet-layout (cond ((imp:flag? :keyboard +dvorak)
                                 meow-cheatsheet-layout-dvorak)
                                ((imp:flag? :keyboard +qwerty)
                                 meow-cheatsheet-layout-qwerty)
                                (t
                                 meow-cheatsheet-layout)))


  ;;------------------------------
  :config
  ;;------------------------------

  ;;---
  ;; Meow "Local" Leader Entries
  ;;---
  (mantle:meow:leader/local:init) ; "SPC m" -> mode/local keybinds

  ;;---
  ;; Engage!
  ;;---
  (meow-mode +1))


;;------------------------------------------------------------------------------
;; NOTE: Re: "add mode and meow state specific keymaps"
;; https://github.com/meow-edit/meow/pull/126#issuecomment-992004368
;;------------------------------------------------------------------------------
;;
;; So my recommendation is to bind keys in vanilla Emacs. For example:
;;
;; You want a consistent keybinding SPC t i to toggle images in both org-mode and
;; markdown-mode. You should make a consistent keybinding in vanilla Emacs, and
;; tell meow to use that.
;;
;;   ;; assume C-x M-t the prefix for 'toggle'
;;   ;; global 'toggle' commands
;;   (global-set-key (kbd "C-x M-t l") 'display-line-numbers-mode)
;;   ;; major mode specific 'toggle' commands
;;   (define-key markdown-mode-map (kbd "C-x M-t i") #'markdown-toggle-inline-images)
;;   (define-key org-mode-map (kbd "C-x M-t i") #'org-toggle-inline-images)
;;   ;; add entry for 'toggle' in leader
;;   (meow-leader-define-key '("t" . "C-x M-t"))
;;
;; Then you have consistent keybinding SPC t i in leader and C-x M-t i in vanilla Emacs.


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'init 'keybinds '+meow)

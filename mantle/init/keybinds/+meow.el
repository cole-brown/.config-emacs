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

  (defconst mantle:meow:leader/local:prefix "C-c M-m"
    "A prefix to bind commands to for Meow \"local\" leader.")


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
                  (keymapp map))
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
            (kbd (concat mantle:meow:leader/local:prefix key))
            func)

        ;; Global keybind.
        (global-set-key (kbd (concat mantle:meow:leader/local:prefix key))
                        func))))


  (defun mantle:meow:leader/local:entry (key)
    "Create a \"local\" leader entry in Meow for \"mode-specific\" keybinds.

Must be used with `mantle:meow:leader/local:key' to populate the entry created.
Example:
  ;; Bind keys (globally or locally) for \"toggle stuff\" that we want in our Meow
  ;; leader \"toggle stuff\" entry.
  (mantle:meow:leader/local:key 'global
                                \"l\" #'display-line-numbers-mode)
  (mantle:meow:leader/local:key 'markdown-mode-map
                                \"i\" #'markdown-toggle-inline-images)
  (mantle:meow:leader/local:key 'org-mode-map
                                \"i\" #'org-toggle-inline-images)
  ;; Define Meow Leader entry for \"toggle stuff\".
  (mantle:meow:leader/local:entry \"t\")
  ;; Now Meow has a global toggle \"SPC t l\" and two modes have a local toggle
  ;; \"SPC t i\".

Re: \"add mode and meow state specific keymaps\"
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
 │   (define-key markdown-mode-map (kbd \"C-x M-t i\") #'markdown-toggle-inline-images)
 │   (define-key org-mode-map (kbd \"C-x M-t i\") #'org-toggle-inline-images)
 │   ;; add entry for 'toggle' in leader
 │   (meow-leader-define-key '(\"t\" . \"C-x M-t\"))
 │
 │ Then you have consistent keybinding SPC t i in leader and C-x M-t i in vanilla Emacs.
 │
 └── https://github.com/meow-edit/meow/pull/126#issuecomment-992004368"
    ;; Add entry to the (global) leader.
    (meow-leader-define-key '(key . (kbd (concat mantle:meow:leader/local:prefix
                                                 key)))))


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

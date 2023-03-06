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

  (defconst mantle:meow:orphan:prefix "C-c M-m M-e M-o M-w"
    "A prefix for commands we don't need/want bound but meow still does.

E.g. Something took `C-k' from `kill-line' but meow still wants `kill-line' for
other things and refuses to refer to it by name? *shrug*")


  ;; TODO-meow: rename this to `key' or `key' and `keys' to `bind'?
  (defmacro mantle:meow:orphan:bind (key func var)
    "Put orphaned keybind FUNC under KEY in our orphan prefix."
    `(let ((key/bind-to (concat mantle:meow:orphan:prefix
                                " "
                                ,key)))
       (global-set-key (kbd key/bind-to)
                       #',func)
       (innit:customize-set-variable ,var key/bind-to)))
  ;; (mantle:meow:orphan:bind "C-k"
  ;;                          kill-line
  ;;                          meow--kbd-kill-line)


  (defconst mantle:meow:leader/local:prefix '(:emacs "C-x M-l" ; -> "SPC l"
                                              ;; :emacs "C-x M-m" ; -> "SPC c m m"
                                              :meow  "l")
    "A prefix to bind commands to for Meow \"local\" leader.

Have to avoid all the \"special\" keys that Meow Keypad state uses.
See: https://github.com/meow-edit/meow/blob/master/TUTORIAL.org#keypad
Example: 'SPC m' actual means 'M-', so you can't actually bind anything
to 'm' in the Meow Leader.")


  (defun mantle:meow:leader/local:key (map key func)
    "Bind prefix + KEY to FUNC for Meow \"local \" leader.

MAP should be a keymap or nil(ish). Add to the global keymap if MAP is:
  - nil
  - `global'
  - `:global'

KEY shoud be a string. It will be appended to `mantle:meow:leader/local:prefix'
and then passed through `kbd'.

FUNC should be the quoted function or unquoted keymap to bind.
Example, Function:
  (mantle:meow:leader/local:key org-mode-map \"x\" #'org-md-export-as-markdown)
Example, Keymap:
  (defvar test:org:keymap
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd \"h\") (lambda () (interactive) (message \"Hello\")))))
  (mantle:meow:leader/local:key org-mode-map \"y\" test:org:keymap)

Note that FUNC will be bound to a /Vanilla Emacs Keybind/! For example:
  (mantle:meow:leader/local:key org-mode-map \"x\" #'org-md-export-as-markdown)
  (mantle:meow:leader/local:key org-mode-map \"x\" #'org-md-export-as-markdown)

This actually technically binds `org-md-export-as-markdown' to \"C-x M-l x\",
and if `mantle:meow:leader/local:init' has been run it will be Meow keybind
\"SPC l x\" (assuming `mantle:meow:leader/local:prefix' hasn't changed from
default)."
    (let ((func/name "mantle:meow:leader/local:key")
          ;; Normalize global MAP to nil.
          (keymap (if (memq map '(:global global))
                      nil
                    map)))

      ;;------------------------------
      ;; Error Checks
      ;;------------------------------
      ;; Check normalized `keymap', report error on original MAP.
      (unless (or (null keymap)
                  (keymapp keymap))
        (nub:error
            :innit
            func/name
          "MAP must be a keymap (or nil/`:global'/`global' for global keymap)! Got %S: %S"
          (type-of map)
          map))

      (unless (stringp key)
        (nub:error
            :innit
            func/name
          "KEY must be a string! Got %S: %S"
          (type-of key)
          key))

      (cond ((commandp func)
             nil)
            ;; Can't use keymaps in `meow' leader? Or, rather, it doesn't like a
            ;; keymap of keymaps? Says "f f is not defined" for "SPC f f" where
            ;; "SPC f" was a keymap and "f" was an entry in that keymap that was
            ;; also a keymap.
            ((keymapp func)
             (nub:error
                 :innit
                 func/name
               "FUNC cannot be a keymap; it must be a command (interactive function)! Got %S: %S"
               (type-of func)
               func))
            (nil
             (nub:error
                 :innit
                 func/name
               "FUNC must be a command (interactive function)! Got %S: %S"
               (type-of func)
               func)))

      ;;------------------------------
      ;; Create Keybind
      ;;------------------------------
      (if (null keymap)
          ;; Global keybind.
          (global-set-key (kbd (concat (plist-get mantle:meow:leader/local:prefix :emacs)
                                       " "
                                       key))
                          func)
        ;; Local keybind.
        (define-key keymap
          (kbd (concat (plist-get mantle:meow:leader/local:prefix :emacs)
                       " "
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
  - function : quote function or unquoted keymap

See `mantle:meow:leader/local:key' for more information."
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
  (mantle:meow:leader/local:key markdown-mode-map
                                \"i\" #'markdown-toggle-inline-images)
  (mantle:meow:leader/local:key org-mode-map
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


  ;; TODO-meow: Patch these into meow itself?
  (defvar mantle:meow/undo:redo/kbd nil
    "String suitable for `kbd' that calls the redo function.")


  ;; TODO-meow: Patch these into meow itself?
  (defun mantle:meow/undo:redo ()
    "Cancel current selection then redo."
    (interactive)
    (when (region-active-p)
      (meow--cancel-selection))
    (meow--execute-kbd-macro mantle:meow/undo:redo/kbd))


  ;; TODO-meow: Patch these into meow itself?
  (defun mantle:meow/undo:redo-in-selection ()
    "Redo in current region."
    (interactive)
    (when (region-active-p)
      (meow--execute-kbd-macro mantle:meow/undo:redo/kbd)))


  ;;------------------------------
  :config
  ;;------------------------------

  ;; `M-x meow-cheatsheet' keyboard layout.
  ;; NOTE: This didn't work in the `:custom' section.
  (setq meow-cheatsheet-layout (cond ((imp:flag? :keyboard +dvorak)
                                      meow-cheatsheet-layout-dvorak)
                                     ((imp:flag? :keyboard +qwerty)
                                      meow-cheatsheet-layout-qwerty)
                                     (t
                                      meow-cheatsheet-layout)))

  ;;---
  ;; Meow "Local" Leader Entries
  ;;---
  (mantle:meow:leader/local:init)

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

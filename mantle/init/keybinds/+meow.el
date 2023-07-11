;;; mantle/init/keybinds/+meow.el --- 'Less is More' Modal Editing -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2022-07-12
;; Timestamp:  2023-06-27
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

  ;;------------------------------
  ;; `meow-visit' and Case sEnSiTiViTy
  ;;------------------------------
  (defcustom mantle:meow:visit/case-fold-search t
    "Should `meow-visit' be case sensitive or not?

nil      - Not case sensitive.
t        - Case sensitive.
:inherit - Leave `case-fold-search' alone!"
    :group 'innit:group
    :type  '(choice (const :tag "Case Insensitive" t)
                    (const :tag "Case Sensitive"   nil)
                    (const :tag "Inherit `case-fold-search' Sensitivity" :inherit)))


  (define-advice meow--visit-point (:override (text reverse) mantle:advice:visit/case-insensitive)
    "Case-insesitive `meow-visit'.

I want `meow-visit' to be optionally case-sensitive, but `meow--visit-point'
forces `case-fold-search' to nil, which forces the search to always be
case-sensitive. See `mantle:meow:visit/case-fold-search' for case-sensitivity
setting.

Return the point of text for visit command.
Argument TEXT current search text.
Argument REVERSE if selection is reversed."
    (let ((func (if reverse #'re-search-backward #'re-search-forward))
          (func-2 (if reverse #'re-search-forward #'re-search-backward))
          (case-fold-search (if (eq mantle:meow:visit/case-fold-search :inherit)
                              case-fold-search
                             mantle:meow:visit/case-fold-search)))
      (save-mark-and-excursion
        (or (funcall func text nil t 1)
            (funcall func-2 text nil t 1)))))


  ;;------------------------------
  ;; "Orphan" Commands: Commands For Only Meow
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


  ;;------------------------------
  ;; Leader Stuff When Using Meow's Leader
  ;;------------------------------

  ;; Don't define these functions at all when not using Meow's leader so that
  ;; errors are even more noticable.
  (unless (imp:provided? :keybinds 'general 'meow)
    (defun mantle:meow:leader/local:key (map key func)
      "Bind prefix + KEY to FUNC for Meow \"local \" leader.

MAP should be a keymap or nil(ish). Add to the global keymap if MAP is:
  - nil
  - `global'
  - `:global'

KEY shoud be a string. It will become `(keybind:prefix :local key)'
and then be passed through `kbd'.

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
\"SPC l x\" (assuming `(keybind:prefix :local)' hasn't changed from
default)."
      (let ((func/name "mantle:meow:leader/local:key")
            (keymap (int<keybind>:keymaps/normalize map))) ; Normalize global MAP to nil.

        ;;------------------------------
        ;; Error Checks
        ;;------------------------------
        ;; TODO:meow:transient: Shouldn't we allow more than one keymap?
        (if (> (length keymap) 1)
            (nub:error
                :innit
                func/name
              "MAP must be a (single) keymap (or nil/`:global'/`global' for global keymap)! Got %S: %S -normalize-> %S"
              (type-of map)
              map
              keymap)
          (setq keymap (nth 0 keymap)))

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

              ;; Can't use (nested) keymaps in `meow' leader? Or, rather, KEYPAD
              ;; mode doesn't like it. Says "f f is not defined" for "SPC f f"
              ;; where "SPC f" was a keymap and "f" was an entry in that keymap
              ;; that was also a keymap.
              ;; KISS: No keymaps allowed at all:
              ;;   ((keymapp func)
              ;;    (nub:error
              ;;        :innit
              ;;        func/name
              ;;      "FUNC cannot be a keymap; it must be a command (interactive function)! Got %S: %S"
              ;;      (type-of func)
              ;;      func))
              ;; Or...: Check keymap, error only if it has a nested keymap.
              ((seq-find (lambda (keymap/entry)
                           (and (listp keymap/entry)
                                (keymapp (cdr keymap/entry))))
                         func)
               (nub:error
                   :innit
                   func/name
                 "FUNC cannot be a keymap containing a nested keymap! Got %S: %S"
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
            (global-set-key (kbd (concat (keybind:leader/local:prefix :meow :emacs)
                                         " "
                                         key))
                            func)
          ;; Local keybind.
          (define-key keymap
            (kbd (concat (keybind:leader/local:prefix :meow :emacs)
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
  ;; Now Meow has a global toggle \"SPC l l\" and two modes have a local toggle
  ;; \"SPC l i\".

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
      (meow-leader-define-key (cons (keybind:prefix :local)
                                    (keybind:leader/local:prefix :meow :emacs)))))


  ;;------------------------------
  ;; Undo & Redo
  ;;------------------------------

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
  ;; Point & Mark
  ;;------------------------------

  (defun mantle:meow-reverse:dwim ()
    "Make `meow-reverse' also do something when a selection isn't active."
    (interactive)
     (if (region-active-p)
         (meow-reverse)
       (exchange-point-and-mark)))


  ;;------------------------------
  :custom
  ;;------------------------------
  ;; See 'meow-vars.el' in "packages/elpa/meow-<version>/meow-var.el".

  ;; Use the system clipboard for `meow-kill', `meow-save', `meow-yank'...
  (meow-use-clipboard t)

  ;; Show meow keypad messages?
  ;; For all the keypad messages:
  ;;   - exit
  ;;   - current key sequence you're building
  ;;   - etc
  ;; (meow-keypad-message nil) ; (default: t)

  ;; Whether to use KEYPAD when the result of executing kbd string is a keymap.
  ;; Well... this could have maybe solved my keymap-inside-of-keymap annoyance
  ;; with KEYPAD, but that's in the past...
  ;; (meow-use-keypad-when-execute-kbd nil) ; (default: t)

  ;; Could get rid of meow special leader translation stuff (maybe?)?
  ;; (meow-keypad-start-keys nil)

  ;; Meow Cursor Types:
  ;; (meow-cursor-type-<...> ???)

  ;; TODO-meow: Currently meow-vars says the integration isn't working so good?
  ;;   "Currently, keypad is not working well with which-key, so Meow ships a
  ;;   default `meow-describe-keymap'."
  ;;     - packages/elpa/meow-20230226.101/meow-var.el
  ;; ;; Can we integrate `which-key' better?
  ;; (meow-keypad-get-title ???)


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
  ;; TODO:meow: anything else in this meow init that needs iffing?
  (unless (imp:provided? :keybinds 'general 'meow)
    (mantle:meow:leader/local:init))

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

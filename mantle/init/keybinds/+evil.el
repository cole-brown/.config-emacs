;;; mantle/init/keybinds/+evil.el --- Emacs Vim Input Layer -*- lexical-binding: t; -*-
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
;; ┌───────────────────────┤ Emacs Vim Input Layer ├───────────────────────┐
;; │                +1.5 Evilness and +7.0 Keybind Confusion               │
;; └───────────────────────┤ (I'm still learning.) ├───────────────────────┘
;;
;; "Emacs Vim Input Layer" doesn't sound all that `evil' at all once you spell
;; it out...
;;
;; This should be entirely optional, since we're swapping over to `meow' to try
;; that modal input package out.
;;
;; https://github.com/emacs-evil/evil
;;
;;; Code:




;;------------------------------------------------------------------------------
;; Evil
;;------------------------------------------------------------------------------

;; Code: https://github.com/emacs-evil/evil
;; Docs: https://evil.readthedocs.io/en/latest/index.html
(imp:use-package evil
  :demand t ;; Always load.

  ;;------------------------------
  ;; https://github.com/emacs-evil/evil#dependencies
  :preface
  ;;------------------------------
  ;; Can't just simply use `:after' since we want to conditionally decide. Rely
  ;; instead on someone setting a flag somewhere (like in the `use-package'
  ;; `:init' section for `undo-tree'.
  (setq evil-undo-system (cond ((imp:flag? :emacs +undo-tree) 'undo-tree)
                               ((imp:flag? :emacs +undo-fu)   'undo-fu)
                               ;; Default to `undo-redo' for Emacs 28+...
                               ((> emacs-major-version 27)    'undo-redo)
                               ;; ...or the pre-Emacs-28 standard undo ring.
                               (t                             nil)))


  ;;------------------------------
  :init
  ;;------------------------------

  ;; Initialize Evil for integrating Evil Collection keybinds.
  ;; NOTE: _MUST_ be set /before/ evil loads!
  (setq evil-want-keybinding  nil
        evil-want-integration t)


  ;;------------------------------
  :custom
  ;;------------------------------

  ;; Don't aggregate all actions while in insert state into a single undo action.
  ;; Act like normal Emacs instead.
  (evil-want-fine-undo t)


  ;;------------------------------
  :config
  ;;------------------------------

  ;; And, of course, be evil in all buffers.
  (evil-mode 1))


;;------------------------------------------------------------------------------
;; Evil Collection (Keybinds of Evilness)
;;------------------------------------------------------------------------------

;; https://github.com/emacs-evil/evil-collection
(imp:use-package evil-collection
  :demand t ;; Always load.
  :after  (evil)

  ;;------------------------------
  :custom
  ;;------------------------------
  ;; https://github.com/emacs-evil/evil-collection#configuration

  ;; Set up Org functions in calendar keymap.
  (evil-collection-calendar-want-org-bindings t)

  ;; TODO: What tab based bindings? And do I want them? The default is nil and Doom doesn't have this variable?
  ;; ;; Enable <tab>-based bindings in Outline mode.
  ;; (evil-collection-outline-bind-tab-p t)

  ;; ;; Do not synchronize insert/normal state with char/line-mode in term-mode.
  ;; (evil-collection-term-sync-state-and-mode-p nil)

  ;; Set up Vim style bindings in the minibuffer.
  (evil-collection-setup-minibuffer t)

  ;; ;; Do not set up debugger keys for certain modes.
  ;; (evil-collection-setup-debugger-keys nil)

  ;; TODO: What "unimpaired bindings"? Doom has this set to nil.
  ;; ;; Set up unimpaired bindings globally.
  ;; (evil-collection-want-unimpaired-p nil)

  ;; ;; Do not bind -find references-, etc to various modes.
  ;; (evil-collection-want-find-usages-bindings nil)

  ;; ;; List of mode specific configurations.
  ;; ;;
  ;; ;; Use to configure specific modes. At the moment, it can be used to defer
  ;; ;; binding keys to those specific modes in order to improve startup time.
  ;; (evil-collection-config '(mode-that-takes-too-long-I-guess ...))


  ;;------------------------------
  :config
  ;;------------------------------

  ;; Register the keybinds all at once.
  ;; NOTE: Could register one-by-one if we want them delayed?
  ;; Example:
  ;;   (with-eval-after-load 'calendar (evil-collection-calendar-setup))
  ;; Or:
  ;;   (with-eval-after-load 'calendar (evil-collection-init 'calendar))
  (evil-collection-init))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'init 'keybinds '+evil)

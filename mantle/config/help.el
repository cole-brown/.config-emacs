;;; mantle/config/help.el --- Which Key & other Helpful things -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2022-07-15
;; Timestamp:  2023-06-28
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;  Which Key & other Helpful things
;;
;;; Code:


(imp:require :window)


;;------------------------------------------------------------------------------
;; General Settings
;;------------------------------------------------------------------------------

;; Always give help window focus?
;; Can set back to t/'always or try 'other if annoying.
;;   - [2019-10-02] sn-002: Changed: t -> nil.
;;   - [2020-10-15] sn-003: Doom also has it as nil.
(innit:customize-set-variable help-window-select nil)


;; Got this buffer a few times and don't know why. Auto-complete help should be
;; amply provided for by other packages/features I have.
;;   - [2019-10-02] sn-002: Can set back to t (needs 1 fail to give help), or try 'lazy (needs 2 fails).
;;   - [2020-10-15] sn-003: Doom doesn't change this from the default, t. Hm...
;;   - [2022-07-15] sn-004: Comment out to start?
;; (innit:customize-set-variable completion-auto-help nil)


;;------------------------------------------------------------------------------
;; Which-Key
;;------------------------------------------------------------------------------

;; Which Key:
;;   https://github.com/justbur/emacs-which-key
;; Which-Key vs Guide-Key:
;;   https://github.com/justbur/emacs-which-key/issues/29
;;
;; Super useful. Shows what all can be done from the buttons you just pressed.
;; E.g. "C-h" and pausing for a sec will bring up which-key with all the
;; commands in the help-command map.
(imp:use-package which-key

  ;;------------------------------
  :custom
  ;;------------------------------

  ;; Sort lower and uppercase keybinds together instead of all upper then all lower.
  ;; And lowercase goes first.
  (which-key-sort-order           'which-key-key-order-alpha)
  (which-key-sort-uppercase-first nil)

  ;; (which-key-side-window-max-width 0.333) ; Default: 0.333
  (which-key-idle-delay 0.5)                 ; Default: 1.0 seconds

  ;; Which-Key & Evil:
  ;;   https://github.com/justbur/emacs-which-key#evil-operators
  ;;   This is done in `:config' so we can check if we have `evil' enabled.

  ;;------------------------------
  :config
  ;;------------------------------

  ;; https://github.com/justbur/emacs-which-key#special-features-and-configuration-options
  ;; Can change where the which-key windows shows up with one of these:
  ;;   (which-key-setup-side-window-bottom) ;; This is the default.
  ;;   (which-key-setup-side-window-right)
  ;;   (which-key-setup-side-window-right-bottom)
  ;; [2022-07-15] Try out right for a while... It's nice that it's one column of
  ;; keys -> binds instead of 1-5 or whatever for the bottom.
  ;; (which-key-setup-side-window-right)
  ;; [2022-07-22] Getting this error with `which-key-setup-side-window-right' if Emacs is small:
  ;;   > which-key can’t show keys: There is not enough space based on your
  ;;   > settings and frame size.
  ;; Try `which-key-setup-side-window-right-bottom' and see if that makes it happier?
  ;; (which-key-setup-side-window-right-bottom)
  ;; If I don't like that, try tweaking these variables:
  ;;   `which-key-side-window-max-height'
  ;;   `which-key-side-window-max-width'
  ;; [2023-04-18] Don't like it...
  ;;   1. Just too far over to look most of the time.
  ;;   2. Doesn't jive with everyone else defaulting to popping up on the bottom.
  ;;   3. Changing buffer widths all the time makes word-wrapped buffers jitter, sometimes.

  (when (imp:feature? 'evil)
    ;; https://github.com/justbur/emacs-which-key#evil-operators
    ;; Evil motions and text objects following an operator like `d' are not all looked up
    ;; in a standard way. Support is controlled through `which-key-allow-evil-operators'
    ;; which should be non-nil if 'evil' is loaded before 'which-key' and through
    ;; `which-key-show-operator-state-maps' which needs to be enabled explicitly because
    ;; it is more of a hack. The former allows for the inner and outer text object maps
    ;; to show, while the latter shows motions as well.
    (setq which-key-allow-evil-operators t
          ;; NOTE: The docstring says this is experimental and Doom doesn't set it.
          ;; So remove if weird?
          which-key-show-operator-state-maps t))

  (which-key-mode +1))


;;------------------------------
;; NOTE: Which-Key: Additional Commands
;;---
;; Commands:
;;   https://github.com/justbur/emacs-which-key#additional-commands
;;
;; `which-key-show-top-level'
;;   - Will show most key bindings without a prefix. It is most and not all,
;;     because many are probably not interesting to most users.
;; `which-key-show-major-mode'
;;   - Will show the currently active major-mode bindings. It’s similar to C-h m
;;     but in a which-key format. It is also aware of evil commands defined
;;     using evil-define-key.
;; `which-key-show-next-page-cycle' / `which-key-show-previous-page-cycle'
;;   - Will flip pages in a circle.
;; `which-key-show-next-page-no-cycle' / `which-key-show-previous-page-no-cycle'
;;   - Will flip pages and stop at first/last page.
;; `which-key-undo'
;;   - Can be used to undo the last keypress when in the middle of a key sequence.
;;------------------------------


;;------------------------------------------------------------------------------
;; Helpful - "A better Emacs *help* buffer"
;;------------------------------------------------------------------------------
;; Replace Emacs' default help bindings/buffer with Helpful's much more helpful
;; help buffer.

;; https://github.com/Wilfred/helpful
(imp:use-package helpful
  ;; After... something?
  ;; :after helm
  ;; :after ivy

  ;; ;;------------------------------
  ;; :custom
  ;; ;;------------------------------

  ;; ;; Default is 5. Can change if desired, but I tried 2 and went back to 5.
  ;; ;; [2019-10-28]: 2->5
  ;; ;; (helpful-max-buffers 5
  ;; ;;   "If this or more, kill oldest when opening another.")


  ;;------------------------------
  :config
  ;;------------------------------

  ;; Set up Ivy to use Helpful:
  (when (imp:feature? 'ivy)
    (setq counsel-describe-function-function #'helpful-callable
          counsel-describe-variable-function #'helpful-variable))

  ;; Set up Helm to use Helpful:
  (when (imp:feature? 'helm)
    (setq helm-describe-function-function 'helpful-callable
          helm-describe-variable-function 'helpful-variable)))


;;------------------------------
;; Keybinds : Emacs, Always
;;------------------------------
;; `C-h [...]' is muscle memory, so we need to allow that still.

(imp:use-package helpful

  ;;------------------------------
  :bind ; emacs
  ;;------------------------------
  ;;---
  ;; global
  ;;---
  (("C-h f" . helpful-callable)
   ("C-h v" . helpful-variable)
   ("C-h k" . helpful-key)
   ("C-h o" . helpful-symbol)
   ;; replaces `finder-by-keyword' but I've never used that...
   ("C-h p" . helpful-at-point)
   ;; replaces `describe-coding-system' but I've never used that...
   ("C-h C" . helpful-command)

   ;;---
   ;; helpful-mode-map
   ;;---
   :map helpful-mode-map
   ;; kill-this-buffer instead of quit (bury)
   ;; TRIAL: [2019-10-28]
   ;; kill-or-quit instead of original quit-or-kill?
   ;;  - "quit" as in bury...
   ("q" . window:kill-or-quit)))


;;------------------------------
;; Keybinds : Meow
;;------------------------------

(imp:use-package helpful
  :when  (imp:flag? :keybinds +meow)
  :after meow

  :general
  ;;------------------------------
  ;; Global Keybinds
  ;;------------------------------
  (keybind:leader/global:def
    "h" '(:keymap help-map :which-key "Help...")))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'config 'user 'help)

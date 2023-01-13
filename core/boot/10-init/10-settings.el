;;; 00-init.el --- Initialize Emacs -*- lexical-binding: t; -*-
;;
;; Author: Cole Brown <code@brown.dev>
;; URL:    https://github.com/cole-brown/.config-emacs
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;; Code:


;;------------------------------------------------------------------------------
;; Optional: Jerky DLV
;;------------------------------------------------------------------------------

;; Add jerky's DLV namespace setter function to the system multiplexer.
;; It will then be called for any multiplexer DLV path/domain registered.
(when (functionp #'jerky:dlv:namespace/set)
  (add-hook 'system:multiplexer:dlv/functions #'jerky:dlv:namespace/set))


;;------------------------------------------------------------------------------
;; Squelch
;;------------------------------------------------------------------------------
;; Silence some functions that are overly chatty.

;; `sh-set-shell' function:
;;   > Setting up indent for shell type bash
;;   > Indentation variables are now local.
;;   > Indentation setup for shell type bash
;; So you end up with "Indentation setup for shell type bash" in the minibuffer
;; at odd times, sometimes. Like opening an org file that I guess has a shell
;; source block maybe?
(advice-add #'sh-set-shell :around #'innit:advice:squelch)


;;------------------------------------------------------------------------------
;; Minibuffer
;;------------------------------------------------------------------------------

;; Allow for minibuffer-ception. Sometimes we need another minibuffer command
;; while we're in the minibuffer.
(innit:customize-set-variable enable-recursive-minibuffers t)

;; Show current key-sequence in minibuffer ala 'set showcmd' in vim. Any
;; feedback after typing is better UX than no feedback at all.
(innit:customize-set-variable echo-keystrokes 0.02)

;; Expand the minibuffer to fit multi-line text displayed in the echo-area. This
;; doesn't look too great with direnv, however...
(innit:customize-set-variable resize-mini-windows 'grow-only)

;; Typing yes/no is obnoxious when y/n will do
(advice-add #'yes-or-no-p :override #'y-or-n-p)

;; Try to keep the cursor out of the read-only portions of the minibuffer.
(innit:customize-set-variable minibuffer-prompt-properties
                              '(read-only         t
                                intangible        t
                                cursor-intangible t
                                face              minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)


;;------------------------------------------------------------------------------
;; Initialize!
;;------------------------------------------------------------------------------
;; TODO: This doesn't go here. This goes in mantle
;; (imp:load :feature  '(:innit systems)
;;           :path     (imp:path:current:dir/relative "...?")
;;           :filename "systems")


;;------------------------------------------------------------------------------
;; Notes (DELETE ME)
;;------------------------------------------------------------------------------

;; TODO: Basically this order:
;; TODO:   1. init this
;; TODO:   2. for each module, init it
;; TODO:   3. for each DOOMDIR, optionally load "init.el".
;; TODO:      - What is DOOMDIR? "init dir"? How is it different from a module?
;; TODO:        - Ah. for secrets etc. So... "SYSTEMDIR", "SECRETDIR", etc.
;; TODO:          1. System-specific
;; TODO:          2. work domain stuff
;; TODO:          3. secrets
;; TODO:          4. etc
;;
;; TODO: Same for config.
;;
;; TODO: Finalize does not do all that - just hooks and stuff.


;; ;; Ensure Doom's core libraries are properly initialized, autoloads file is
;; ;; loaded, and hooks set up for an interactive session.
;; (doom-initialize)
;;
;; ;; Now we load all enabled modules in the order dictated by your `doom!' block
;; ;; in $DOOMDIR/init.el. `doom-initialize-modules' loads them (and hooks) in the
;; ;; given order:
;; ;;
;; ;;   $DOOMDIR/init.el
;; ;;   {$DOOMDIR,~/.emacs.d}/modules/*/*/init.el
;; ;;   `doom-before-init-modules-hook'
;; ;;   {$DOOMDIR,~/.emacs.d}/modules/*/*/config.el
;; ;;   `doom-init-modules-hook'
;; ;;   $DOOMDIR/config.el
;; ;;   `doom-after-init-modules-hook'
;; ;;   `after-init-hook'
;; ;;   `emacs-startup-hook'
;; ;;   `doom-init-ui-hook'
;; ;;   `window-setup-hook'
;; ;;
;; ;; And then we're good to go!
;; (doom-initialize-modules)
;;
;; ;; TODO: split doom-initialize-modules up into explicit stages


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :core 'boot 'init 'settings)

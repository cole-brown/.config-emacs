;;; core/boot/10-init/10-settings.el --- Normal Init: Settings -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2022-03-25
;; Timestamp:  2023-06-20
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Settings for Init
;;   - Directory Local Vars helpers.
;;   - ...
;;   - etc.
;;
;;
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

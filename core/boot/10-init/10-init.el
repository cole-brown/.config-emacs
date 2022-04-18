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
;; Set-Up: `imp'
;;------------------------------------------------------------------------------
;; `imp' is now loaded, so we can set ourselves up to use it.

(imp:path:root :core    innit:path:core/boot)
(imp:path:root :modules (imp:path:join user-emacs-directory "modules"))


;; NOTE: User's list of things to load (the "mantle" layer to our "core") are
;; in "20-load.el".


;;------------------------------------------------------------------------------
;; Initialize!
;;------------------------------------------------------------------------------

;; (imp:load :feature  '(:dot-emacs innit systems)
;;           :path     (imp:path:current:dir/relative :dot-emacs)
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
(imp:provide :dot-emacs 'core 'boot '10-init 'init)

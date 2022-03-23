;;; init.el -*- lexical-binding: t; -*-
;;
;; Author: Cole Brown <code@brown.dev>
;; URL:    https://github.com/cole-brown/.config-emacs
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.


;;------------------------------------------------------------------------------
;; POST:
;;------------------------------------------------------------------------------
;; Power On Self-Test: Has "early-init" run?
;;
;; In the strange case that early-init.el wasn't loaded (e.g. you're using
;; Chemacs 1? Or you're loading this file directly?), we do it explicitly:
(unless (and (boundp 'spy:emacs:version)
             (stringp spy:emacs:version))
  (load (concat (file-name-directory load-file-name) "early-init")
        nil t))


;; TODO: naming this stuff.
;;   - early-init.el with:
;;     - misc stuff
;;     - "Bootstrap"
;;   - init.el with:
;;     - "POST"
;;     - "Bootstrap"
;;     - "Initialize"
;;     - "Configure"
;;     - "Finalize"

;;------------------------------------------------------------------------------
;; Bootstrap
;;------------------------------------------------------------------------------

;; TODO: load imp & other early/required-for-everything packages/modules


;;------------------------------------------------------------------------------
;; Initialize!
;;------------------------------------------------------------------------------


;; TODO: Basically this order:
;; TODO:   1. init this
;; TODO:   2. for each module, init it
;; TODO:   3. for each DOOMDIR, init it
;; TODO: Same for config.
;; TODO: Finalize does not do all that - just hooks and stuff.


;; Ensure Doom's core libraries are properly initialized, autoloads file is
;; loaded, and hooks set up for an interactive session.
(doom-initialize)

;; Now we load all enabled modules in the order dictated by your `doom!' block
;; in $DOOMDIR/init.el. `doom-initialize-modules' loads them (and hooks) in the
;; given order:
;;
;;   $DOOMDIR/init.el
;;   {$DOOMDIR,~/.emacs.d}/modules/*/*/init.el
;;   `doom-before-init-modules-hook'
;;   {$DOOMDIR,~/.emacs.d}/modules/*/*/config.el
;;   `doom-init-modules-hook'
;;   $DOOMDIR/config.el
;;   `doom-after-init-modules-hook'
;;   `after-init-hook'
;;   `emacs-startup-hook'
;;   `doom-init-ui-hook'
;;   `window-setup-hook'
;;
;; And then we're good to go!
(doom-initialize-modules)

;; TODO: split doom-initialize-modules up into explicit stages


;;------------------------------------------------------------------------------
;; Configure?
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; Finalize.
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
;; TODO: provide imp symbol

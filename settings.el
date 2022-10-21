;;; settings.el --- Settings for Innit -*- lexical-binding: t; -*-
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
;; Debug
;;------------------------------------------------------------------------------

;; You should use the Emacs `--debug-init' flag on the command line...
;; (setq init-file-debug nil)
;; (setq debug-on-error nil)


;;------------------------------------------------------------------------------
;; Init Output
;;------------------------------------------------------------------------------

;; Allow messages during innit for debugging.
(setq innit:display:messages  t
      innit:display:load-file t)


;;------------------------------------------------------------------------------
;; Window
;;------------------------------------------------------------------------------

;; Resize based on pixels, not characters. Allows maximizing to actually
;; maximize.
(setq frame-resize-pixelwise t)


;; Menu Bar (File, Edit... Menus): No.
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))

;; Tool Bar (New, Open... Buttons): Go away.
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; Scroll Bar: Hm..........
;; Not sure.
;;   - On the one hand, get rid of it to get a bit more screen real estate.
;;   - On the other hand, keep it for showing where in buffer we are and how big
;;     buffer is?
;; (when (fboundp 'scroll-bar-mode)
;;   (scroll-bar-mode -1))


;;------------------------------------------------------------------------------
;; Large Monitors / Window Splitting
;;------------------------------------------------------------------------------

;; For `split-window-sensibly'.
;; To split "vertically" (as in a horizontal split so the new and current are
;; now stacked vertically), must have at least this many lines visible RIGHT
;; NOW (not after split). Or set to nil to disable.
;;
;; Annoying right now since my work monitors aren't big enough to bother getting
;; used to more than 2 side-by-sides.
(customize-set-variable 'split-height-threshold nil)


;; For `split-window-sensibly'.
;; To split "horizontally" (as in a vertical split so the new and current are
;; now stacked side-by-side), must have at least this many columns visible RIGHT
;; NOW (not after split). Or set to nil to disable.
;;
;; Annoying right now since my work monitors aren't big enough to bother getting
;; used to more than 2 side-by-sides. 160 is wide enough that it /should/ work
;; for most situations.
(customize-set-variable 'split-width-threshold 160)


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :settings)

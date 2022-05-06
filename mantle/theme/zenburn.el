;;; zenburn.el --- Low-Contrast Theme -*- lexical-binding: t; -*-
;;
;; Author: Cole Brown <code@brown.dev>
;; URL:    https://github.com/cole-brown/.config-emacs
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;  Low-Contrast Theme
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Best Theme: Zenburn
;;------------------------------------------------------------------------------
;; Second Best Theme: Also Zenburn

(imp:use-package zenburn-theme
  ;; Don't see any reason not to demand the theme.
  :demand t

  ;;----------------------------------------------------------------------------
  :init
  ;;----------------------------------------------------------------------------

  ;;------------------------------
  ;; Colors
  ;;------------------------------

  ;; NOTE: 'zenburn' uses "-N" for lighter and "+N" for darker in their names
  ;; in the `zenburn-default-colors-alist' variable.
  ;;
  ;; These are some additional colors I'm testing out.
  ;;
  ;; Went to this website and plugged in `zenburn-magenta' and `zenburn-bg'
  ;; with 10 midpoints:
  ;;   https://meyerweb.com/eric/tools/color-blend/#3F3F3F:DC8CC3:10:hex
  (setq mantle:theme:color:zenburn-magenta-bg   "#4D464B"
        mantle:theme:color:zenburn-magenta-bg-1 "#5C4D57"
        mantle:theme:color:zenburn-magenta-bg-2 "#6A5463"
        mantle:theme:color:zenburn-magenta-bg-3 "#785B6F"
        mantle:theme:color:zenburn-magenta-bg-4 "#86627B"
        mantle:theme:color:zenburn-magenta-bg-5 "#956987")
  ;; ;;----------------------------------------------------------------------------
  ;; :custom
  ;; ;;----------------------------------------------------------------------------


  ;;----------------------------------------------------------------------------
  :config
  ;;----------------------------------------------------------------------------

  ;;------------------------------
  ;; Settings that aren't `defcustom'
  ;;------------------------------
  ;; NOTE: These are all `defvar', so they can't be set in `:custom' section.

  (setq zenburn-scale-org-headlines     t ; Scale headings in `org-mode'?
        zenburn-scale-outline-headlines t ; Scale headings in `outline-mode'?
        ;; zenburn-use-variable-pitch   t ; Use variable-pitch fonts for some headings and titles
        )


  ;;------------------------------
  ;; Faces
  ;;------------------------------



  ;;------------------------------
  ;; Finally: Load the Theme
  ;;------------------------------

  ;; NOTE: The theme in the `zenburn-theme' package is just called `zenburn'.
  ;;
  ;; Load the theme please and yes, I do want to load a theme thank you.
  (load-theme 'zenburn :no-confirm))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'theme 'zenburn)

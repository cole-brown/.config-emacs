;;; init.el --- User-level Emacs Initialization -*- lexical-binding: t; -*-
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


(nub:out :innit
         :debug
         (imp:file:current)
         "mantle/init: Start")


;;------------------------------------------------------------------------------
;; "init.el" and "config.el"
;;------------------------------------------------------------------------------
;; Add `imp' feature lists to be loaded if correct file is present at
;; imp root path.
;;   - "init.el" will be checked for after core init is run.
;;   - "config.el" will be checked for after core config is run.
;;
;; (innit:feature:mantle:add "core/10-init/20-load.el" :foo)
;; (innit:feature:mantle:add "core/10-init/20-load.el" :zort narf)


;;------------------------------------------------------------------------------
;; Order of User Init
;;------------------------------------------------------------------------------

;; TODO: something like `doom!' so user can define a load order easily.


;;------------------------------------------------------------------------------
;; Run User's Init
;;------------------------------------------------------------------------------

;; TODO: Move into another file so this can just be the load order thingy...
;;------------------------------------------------------------------------------
;; Color scheme: Zenburn
;;------------------------------------------------------------------------------
(imp:use-package zenburn-theme
  ;; Don't see any reason not to demand the theme.
  :demand t

  ;;--------------------
  :init
  ;;--------------------
  ;; NOTE: 'zenburn' uses "-N" for lighter and "+N" for darker in their names
  ;; in the `zenburn-default-colors-alist' variable.
  ;;
  ;; These are some additional colors I'm testing out.
  ;;
  ;; Went to this website and plugged in `zenburn-magenta' and `zenburn-bg'
  ;; with 10 midpoints:
  ;;   https://meyerweb.com/eric/tools/color-blend/#3F3F3F:DC8CC3:10:hex
  (setq dot-emacs:theme:color:zenburn-magenta-bg   "#4D464B"
        dot-emacs:theme:color:zenburn-magenta-bg-1 "#5C4D57"
        dot-emacs:theme:color:zenburn-magenta-bg-2 "#6A5463"
        dot-emacs:theme:color:zenburn-magenta-bg-3 "#785B6F"
        dot-emacs:theme:color:zenburn-magenta-bg-4 "#86627B"
        dot-emacs:theme:color:zenburn-magenta-bg-5 "#956987")

  ;; ;;--------------------
  ;; :custom
  ;; ;;--------------------

  ;;--------------------
  :config
  ;;--------------------

  ;; NOTE: These are all `defvar', so they can't be set in `:custom' section.
  (setq zenburn-scale-org-headlines     t ; Scale headings in `org-mode'?
        zenburn-scale-outline-headlines t ; Scale headings in `outline-mode'?
        ;; zenburn-use-variable-pitch   t ; Use variable-pitch fonts for some headings and titles
        )

  ;; NOTE: The theme in the `zenburn-theme' package is just called `zenburn'.
  (load-theme 'zenburn))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(nub:out :innit
         :debug
         (imp:file:current)
         "mantle/init: End")
(imp:provide :mantle 'init)

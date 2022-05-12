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

  ;; ;;----------------------------------------------------------------------------
  ;; :init
  ;; ;;----------------------------------------------------------------------------


  ;;----------------------------------------------------------------------------
  :custom
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
  ;;
  ;; NOTE: Currently none of these actually override Zenburn's colors - they
  ;; just additive.
  ;;
  ;; TODO-color-font-lock: NOTE: Enable colors font-lock with `font:font-lock:color:hex'.
  ;; TODO-color-font-lock: Is that the right function name?
  (zenburn-override-colors-alist '((zenburn-magenta-bg   . "#4D464B")
                                   (zenburn-magenta-bg-1 . "#5C4D57")
                                   (zenburn-magenta-bg-2 . "#6A5463")
                                   (zenburn-magenta-bg-3 . "#785B6F")
                                   (zenburn-magenta-bg-4 . "#86627B")
                                   (zenburn-magenta-bg-5 . "#956987")
                                   (zenburn-violet       . "#a9a1e1")
                                   (zenburn-magenta-01   . "#c67eaf")
                                   (zenburn-magenta-03   . "#9a6288"))
  ;; TODO: Delete this?
  ;; NOTE: Colors used to customize `doom-zenburn'
  ;; (doom-color 'bg-alt)                     "#383838"
  ;; (doom-color 'base7)                      "#6F6F6F"
  ;; (doom-lighten (doom-color 'base7) 0.3)   "#9a9a9a"
  ;; (doom-color 'red-1)                      "#BC8383"
  ;; (doom-color 'red-4)                      "#8C5353"
  ;; (doom-darken (doom-color 'red-4) 0.1)    "#7e4a4a"
  ;; (doom-color 'orange)                     "#DFAF8F"
  ;; (doom-color 'green-2)                    "#5F7F5F"
  ;; (doom-darken (doom-color 'green-2) 0.15) "#506b50"
  ;; (doom-color 'green+2)                    "#9FC59F"
  ;; (doom-color 'blue-1)                     "#7CB8BB"
  ;; (doom-color 'blue-3)                     "#5C888B"
  ;; (doom-color 'violet)                     "#a9a1e1"
  ;; (doom-color 'magenta)                    "#DC8CC3"
  ;; (doom-darken (doom-color 'magenta) 0.1)  "#c67eaf"
  ;; (doom-darken (doom-color 'magenta) 0.3)  "#9a6288"
  ;;
  ;; TODO: Delete this?
  ;; NOTE: Zenburn's colors:
  ;; (defvar zenburn-default-colors-alist
  ;;   '(("zenburn-fg-1"     . "#656555")
  ;;     ("zenburn-fg-05"    . "#989890") ; close: 'base7 lighten by 0.3
  ;;     ("zenburn-fg"       . "#DCDCCC")
  ;;     ("zenburn-fg+1"     . "#FFFFEF")
  ;;     ("zenburn-fg+2"     . "#FFFFFD")
  ;;     ("zenburn-bg-2"     . "#000000")
  ;;     ("zenburn-bg-1"     . "#2B2B2B")
  ;;     ("zenburn-bg-08"    . "#303030")
  ;;     ("zenburn-bg-05"    . "#383838") ; doom: 'bg-alt
  ;;     ("zenburn-bg"       . "#3F3F3F")
  ;;     ("zenburn-bg+05"    . "#494949")
  ;;     ("zenburn-bg+1"     . "#4F4F4F")
  ;;     ("zenburn-bg+2"     . "#5F5F5F")
  ;;     ("zenburn-bg+3"     . "#6F6F6F") ; doom: 'base7
  ;;     ("zenburn-red-6"    . "#6C3333")
  ;;     ("zenburn-red-5"    . "#7C4343") ; close: 'red-4 darken by 0.1
  ;;     ("zenburn-red-4"    . "#8C5353") ; doom: 'red-4
  ;;     ("zenburn-red-3"    . "#9C6363")
  ;;     ("zenburn-red-2"    . "#AC7373")
  ;;     ("zenburn-red-1"    . "#BC8383") ; doom: 'red-1
  ;;     ("zenburn-red"      . "#CC9393")
  ;;     ("zenburn-red+1"    . "#DCA3A3")
  ;;     ("zenburn-red+2"    . "#ECB3B3")
  ;;     ("zenburn-orange"   . "#DFAF8F") ; doom: 'orange
  ;;     ("zenburn-yellow-2" . "#D0BF8F")
  ;;     ("zenburn-yellow-1" . "#E0CF9F")
  ;;     ("zenburn-yellow"   . "#F0DFAF")
  ;;     ("zenburn-green-5"  . "#2F4F2F")
  ;;     ("zenburn-green-4"  . "#3F5F3F")
  ;;     ("zenburn-green-3"  . "#4F6F4F") ; close: 'green-2 darken by 0.15
  ;;     ("zenburn-green-2"  . "#5F7F5F") ; doom: 'green-2
  ;;     ("zenburn-green-1"  . "#6F8F6F")
  ;;     ("zenburn-green"    . "#7F9F7F")
  ;;     ("zenburn-green+1"  . "#8FB28F")
  ;;     ("zenburn-green+2"  . "#9FC59F") ; doom: 'green+2
  ;;     ("zenburn-green+3"  . "#AFD8AF")
  ;;     ("zenburn-green+4"  . "#BFEBBF")
  ;;     ("zenburn-cyan"     . "#93E0E3")
  ;;     ("zenburn-blue+3"   . "#BDE0F3")
  ;;     ("zenburn-blue+2"   . "#ACE0E3")
  ;;     ("zenburn-blue+1"   . "#94BFF3")
  ;;     ("zenburn-blue"     . "#8CD0D3")
  ;;     ("zenburn-blue-1"   . "#7CB8BB") ; doom: 'blue-1
  ;;     ("zenburn-blue-2"   . "#6CA0A3")
  ;;     ("zenburn-blue-3"   . "#5C888B") ; doom: 'blue-3
  ;;     ("zenburn-blue-4"   . "#4C7073")
  ;;     ("zenburn-blue-5"   . "#366060")
  ;;     ("zenburn-magenta"  . "#DC8CC3"))) ; doom: 'magenta
  ;;
  ;; TODO Need?:
  ;; (doom-color 'violet)                     "#a9a1e1"
  ;; (doom-darken (doom-color 'magenta) 0.1)  "#c67eaf"
  ;; (doom-darken (doom-color 'magenta) 0.3)  "#9a6288"


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
  ;; I need to customize some org-mode faces - they're not the greatest (IMO) in
  ;; Zenburn. So here are the vars at their defaults.
  ;;
  ;; Could maybe move these out of Zenburn into general theme stuff if ever
  ;; other themes want to adjust the same faces? In that case, split like:
  ;;
  ;; "some-theme-file.el":
  ;; (defface mantle:theme:face:org.todo.keyword:todo
  ;;   (list (cons
  ;;          ;; display type
  ;;          t
  ;;          ;; attributes
  ;;          (list
  ;;           :weight 'bold
  ;;           :inherit 'warning)))
  ;;   "Face for todo keyword in todo sequence."
  ;;   :group 'innit:group)
  ;;
  ;; "zenburn.el":
  ;; (face-spec-set 'mantle:theme:face:org.todo.keyword:todo
  ;;                (list (cons
  ;;                       ;; display type
  ;;                       t
  ;;                       ;; attributes
  ;;                       face-plist)))
  ;;------------------------------
  (imp:eval:after org-mode
    ;; Set up keys in `zenburn-*-colors-alist' as variables.
    (zenburn-with-color-variables

     (defface mantle:theme:face:org.todo.keyword:background
       (list (cons
              ;; display type
              t
              ;; attributes
              ;; nil
              (list :background zenburn-bg-05)))
       "Face for the background of todo sequence keywords."
       :group 'innit:group:theme)

     (defface mantle:theme:face:org.todo.keyword:todo
       (list (cons
              ;; display type
              t
              ;; attributes
              ;; (list
              ;;  :weight 'bold
              ;;  :inherit 'warning)
              (list :foreground zenburn-magenta-01
                    :background zenburn-bg-05
                    :weight 'bold)))
       "Face for todo keyword in todo sequence."
       :group 'innit:group:theme)

     (defface mantle:theme:face:org.todo.keyword:done.good
       (list (cons
              ;; display type
              t
              ;; attributes
              ;; (list
              ;;  :inherit 'org-done)
              (list :background zenburn-bg-05
                    :inherit 'org-done)))
       "Face for todo keyword in todo sequence."
       :group 'innit:group:theme)

     (defface mantle:theme:face:org.todo.keyword:done.bad
       (list (cons
              ;; display type
              t
              ;; attributes
              ;; (list
              ;;  :inherit 'org-done)))
              (list :background zenburn-bg-05
                    :foreground zenburn-red-5)))
       "Face for todo keyword in todo sequence."
       :group 'innit:group:theme)

     (defface mantle:theme:face:org.todo.keyword:info
       (list (cons
              ;; display type
              t
              ;; attributes
              ;; (list
              ;;  :inherit 'org-done)
              (list :background zenburn-bg-05
                    :foreground zenburn-bg+3)))
       "Face for todo keyword in todo sequence."
       :group 'innit:group:theme)

     (defface mantle:theme:face:org.todo.keyword:null
       (list (cons
              ;; display type
              t
              ;; attributes
              ;; (list
              ;;  :inherit 'org-done)
              (list :background zenburn-bg-05
                    :foreground zenburn-bg+3)))
       "Face for todo keyword in todo sequence."
       :group 'innit:group:theme)))


  ;;------------------------------
  ;; Finally: Load the Theme
  ;;------------------------------
  ;; NOTE: The theme in the `zenburn-theme' package is just called `zenburn'.
  (innit:theme:load 'zenburn))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'theme 'zenburn)

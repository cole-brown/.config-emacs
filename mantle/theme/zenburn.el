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


  ;;------------------------------
  ;; Faces
  ;;------------------------------
  ;; I need to customize some org-mode faces - they're not the greatest (IMO) in
  ;; Zenburn. So here are the vars at their defaults. The config section can
  ;; update them as desired.
  ;;
  ;; Could maybe move these out of Zenburn into general theme stuff if ever
  ;; other themes want to adjust the same faces?
  ;;------------------------------

  (defface mantle:theme:face:org.todo.keyword:todo
    (list (cons
           ;; display type
           t
           ;; attributes
           (list
            :weight 'bold
            :inherit 'warning)))
    "Face for todo keyword in todo sequence."
    :group 'doom-themes)


  (defface mantle:theme:face:org.todo.keyword:background
    (list (cons
           ;; display type
           t
           ;; attributes
           nil))
    "Face for todo keyword in todo sequence."
    :group 'doom-themes)


  (defface mantle:theme:face:org.todo.keyword:done.good
    (list (cons
           ;; display type
           t
           ;; attributes
           (list
            :inherit 'org-done)))
    "Face for todo keyword in todo sequence."
    :group 'doom-themes)


  (defface mantle:theme:face:org.todo.keyword:done.bad
    (list (cons
           ;; display type
           t
           ;; attributes
           (list
            :inherit 'org-done)))
    "Face for todo keyword in todo sequence."
    :group 'doom-themes)


  (defface mantle:theme:face:org.todo.keyword:info
    (list (cons
           ;; display type
           t
           ;; attributes
           (list
            :inherit 'org-done)))
    "Face for todo keyword in todo sequence."
    :group 'doom-themes)

  (defface mantle:theme:face:org.todo.keyword:null
    (list (cons
           ;; display type
           t
           ;; attributes
           (list
            :inherit 'org-done)))
    "Face for todo keyword in todo sequence."
    :group 'doom-themes)


  ;; TODO: Steal `doom-darken', `doom-color', etc?
  ;;
  ;; TODO: Use above color variables or zenburn's color variables?
  ;; TODO: Use website from above color variables to check on the colors used here?
  ;;
  ;; TODO: Combine these into the defface? No real point having them separate unless I'm going to use Zenburn funcs to set the colors....
  ;;   - ALT-TODO: Use Zenburn funcs to set the colors, move back down to `:config'

  ;; ;; Attributes plist:
  ;; (let ((face-plist
  ;;        (list :foreground "#c67eaf" ;; (doom-darken (doom-color 'magenta) 0.1)
  ;;              :background "#383838" ;; (doom-color 'bg-alt)
  ;;              :weight 'bold
  ;;              ;; :underline t
  ;;              )))

  ;;   (face-spec-set 'mantle:theme:face:org.todo.keyword:todo
  ;;                  (list (cons
  ;;                         ;; display type
  ;;                         t
  ;;                         ;; attributes
  ;;                         face-plist)))

  ;;   (face-spec-set 'mantle:theme:face:org.todo.keyword:background
  ;;                  (list (cons
  ;;                         ;; display type
  ;;                         t
  ;;                         ;; attributes
  ;;                         (list :background "#383838" ;; (doom-color 'bg-alt)
  ;;                               ))))

  ;;   (face-spec-set 'mantle:theme:face:org.todo.keyword:done.good
  ;;                  (list (cons
  ;;                         ;; display type
  ;;                         t
  ;;                         ;; attributes
  ;;                         (list :background "#383838" ;; (doom-color 'bg-alt)
  ;;                               :inherit 'org-done))))

  ;;   (face-spec-set 'mantle:theme:face:org.todo.keyword:done.bad
  ;;                  (list (cons
  ;;                         ;; display type
  ;;                         t
  ;;                         ;; attributes
  ;;                         (list :background "#383838" ;; (doom-color 'bg-alt)
  ;;                               :foreground "#7e4a4a" ;; (doom-darken (doom-color 'red-4) 0.1)
  ;;                               ))))

  ;;   (face-spec-set 'mantle:theme:face:org.todo.keyword:info
  ;;                  (list (cons
  ;;                         ;; display type
  ;;                         t
  ;;                         ;; attributes
  ;;                         (list :background "#383838" ;; (doom-color 'bg-alt)
  ;;                               :foreground "#6F6F6F" ;; (doom-color 'base7)
  ;;                               ))))

  ;;   (face-spec-set 'mantle:theme:face:org.todo.keyword:null
  ;;                  (list (cons
  ;;                         ;; display type
  ;;                         t
  ;;                         ;; attributes
  ;;                         (list :background "#383838" ;; (doom-color 'bg-alt)
  ;;                               :foreground "#6F6F6F" ;; (doom-color 'base7)
  ;;                               )))))


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
  (innit:theme:load 'zenburn))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'theme 'zenburn)

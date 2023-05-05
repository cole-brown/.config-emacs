;;; mantle/config/all-the-icons.el --- Icon Fonts -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2022-07-13
;; Modified:   2022-07-13
;; URL:        https://github.com/cole-brown/.config-emacs
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;  Icon Fonts
;;
;;; Code:


;; ┌──────────────────────────────────═══───────────────────────────────────┐ ;;
;; │                                 ═════                                  │ ;;
;; │                             All The Icons                              │ ;;
;; │                            (Wingdings 3.0)                             │ ;;
;; │                                 ═════                                  │ ;;
;; └──────────────────────────────────═══───────────────────────────────────┘ ;;


;; TODO:all-the-icons: [2023-05-02]: Can't figure out how to say, like, "I'd
;; really rather `all-the-icons' exist when these are called but if it can't
;; then ok use these backups.
;;
;; If we do these placeholder defuns, then override with advice later,
;; `all-the-icons' doesn't bother noticing it's wanted and can/should (auto?)load.

;; ;;------------------------------------------------------------------------------
;; ;; Helper Functions, Placeholder
;; ;;------------------------------------------------------------------------------
;; ;;
;; ;; Docs for Pretty Hydra use some of these in their example, which is great,
;; ;; except they don't exist... So I found a GitHub Gist that defines them:
;; ;;   https://gist.github.com/mbuczko/e15d61363d31cf78ff17427072e0c325"
;; ;;
;; ;; We're going to build them in 2 parts, so that we can have or not have
;; ;; `all-the-icons'.
;; ;;   1) `defun' the functions without any `all-the-icons' dependencies.
;; ;;   2) `define-advice' with `:override' after `all-the-icons' is loaded.
;;
;; (defun mantle:user:icon/font-awesome (icon str &rest plist)
;;   "Return string of Font Awesome ICON and STR.
;;
;; Optional PLIST's optional keys:
;;   - `:height'     - HEIGHT
;;   - `:v-adjust'   - V-ADJUST
;;   - `:color:icon' - ICON-COLOR
;;   - `:face'       - FACE
;;   - `:help:echo'  - HELP-ECHO
;;
;; HEIGHT and V-ADJUST are sent to `all-the-icons-faicon'.
;;
;; ICON-COLOR is used to color only the icon character.
;;
;; FACE is used for the icon and label.
;;
;; HELP-ECHO should be a string and will be put in the `help-echo' property.
;;
;; NOTE: Used to be `with-fa'.
;; [2022-02-04] https://gist.github.com/mbuczko/e15d61363d31cf78ff17427072e0c325"
;;     ;; Want to keep the variables named properly, as the actual function (aka
;;     ;; override advice) uses them but don't want the linter complaining. So...
;;     ;; ignore them:
;;     (ignore icon plist)
;;
;;     ;; Placeholder function; will be used if `all-the-icons' isn't loaded, so
;;     ;; don't call any `all-the-icons' functions. Just return the supplied STR.
;;     str)
;;   ;; (mantle:user:icon/font-awesome "spotify" "Spotify" :color:icon "limegreen" :height 1 :v-adjust -0.05)
;;   ;; (insert (mantle:user:icon/font-awesome "spotify" "Spotify" :color:icon "limegreen" :height 1 :v-adjust -0.05))
;;
;;
;;   (defun mantle:user:icon/file-icon (icon str &rest plist)
;;     "Return string of File ICON and STR.
;;
;; Optional PLIST's optional keys:
;;   - `:height'     - HEIGHT
;;   - `:v-adjust'   - V-ADJUST
;;   - `:color:icon' - ICON-COLOR
;;   - `:face'       - FACE
;;   - `:help:echo'  - HELP-ECHO
;;
;; HEIGHT and V-ADJUST are sent to `all-the-icons-fileicon'.
;;
;; ICON-COLOR is used to color only the icon character.
;;
;; FACE is used for the icon and label.
;;
;; HELP-ECHO should be a string and will be put in the `help-echo' property.
;;
;; [2022-02-04] https://gist.github.com/mbuczko/e15d61363d31cf78ff17427072e0c325"
;;     ;; Want to keep the variables named properly, as the actual function (aka
;;     ;; override advice) uses them but don't want the linter complaining. So...
;;     ;; ignore them:
;;     (ignore icon plist)
;;
;;     ;; Placeholder function; will be used if `all-the-icons' isn't loaded, so
;;     ;; don't call any `all-the-icons' functions. Just return the supplied STR.
;;     str)
;;
;;
;;   (defun mantle:user:icon/octicon (icon str &rest plist)
;;     "Return string of Octicon ICON and STR.
;;
;; Optional PLIST's optional keys:
;;   - `:height'     - HEIGHT
;;   - `:v-adjust'   - V-ADJUST
;;   - `:color:icon' - ICON-COLOR
;;   - `:face'       - FACE
;;   - `:help:echo'  - HELP-ECHO
;;
;; HEIGHT and V-ADJUST are sent to `all-the-icons-octicon'.
;;
;; ICON-COLOR is used to color only the icon character.
;;
;; FACE is used for the icon and label.
;;
;; HELP-ECHO should be a string and will be put in the `help-echo' property.
;;
;; [2022-02-04] https://gist.github.com/mbuczko/e15d61363d31cf78ff17427072e0c325"
;;     ;; Want to keep the variables named properly, as the actual function (aka
;;     ;; override advice) uses them but don't want the linter complaining. So...
;;     ;; ignore them:
;;     (ignore icon plist)
;;
;;     ;; Placeholder function; will be used if `all-the-icons' isn't loaded, so
;;     ;; don't call any `all-the-icons' functions. Just return the supplied STR.
;;     str)
;;
;;
;;   (defun mantle:user:icon/material (icon str &rest plist)
;;     "Return string of Material ICON and STR.
;;
;; Optional PLIST's optional keys:
;;   - `:height'     - HEIGHT
;;   - `:v-adjust'   - V-ADJUST
;;   - `:color:icon' - ICON-COLOR
;;   - `:face'       - FACE
;;   - `:help:echo'  - HELP-ECHO
;;
;; HEIGHT and V-ADJUST are sent to `all-the-icons-material'.
;;
;; ICON-COLOR is used to color only the icon character.
;;
;; FACE is used for the icon and label.
;;
;; HELP-ECHO should be a string and will be put in the `help-echo' property.
;;
;; [2022-02-04] https://gist.github.com/mbuczko/e15d61363d31cf78ff17427072e0c325"
;;     ;; Want to keep the variables named properly, as the actual function (aka
;;     ;; override advice) uses them but don't want the linter complaining. So...
;;     ;; ignore them:
;;     (ignore icon plist)
;;
;;     ;; Placeholder function; will be used if `all-the-icons' isn't loaded, so
;;     ;; don't call any `all-the-icons' functions. Just return the supplied STR.
;;     str)
;;
;;
;;   (defun mantle:user:icon:for-mode (mode str &rest plist)
;;     "Return string of MODE's icon and STR.
;;
;; Optional PLIST's optional keys:
;;   - `:separator'  - SEPARATOR
;;   - `:height'     - HEIGHT
;;   - `:v-adjust'   - V-ADJUST
;;   - `:color:icon' - ICON-COLOR
;;   - `:face'       - FACE
;;   - `:help:echo'  - HELP-ECHO
;;
;; SEPARATOR, a string to use to separate the icon and STR. Defaults to \" \".
;; If no separator is desired, supply something that is not a string, like `:none'
;; or (an explicit) nil.
;;   Space Separator (default):
;;     (mantle:user:icon:for-mode 'emacs-lisp \"Emacs Lisp\")
;;     (mantle:user:icon:for-mode 'emacs-lisp \"Emacs Lisp\" :separator \" \")
;;   No Separator:
;;     (mantle:user:icon:for-mode 'emacs-lisp \"Emacs Lisp\" :separator :none)
;;     (mantle:user:icon:for-mode 'emacs-lisp \"Emacs Lisp\" :separator nil)
;;
;; Currently, defaults to setting V-ADJUST based on... `major-mode' variable???
;;   - `emacs-lisp-mode' gets V-ADJUST of 0.0
;;   - Everyone else gets V-ADJUST of 0.05
;; If this is not desired, supply the correct V-ADJUST.
;;
;; HEIGHT and V-ADJUST are sent to `all-the-icons-icon-for-mode'.
;;
;; ICON-COLOR is used to color only the icon character.
;;
;; FACE is used for the icon and label.
;;
;; HELP-ECHO should be a string and will be put in the `help-echo' property.
;;
;; [2022-02-04] https://gist.github.com/mbuczko/e15d61363d31cf78ff17427072e0c325"
;;     ;; Want to keep the variables named properly, as the actual function (aka
;;     ;; override advice) uses them but don't want the linter complaining. So...
;;     ;; ignore them:
;;     (ignore mode plist)
;;
;;     ;; Placeholder function; will be used if `all-the-icons' isn't loaded, so
;;     ;; don't call any `all-the-icons' functions. Just return the supplied STR.
;;     str)


;;------------------------------------------------------------------------------
;; Icons, Except They're Glyphs in Special Fonts
;;------------------------------------------------------------------------------
;; aka Rise of the Nerd Fonts

(imp:use-package all-the-icons
  ;; Only use `all-the-icons' when we have a GUI.
  ;;---
  ;; NOTE [2023-05-04]:
  ;; Just checking `display-graphic-p' is what `all-the-icons' says to do, and
  ;; it is incorrect. It returns nil during startup. I assume because I'm
  ;; starting Emacs as a server? It returns t properly once I'm in control
  ;; enough to evaluate: (display-graphic-p)
  ;;
  ;; Can't check `server-running-p' for other reasons...
  ;;   http://emacshorrors.com/posts/determining-if-the-server-is-started-or-the-wonders-of-server-running-p.html
  ;; But `innit:emacs/server:running?' does implement a suggestion from long ago
  ;; that never got added to Emacs:
  ;;   https://lists.gnu.org/archive/html/bug-gnu-emacs/2018-06/msg00723.html
  ;;---
  :if (or (display-graphic-p)
          (innit:emacs/server:running?))

  ;;------------------------------------------------------------------------------
  :init
  ;;------------------------------------------------------------------------------

  ;;------------------------------------------------------------------------------
  ;; Helper Functions, Actual
  ;;------------------------------------------------------------------------------

  (defun int<mantle>:user:face/icon (plist)
    "Get face to use for icon from PLIST.

Check `:face', `:face:icon', and `:color:icon'.
Return nil if no face found."
    ;; Options specific to the icon first.
    (cond ((plist-get plist :face:icon))
          ((plist-member plist :color:icon)
           (list :foreground (plist-get plist :color:icon)))
          ((plist-get plist :face))
          (t
           nil)))


  (defun int<mantle>:user:face/text (plist)
    "Get face to use for text from PLIST.

Check `:face', `:face:text', and `:color:text'.
Return nil if no face found."
    ;; Options specific to the text first.
    (cond ((plist-get plist :face:text))
          ((plist-member plist :color:text)
           (list :foreground (plist-get plist :color:text)))
          ((plist-get plist :face))
          (t
           nil)))


  ;; TODO:all-the-icons: `help-echo' and others from the optional PLIST?
  ;; (defun +modeline-format-icon (icon label &optional face help-echo voffset)
  ;;   (propertize (concat (all-the-icons-material
  ;;                        icon
  ;;                        :face face
  ;;                        :height 1.1
  ;;                        :v-adjust (or voffset -0.225))
  ;;                       (propertize label 'face face))
  ;;               'help-echo help-echo))


  ;; ;; Advice via `define-advice' is named SYMBOL@NAME, so in this case the advice
  ;; ;; function is `mantle:user:icon/font-awesome@mantle:override'.
  ;; (define-advice mantle:user:icon/font-awesome (:override (icon str &rest plist) mantle:override)
  (defun mantle:user:icon/font-awesome (icon str &rest plist)
    "Return string of Font Awesome ICON and STR.

Optional PLIST's optional keys:
  - `:height'     - HEIGHT
  - `:v-adjust'   - V-ADJUST
  - `:color:icon' - ICON-COLOR
  - `:face'       - FACE
  - `:help:echo'  - HELP-ECHO


HEIGHT and V-ADJUST are sent to `all-the-icons-faicon'.

ICON-COLOR is used to color only the icon character.

FACE is used for the icon and label.

HELP-ECHO should be a string and will be put in the `help-echo' property.

[2022-02-04] https://gist.github.com/mbuczko/e15d61363d31cf78ff17427072e0c325"
    ;; Only bother propertizing if we need to.
    (let ((face/icon (int<mantle>:user:face/icon plist))
          (face/text (int<mantle>:user:face/text plist)))
      (concat
       (all-the-icons-faicon icon
                             :face face/icon
                             :v-adjust (or (plist-get plist :v-adjust) 0)
                             :height (or (plist-get plist :height) 1))
       " "
       (if face/text
           (propertize str 'face face/text)
         str))))
  ;; (mantle:user:icon/font-awesome "spotify" "Spotify" :color:icon "limegreen" :height 1 :v-adjust -0.05)
  ;; (insert (mantle:user:icon/font-awesome "spotify" "Spotify" :color:icon "limegreen" :height 1 :v-adjust -0.05))


  ;; (define-advice mantle:user:icon/file-icon (:override (icon str &rest plist) mantle:override)
  (defun mantle:user:icon/file-icon (icon str &rest plist)
    "Return string of File ICON and STR.

Optional PLIST's optional keys:
  - `:height'     - HEIGHT
  - `:v-adjust'   - V-ADJUST
  - `:color:icon' - ICON-COLOR
  - `:face'       - FACE
  - `:help:echo'  - HELP-ECHO

HEIGHT and V-ADJUST are sent to `all-the-icons-fileicon'.

ICON-COLOR is used to color only the icon character.

FACE is used for the icon and label.

HELP-ECHO should be a string and will be put in the `help-echo' property.

[2022-02-04] https://gist.github.com/mbuczko/e15d61363d31cf78ff17427072e0c325"
    ;; Only bother propertizing if we need to.
    (let ((face/icon (int<mantle>:user:face/icon plist))
          (face/text (int<mantle>:user:face/text plist)))
      (concat
       (all-the-icons-fileicon icon
                               :face face/icon
                               :v-adjust (or (plist-get plist :v-adjust) 0)
                               :height (or (plist-get plist :height) 1))
       " "
       (if face
           (propertize str 'face face/text)
         str))))


  ;; (define-advice mantle:user:icon/octicon (:override (icon str &rest plist) mantle:override)
  (defun mantle:user:icon/octicon (icon str &rest plist)
    "Return string of Octicon ICON and STR.

Optional PLIST's optional keys:
  - `:height'     - HEIGHT
  - `:v-adjust'   - V-ADJUST
  - `:color:icon' - ICON-COLOR
  - `:face'       - FACE
  - `:help:echo'  - HELP-ECHO

HEIGHT and V-ADJUST are sent to `all-the-icons-octicon'.

ICON-COLOR is used to color only the icon character.

FACE is used for the icon and label.

HELP-ECHO should be a string and will be put in the `help-echo' property.

[2022-02-04] https://gist.github.com/mbuczko/e15d61363d31cf78ff17427072e0c325"
    ;; Only bother propertizing if we need to.
    (let ((face/icon (int<mantle>:user:face/icon plist))
          (face/text (int<mantle>:user:face/text plist)))
      (concat
       (all-the-icons-octicon icon
                              :face face/icon
                              :v-adjust (or (plist-get plist :v-adjust) 0)
                              :height (or (plist-get plist :height) 1))
       " "
       (if face
           (propertize str 'face face/text)
         str))))


  ;; (define-advice mantle:user:icon/material (:override (icon str &rest plist) mantle:override)
  (defun mantle:user:icon/material (icon str &rest plist)
    "Return string of Material ICON and STR.

Optional PLIST's optional keys:
  - `:height'     - HEIGHT
  - `:v-adjust'   - V-ADJUST
  - `:color:icon' - ICON-COLOR
  - `:face'       - FACE
  - `:help:echo'  - HELP-ECHO

HEIGHT and V-ADJUST are sent to `all-the-icons-material'.

ICON-COLOR is used to color only the icon character.

FACE is used for the icon and label.

HELP-ECHO should be a string and will be put in the `help-echo' property.

[2022-02-04] https://gist.github.com/mbuczko/e15d61363d31cf78ff17427072e0c325"
    ;; Only bother propertizing if we need to.
    (let ((face/icon (int<mantle>:user:face/icon plist))
          (face/text (int<mantle>:user:face/text plist)))
      (concat
       (all-the-icons-material icon
                               :face face/icon
                               :v-adjust (or (plist-get plist :v-adjust) 0)
                               :height (or (plist-get plist :height) 1))
       " "
       (if face
           (propertize str 'face face/text)
         str))))


  ;; (define-advice mantle:user:icon:for-mode (:override (icon str &rest plist) mantle:override)
  (defun mantle:user:icon:for-mode (mode str &rest plist)
    "Return string of MODE's icon and STR.

Optional PLIST's optional keys:
  - `:separator'  - SEPARATOR
  - `:height'     - HEIGHT
  - `:v-adjust'   - V-ADJUST
  - `:color:icon' - ICON-COLOR
  - `:face'       - FACE
  - `:help:echo'  - HELP-ECHO

SEPARATOR, a string to use to separate the icon and STR. Defaults to \" \".
If no separator is desired, supply something that is not a string, like `:none'
or (an explicit) nil.
  Space Separator (default):
    (mantle:user:icon:for-mode 'emacs-lisp \"Emacs Lisp\")
    (mantle:user:icon:for-mode 'emacs-lisp \"Emacs Lisp\" :separator \" \")
  No Separator:
    (mantle:user:icon:for-mode 'emacs-lisp \"Emacs Lisp\" :separator :none)
    (mantle:user:icon:for-mode 'emacs-lisp \"Emacs Lisp\" :separator nil)

Currently, defaults to setting V-ADJUST based on... `major-mode' variable???
  - `emacs-lisp-mode' gets V-ADJUST of 0.0
  - Everyone else gets V-ADJUST of 0.05
If this is not desired, supply the correct V-ADJUST.

HEIGHT and V-ADJUST are sent to `all-the-icons-icon-for-mode'.

ICON-COLOR is used to color only the icon character.

FACE is used for the icon and label.

HELP-ECHO should be a string and will be put in the `help-echo' property.

[2022-02-04] https://gist.github.com/mbuczko/e15d61363d31cf78ff17427072e0c325"
    ;; Only bother propertizing if we need to.
    (let* ((height    (plist-get plist :height))
           (v-adjust  (or (plist-get plist :v-adjust)
                          (if (eq major-mode 'emacs-lisp-mode)
                              0.0
                            0.05)))
           (face/icon (int<mantle>:user:face/icon plist))
           (face/text (int<mantle>:user:face/text plist))
           (separator (if (stringp (plist-get plist :separator))
                          (plist-get plist :separator)
                        " "))
           (icon      (all-the-icons-icon-for-mode mode
                                                   :face     face/icon
                                                   :height   height
                                                   :v-adjust v-adjust))
           (icon      (if (symbolp icon)
                          ;; Failed to find a specific icon for the mode; fallback:
                          (all-the-icons-octicon "file-text"
                                                 :face     face/icon
                                                 :height   height
                                                 :v-adjust v-adjust)
                        icon)))

      (concat icon
              separator
              (if face/text
                  (propertize str 'face face/text)
                str))))


  ;;------------------------------------------------------------------------------
  :config
  ;;------------------------------------------------------------------------------
  ;;------------------------------------------------------------------------------
  ;; Configuration
  ;;------------------------------------------------------------------------------

  ;; Try to automatically install the fonts if they're not present.
  ;; Closest I can get for now is this, I think.
  ;; See: https://github.com/domtronn/all-the-icons.el/issues/120
  (unless (find-font (font-spec :name "all-the-icons"))
    ;; Alternate check for the fonts if that doesn't work right:
    ;;   (unless (member "all-the-icons" (font-family-list))
    (all-the-icons-install-fonts :confirm)))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'config 'user 'all-the-icons)

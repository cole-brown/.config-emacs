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


;;------------------------------------------------------------------------------
;; all-the-icons
;;------------------------------------------------------------------------------

(imp:use-package all-the-icons
  ;; Only use `all-the-icons' when we have a GUI.
  :if (display-graphic-p)

  ;;------------------------------
  :config
  ;;------------------------------

  ;; Try to automatically install the fonts if they're not present.
  ;; Closest I can get for now is this, I think.
  ;; See: https://github.com/domtronn/all-the-icons.el/issues/120
  (unless (find-font (font-spec :name "all-the-icons"))
    ;; Alternate check for the fonts if that doesn't work right:
    ;;   (unless (member "all-the-icons" (font-family-list))
    (all-the-icons-install-fonts :confirm)))


;;------------------------------------------------------------------------------
;; Helper Functions
;;------------------------------------------------------------------------------

;; Docs for Pretty Hydra use some of these in their example, which is great,
;; except they don't exist... So I found a GitHub Gist that defines them:
;;   https://gist.github.com/mbuczko/e15d61363d31cf78ff17427072e0c325"
;;
;; We're going to build them in 2 parts, so that we can have or not have
;; `all-the-icons'.
;;   1) `defun' the functions without any `all-the-icons' dependencies.
;;   2) `define-advice' with `:override' after `all-the-icons' is loaded.


;;------------------------------
;; 1. Placeholders (sans `all-the-icons')
;;------------------------------

(defun mantle:user:icon/font-awesome (icon str &rest plist)
  "Return string of Font Awesome ICON and STR.

Optional PLIST's optional keys:
  - `:height'     - HEIGHT
  - `:v-adjust'   - V-ADJUST
  - `:color:icon' - ICON-COLOR

HEIGHT and V-ADJUST are sent to `all-the-icons-faicon'.

ICON-COLOR is used to color only the icon character.

[2022-02-04] https://gist.github.com/mbuczko/e15d61363d31cf78ff17427072e0c325"
  ;; Want to keep the variables named properly, as the actual function (aka
  ;; override advice) uses them but don't want the linter complaining. So...
  ;; ignore them:
  (ignore icon plist)

  ;; Placeholder function; will be used if `all-the-icons' isn't loaded, so
  ;; don't call any `all-the-icons' functions. Just return the supplied STR.
  str)
;; (with-faicon "spotify" "Spotify" :color:icon "limegreen" :height 1 :v-adjust -0.05)
;; (insert (with-faicon "spotify" "Spotify" :color:icon "limegreen" :height 1 :v-adjust -0.05))


(defun mantle:user:icon/file-icon (icon str &rest plist)
  "Return string of File ICON and STR.

Optional PLIST's optional keys:
  - `:height'     - HEIGHT
  - `:v-adjust'   - V-ADJUST
  - `:color:icon' - ICON-COLOR

HEIGHT and V-ADJUST are sent to `all-the-icons-fileicon'.

ICON-COLOR is used to color only the icon character.

[2022-02-04] https://gist.github.com/mbuczko/e15d61363d31cf78ff17427072e0c325"
  ;; Want to keep the variables named properly, as the actual function (aka
  ;; override advice) uses them but don't want the linter complaining. So...
  ;; ignore them:
  (ignore icon plist)

  ;; Placeholder function; will be used if `all-the-icons' isn't loaded, so
  ;; don't call any `all-the-icons' functions. Just return the supplied STR.
  str)


(defun mantle:user:icon/octicon (icon str &rest plist)
  "Return string of Octicon ICON and STR.

Optional PLIST's optional keys:
  - `:height'     - HEIGHT
  - `:v-adjust'   - V-ADJUST
  - `:color:icon' - ICON-COLOR

HEIGHT and V-ADJUST are sent to `all-the-icons-octicon'.

ICON-COLOR is used to color only the icon character.

[2022-02-04] https://gist.github.com/mbuczko/e15d61363d31cf78ff17427072e0c325"
  ;; Want to keep the variables named properly, as the actual function (aka
  ;; override advice) uses them but don't want the linter complaining. So...
  ;; ignore them:
  (ignore icon plist)

  ;; Placeholder function; will be used if `all-the-icons' isn't loaded, so
  ;; don't call any `all-the-icons' functions. Just return the supplied STR.
  str)


(defun mantle:user:icon/material (icon str &rest plist)
  "Return string of Material ICON and STR.

Optional PLIST's optional keys:
  - `:height'     - HEIGHT
  - `:v-adjust'   - V-ADJUST
  - `:color:icon' - ICON-COLOR

HEIGHT and V-ADJUST are sent to `all-the-icons-material'.

ICON-COLOR is used to color only the icon character.

[2022-02-04] https://gist.github.com/mbuczko/e15d61363d31cf78ff17427072e0c325"
  ;; Want to keep the variables named properly, as the actual function (aka
  ;; override advice) uses them but don't want the linter complaining. So...
  ;; ignore them:
  (ignore icon plist)

  ;; Placeholder function; will be used if `all-the-icons' isn't loaded, so
  ;; don't call any `all-the-icons' functions. Just return the supplied STR.
  str)


(defun mantle:user:icon:for-mode (mode str &rest plist)
  "Return string of MODE's icon and STR.

Optional PLIST's optional keys:
  - `:separator'  - SEPARATOR
  - `:height'     - HEIGHT
  - `:v-adjust'   - V-ADJUST
  - `:color:icon' - ICON-COLOR

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

[2022-02-04] https://gist.github.com/mbuczko/e15d61363d31cf78ff17427072e0c325"
  ;; Want to keep the variables named properly, as the actual function (aka
  ;; override advice) uses them but don't want the linter complaining. So...
  ;; ignore them:
  (ignore mode plist)

  ;; Placeholder function; will be used if `all-the-icons' isn't loaded, so
  ;; don't call any `all-the-icons' functions. Just return the supplied STR.
  str)


;;------------------------------
;; 2. Actual `all-the-icons' Helpers
;;------------------------------

;; Wait for `all-the-icons' to appear.
(imp:eval:after all-the-icons
  ;; Advice via `define-advice' is named SYMBOL@NAME, so in this case the advice
  ;; function is `mantle:user:icon/font-awesome@mantle:override'.
  (define-advice mantle:user:icon/font-awesome (:override (icon str &rest plist) mantle:override)
    "Return string of Font Awesome ICON and STR.

Optional PLIST's optional keys:
  - `:height'     - HEIGHT
  - `:v-adjust'   - V-ADJUST
  - `:color:icon' - ICON-COLOR

HEIGHT and V-ADJUST are sent to `all-the-icons-faicon'.

ICON-COLOR is used to color only the icon character.

[2022-02-04] https://gist.github.com/mbuczko/e15d61363d31cf78ff17427072e0c325"
    ;; Only bother propertizing if we need to.
    (let ((face (if (plist-member plist :color:icon)
                    (list :foreground (plist-get plist :color:icon))
                  nil)))

      (concat
       (all-the-icons-faicon icon
                             :face face
                             :v-adjust (or (plist-get plist :v-adjust) 0)
                             :height (or (plist-get plist :height) 1))
       " "
       str)))
  ;; (with-faicon "spotify" "Spotify" :color:icon "limegreen" :height 1 :v-adjust -0.05)
  ;; (insert (with-faicon "spotify" "Spotify" :color:icon "limegreen" :height 1 :v-adjust -0.05))


  (define-advice mantle:user:icon/file-icon (:override (icon str &rest plist) mantle:override)
    "Return string of File ICON and STR.

Optional PLIST's optional keys:
  - `:height'     - HEIGHT
  - `:v-adjust'   - V-ADJUST
  - `:color:icon' - ICON-COLOR

HEIGHT and V-ADJUST are sent to `all-the-icons-fileicon'.

ICON-COLOR is used to color only the icon character.

[2022-02-04] https://gist.github.com/mbuczko/e15d61363d31cf78ff17427072e0c325"
    ;; Only bother propertizing if we need to.
    (let ((face (if (plist-member plist :color:icon)
                    (list :foreground (plist-get plist :color:icon))
                  nil)))

      (concat
       (all-the-icons-fileicon icon
                               :face face
                               :v-adjust (or (plist-get plist :v-adjust) 0)
                               :height (or (plist-get plist :height) 1))
       " "
       str)))


  (define-advice mantle:user:icon/octicon (:override (icon str &rest plist) mantle:override)
    "Return string of Octicon ICON and STR.

Optional PLIST's optional keys:
  - `:height'     - HEIGHT
  - `:v-adjust'   - V-ADJUST
  - `:color:icon' - ICON-COLOR

HEIGHT and V-ADJUST are sent to `all-the-icons-octicon'.

ICON-COLOR is used to color only the icon character.

[2022-02-04] https://gist.github.com/mbuczko/e15d61363d31cf78ff17427072e0c325"
    ;; Only bother propertizing if we need to.
    (let ((face (if (plist-member plist :color:icon)
                    (list :foreground (plist-get plist :color:icon))
                  nil)))

      (concat
       (all-the-icons-octicon icon
                              :face face
                              :v-adjust (or (plist-get plist :v-adjust) 0)
                              :height (or (plist-get plist :height) 1))
       " "
       str)))


  (define-advice mantle:user:icon/material (:override (icon str &rest plist) mantle:override)
    "Return string of Material ICON and STR.

Optional PLIST's optional keys:
  - `:height'     - HEIGHT
  - `:v-adjust'   - V-ADJUST
  - `:color:icon' - ICON-COLOR

HEIGHT and V-ADJUST are sent to `all-the-icons-material'.

ICON-COLOR is used to color only the icon character.

[2022-02-04] https://gist.github.com/mbuczko/e15d61363d31cf78ff17427072e0c325"
    ;; Only bother propertizing if we need to.
    (let ((face (if (plist-member plist :color:icon)
                    (list :foreground (plist-get plist :color:icon))
                  nil)))

      (concat
       (all-the-icons-material icon
                               :face face
                               :v-adjust (or (plist-get plist :v-adjust) 0)
                               :height (or (plist-get plist :height) 1))
       " "
       str)))


  (define-advice mantle:user:icon:for-mode (:override (icon str &rest plist) mantle:override)
    "Return string of MODE's icon and STR.

Optional PLIST's optional keys:
  - `:separator'  - SEPARATOR
  - `:height'     - HEIGHT
  - `:v-adjust'   - V-ADJUST
  - `:color:icon' - ICON-COLOR

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

[2022-02-04] https://gist.github.com/mbuczko/e15d61363d31cf78ff17427072e0c325"
    ;; Only bother propertizing if we need to.
    (let* ((height    (plist-get plist :height))
           (v-adjust  (or (plist-get plist :v-adjust)
                          (if (eq major-mode 'emacs-lisp-mode)
                              0.0
                            0.05)))
           (face      (if (plist-member plist :color:icon)
                          (list :foreground (plist-get plist :color:icon))
                        nil))
           (separator (if (stringp (plist-get plist :separator))
                          (plist-get plist :separator)
                        " "))
           (icon      (all-the-icons-icon-for-mode mode
                                                   :face     face
                                                   :height   height
                                                   :v-adjust v-adjust))
           (icon      (if (symbolp icon)
                          ;; Failed to find a specific icon for the mode; fallback:
                          (all-the-icons-octicon "file-text"
                                                 :face     face
                                                 :height   height
                                                 :v-adjust v-adjust)
                        icon)))

      (concat icon separator str))))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'config 'user 'all-the-icons)

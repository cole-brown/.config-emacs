;;; mantle/config/hydra.el - hydras, pretty and otherwise -*- lexical-binding: t; -*-
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
;; `hydra':
;;   https://github.com/abo-abo/hydra
;;
;; `pretty-hydra' & `major-mode-hydra':
;;   https://github.com/jerrypnz/major-mode-hydra.el
;;
;; `hydra-posframe':
;;   https://github.com/Ladicle/hydra-posframe
;;
;;; Code:


;; ┌──────────────────────────────────═══───────────────────────────────────┐ ;;
;; │                                 ═════                                  │ ;;
;; │           Multi-Headed Monster - Major, Pretty, and Original           │ ;;
;; │                                 ═════                                  │ ;;
;; └──────────────────────────────────═══───────────────────────────────────┘ ;;


;;------------------------------------------------------------------------------
;; Hydra
;;------------------------------------------------------------------------------

(imp:use-package hydra)


;;------------------------------------------------------------------------------
;; Major-Mode & Pretty Hydra
;;------------------------------------------------------------------------------
;; NOTE: Major-Mode Hydra includes Pretty-Hydra, but you can install
;; Pretty-Hydra on its own if you want...
;;   https://github.com/jerrypnz/major-mode-hydra.el
;;   https://github.com/jerrypnz/major-mode-hydra.el#pretty-hydra

;; NOTE: Slow Rendering
;; If you experience a slow down in performance when rendering multiple icons
;; simultaneously, you can try setting the following (emacs) variable:
;;   (setq inhibit-compacting-font-caches t)

;; NOTE: Troubleshooting
;;   TODO: Some of my Font Awesome icons are wrong, so I might have to do this...
;; https://github.com/domtronn/all-the-icons.el#troubleshooting

;; TODO: Move to "mantle/config/all-the-icon.el" or whatever.
;;------------------------------
;; The Missing Functions:
;;------------------------------
;; NOTE [2022-02-04]:
;; Docs for Pretty Hydra use some of these in their example, which is great,
;; except they don't exist... So I found a GitHub Gist that defines them:
;;   https://gist.github.com/mbuczko/e15d61363d31cf78ff17427072e0c325"


(require 'all-the-icons)

(imp:require :str)

;; TODO: Move to mis.
(defun with-faicon (icon str &rest plist)
  "Return string of Font Awesome ICON and STR.

Optional PLIST's optional keys:
  - `:height'     - HEIGHT
  - `:v-adjust'   - V-ADJUST
  - `:color:icon' - ICON-COLOR

Passes HEIGHT and V-ADJUST to `all-the-icons-faicon'.

Uses ICON-COLOR's font to color the icon character.

[2022-02-04] https://gist.github.com/mbuczko/e15d61363d31cf78ff17427072e0c325"
  ;; Only bother propertizing if we need to.
  (let ((properties
         output))
    (when-let ((color:icon (plist-get plist :color:icon)))
      ;; figure out font or color to use?
      ;; Figure out what to call to propertize string w/ color/font.
      (setq properties (list :foreground color:icon)))

    (s-concat
     (all-the-icons-faicon icon
                           :face properties
                           :v-adjust (or (plist-get plist :v-adjust) 0)
                           :height (or (plist-get plist :height) 1))
     " "
     str)))
;; (with-faicon "spotify" "Spotify" :color:icon "limegreen" :height 1 :v-adjust -0.05)
;; (insert (with-faicon "spotify" "Spotify" :color:icon "limegreen" :height 1 :v-adjust -0.05))


;; TODO: Move to mis.
;; TODO: Change to plist like `with-faicon'.
(defun with-fileicon (icon str &optional height v-adjust)
  "Return string of File ICON and STR.

Passes HEIGHT and V-ADJUST to `all-the-icons-fileicon'.

[2022-02-04] https://gist.github.com/mbuczko/e15d61363d31cf78ff17427072e0c325"
  (s-concat (all-the-icons-fileicon icon
                                    :v-adjust (or v-adjust 0)
                                    :height (or height 1))
            " "
            str))


;; TODO: Move to mis.
;; TODO: Change to plist like `with-faicon'.
(defun with-octicon (icon str &optional height v-adjust)
  "Return string of Octicon ICON and STR.

Passes HEIGHT and V-ADJUST to `all-the-icons-octicon'.

[2022-02-04] https://gist.github.com/mbuczko/e15d61363d31cf78ff17427072e0c325"
  (s-concat (all-the-icons-octicon icon
                                   :v-adjust (or v-adjust 0)
                                   :height (or height 1))
            " "
            str))


;; TODO: Move to mis.
;; TODO: Change to plist like `with-faicon'.
(defun with-material (icon str &optional height v-adjust)
  "Return string of Material ICON and STR.

Passes HEIGHT and V-ADJUST to `all-the-icons-material'.

[2022-02-04] https://gist.github.com/mbuczko/e15d61363d31cf78ff17427072e0c325"
  (s-concat (all-the-icons-material icon
                                    :v-adjust (or v-adjust 0)
                                    :height (or height 1))
            " "
            str))


;; TODO: Move to mis.
;; TODO: Change to plist like `with-faicon'.
(defun with-mode-icon (mode str &optional height no-space face)
  "Return string of MODE's icon and STR.

If HEIGHT and/or FACE are non-nil, passes them to `all-the-icons-icon-for-mode'.

Automatically sets `:v-adjust' based on... `major-mode'???
  - IDK.

If NO-SPACE is nil, adds a space between MODE's icon and STR.

[2022-02-04] https://gist.github.com/mbuczko/e15d61363d31cf78ff17427072e0c325"
  (let* ((v-adjust (if (eq major-mode 'emacs-lisp-mode) 0.0 0.05))
         (args     `(:height ,(or height 1) :v-adjust ,v-adjust))
         (_         (when face
                      (lax-plist-put args :face face)))
         (icon     (apply #'all-the-icons-icon-for-mode mode args))
         (icon     (if (symbolp icon)
                       (apply #'all-the-icons-octicon "file-text" args)
                     icon)))
    (s-concat icon (if no-space "" " ") str)))


;;------------------------------------------------------------------------------
;; Major-Mode Hydra
;;------------------------------------------------------------------------------
;; https://github.com/jerrypnz/major-mode-hydra.el#major-mode-hydra

(imp:use-package major-mode-hydra
  ;; If you want a keybind:
  ;; :bind
  ;; ("M-SPC" . major-mode-hydra)
  ;; NOTE: If desired, maybe do with `:general' keyword instead so it works
  ;; better with `general' & `evil'? Except that might introduce a circular
  ;; dependency if `general' is still wanting `pretty-hydra' for the 'SPC'
  ;; leader key function.
  )


;;------------------------------------------------------------------------------
;; Pretty Hydra
;;------------------------------------------------------------------------------
;; https://github.com/jerrypnz/major-mode-hydra.el#pretty-hydra

;; `with-faicon'
;;   - Uses `all-the-icons-faicon'.
;;   - aka "With Font Awesome icon"
;;
;; All-the-Icons: Inserting Icons Directly
;;   https://github.com/domtronn/all-the-icons.el#inserting-icons-directly
;;
;; The icon alists are prefixed with: `all-the-icons-data/'
;;   (pp all-the-icons-data/fa-icon-alist)

;;
;;
;; No package setup, currently. `major-mode-hydra' pulls it in.
;;
;;


;;------------------------------------------------------------------------------
;; Hydra Posframe
;;------------------------------------------------------------------------------

;; [2022-07-13] This can cover up what I'm trying to do in the buffer, which is
;; very annoying and not easy to fix on the spot? Skip using it while I think
;; about it.
;; (imp:use-package hydra-posframe
;;   ;;--------------------
;;   :hook
;;   ;;--------------------
;;   (after-init . hydra-posframe-enable)
;;
;;   ;;--------------------
;;   :custom
;;   ;;--------------------
;;   ;; Defaults to centered, which is a bit far from any feedback printed into the minibuffer...
;;   (hydra-posframe-poshandler 'posframe-poshandler-frame-center)
;;   ;; TODO: If center is no good, try one of these:
;;   ;; (hydra-posframe-poshandler 'posframe-poshandler-frame-bottom-center)
;;   ;; (hydra-posframe-poshandler 'posframe-poshandler-frame-bottom-left-corner)
;;   )


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'config 'user 'hydra)

;;; mantle/config/common.el --- General Development Environment Stuff -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2022-08-05
;; Modified:   2022-08-05
;; URL:        https://github.com/cole-brown/.config-emacs
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;  General Development Environment Stuff
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Color Codes
;;------------------------------------------------------------------------------

;; Set the background color of color strings to the color they represent.
;; e.g.:
;;   - #ff1100
;;   - #abc
;;   - rgb(100, 100, 100)
;;   - YellowGreen
;;   - etc
;; https://github.com/emacsmirror/rainbow-mode/blob/master/rainbow-mode.el
;; https://elpa.gnu.org/packages/rainbow-mode.html
(imp:use-package rainbow-mode

  ;;--------------------
  :init
  ;;--------------------

  ;;---
  ;; Create a hook for enabling this minor mode.
  ;;---
  ;; Creates a func called `mantle:hook:rainbow-mode/enable'.
  ;;   (innit:hook:func/name:symbol "rainbow-mode/enable" nil)
  (innit:hook:defun
      (:name "rainbow-mode/enable"
       :file (path:current:file)
       :docstr "Enable `rainbow-mode' (colorize color codes) for this buffer."
       :quiet t)
    (rainbow-mode +1))

  ;;--------------------
  :hook
  ;;--------------------

  ((org-mode-hook    . mantle:hook:rainbow-mode/enable)
   (csharp-mode-hook . mantle:hook:rainbow-mode/enable)
   (css-mode-hook   . mantle:hook:rainbow-mode/enable)
   (php-mode-hook   . mantle:hook:rainbow-mode/enable)
   (html-mode-hook   . mantle:hook:rainbow-mode/enable)))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'config 'user 'dev-env 'common)

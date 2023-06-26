;;; core/modules/emacs/window/init.el --- Emacs Windows -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2022-07-15
;; Timestamp:  2023-06-26
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Emacs Windows
;; Not:
;;   - Windows the operating system
;;   - The operating system's windows
;;   - Emacs frames, which are basically the operating system's windows
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Set up imp.
;;------------------------------------------------------------------------------

(imp:path:root/set :window
                   (imp:path:current:dir)
                   "init.el")


;;------------------------------------------------------------------------------
;; Load files.
;;------------------------------------------------------------------------------

(imp:timing
    '(:window)
    (imp:file:current)
    (imp:path:current:dir)

  (imp:load :feature  '(:window manage)
            :filename "manage"))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :window)

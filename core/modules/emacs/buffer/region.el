;;; core/modules/emacs/buffer/region.el --- Buffer Region Functions -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2021-09-22
;; Modified:   2021-09-22
;; URL:        https://github.com/cole-brown/.config-emacs
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;  Buffer Region Functions
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Predicates
;;------------------------------------------------------------------------------

(defun buffer:region:active? ()
  "Return non-nil if selection is active.
Will detect 'evil' visual mode as well."
  (declare (side-effect-free t))
  (or (use-region-p)
      (and (bound-and-true-p evil-local-mode)
           (evil-visual-state-p))))


;;------------------------------------------------------------------------------
;; Region Helpers
;;------------------------------------------------------------------------------

(defun buffer:region:beginning ()
  "Return beginning position of selection.
Use `evil-visual-beginning' if available."
  (declare (side-effect-free t))
  (or (and (bound-and-true-p evil-local-mode)
           (markerp evil-visual-beginning)
           (marker-position evil-visual-beginning))
      (region-beginning)))


(defun buffer:region:end ()
  "Return end position of selection.
Use `evil-visual-end' if available."
  (declare (side-effect-free t))
  (if (bound-and-true-p evil-local-mode)
      evil-visual-end
    (region-end)))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :buffer 'region)

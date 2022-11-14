;;; predicates.el --- Predicate Functions -*- lexical-binding: t; -*-
;;
;; Author:   Cole Brown <code@brown.dev>
;; URL:      https://github.com/cole-brown/.config-emacs
;; Created:  2022-11-14
;; Modified: 2022-11-14
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;  Predicate Functions
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Evil
;;------------------------------------------------------------------------------

(defun elisp:evil? ()
  "Does `evil' exist and is it enabled?"
  (and (featurep 'evil)
       evil-mode))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :elisp 'utils 'predicates)

;;; mis-pkg.el --- Make It So -*- lexical-binding: t; -*-
;;
;; Author:           Cole Brown <code@brown.dev>
;; Maintainer:       Cole Brown <code@brown.dev>
;; URL:              https://github.com/cole-brown/.config-emacs
;; Created:          2019-10-23
;; Modified:         2022-08-08
;; Version:          4.0.0
;; Keywords:         convenience extensions faces
;; Package-Requires: ((emacs "28"))
;;
;;; Commentary:
;;
;; mis ("Make It So") is a rich(er) text cousin to `message'.
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Define Package (???)
;;------------------------------------------------------------------------------
;; Obsolete in Emacs 28.1, but no documentation to say what to switch to.
;; Maybe when Emacs 29 comes along and they delete `define-package' there'll be
;; a replacement? Already `define-package' is just:
;;   (error "Don't call me")
;; ...Which isn't very helpful.
(define-package "mis" "4.0.0"
  "mis (\"Make It So\") is a rich(er) text cousin to `message'."
  '((emacs "28"))
  ;; TODO: Change when mis gets its own repository
  :homepage "https://github.com/cole-brown/.config-emacs"
  :keywords '("convenience" "extensions" "faces"))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'mis-pkg)
;;; mis-pkg.el ends here

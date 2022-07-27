;;; autogit-pkg.el --- auto-commit git repos -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2020-10-28
;; Modified:   2022-07-27
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Auto-commit git repos & view status on git repos.
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
(define-package "autogit" "1.0.0"
  "Auto-commit git repos & view status on git repos."
  '((emacs    "28.1")
    (magit    "3.3.0")
    (deferred "0.5.1"))
  ;; TODO: Change when autogit gets its own repository
  :homepage "https://github.com/cole-brown/.config-emacs"
  :keywords '("git" "tools" "vc"))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'autogit-pkg)
;;; autogit-pkg.el ends here

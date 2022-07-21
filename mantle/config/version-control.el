;;; mantle/config/version-control.el --- Git et al -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2022-07-21
;; Modified:   2022-07-21
;; URL:        https://github.com/cole-brown/.config-emacs
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;  Git et al
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Magit & Friends
;;------------------------------------------------------------------------------
;; The best Git Porcelain:
;;   https://magit.vc/
;;   https://github.com/magit/magit
;;
;; Plus some other version control things.
;; Whatever.
;; The important thing is: Magit.


;;------------------------------------------------------------------------------
;; Magit: Git Front-End (Porcelain)
;;------------------------------------------------------------------------------
;; The best Git Porcelain.
;;   https://magit.vc/
;;   https://github.com/magit/magit

(imp:use-package magit
  ;;--------------------
  :general
  ;;--------------------
  ;; Get into the global leader.
  (:prefix keybind:leader/global:key
   :states '(normal visual motion)

   ;; Make a magit leader.
   (:prefix "g" ;; TODO: prefix name?
    ;; TODO: More keybinds!
    "g" 'magit-status
    )))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'config 'user 'version-control)

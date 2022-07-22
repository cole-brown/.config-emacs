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
  ;; Put Magit Leader under the Global Leader
  ;;---
  (:prefix  (keybind:leader :global "g") ;; TODO: prefix name?
   :states  keybind:leader/global:states
   :keymaps keybind:leader/global:keymaps
   ;; Title
   "" '(nil :which-key "Magit / Version Control")

   ;;---
   ;; Magit Keybinds
   ;;---
   "g" 'magit-status

   ;; TODO: More keybinds!
   ;; TODO: And use `:repeat' / `:jump'?
   ;;   https://github.com/noctuid/general.el#evil-command-properties
   ;;   https://github.com/noctuid/evil-guide#command-properties
   ;;
   ;; (general-define-key
   ;;  :keymaps 'normal
   ;;  :prefix "SPC"
   ;;  "gj" '(git-gutter:next-hunk :properties (:repeat t :jump t))
   ;;  "gk" '(git-gutter:previous-hunk :repeat t :jump t))
   ;;
   ;; ;; they also work globally
   ;; (general-define-key
   ;;  :keymaps 'normal
   ;;  :prefix "SPC"
   ;;  :properties '(:repeat t :jump t)
   ;;  ;; or
   ;;  :repeat t
   ;;  :jump t
   ;;  "gj" 'git-gutter:next-hunk
   ;;  "gk" 'git-gutter:previous-hunk)
   ))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'config 'user 'version-control)

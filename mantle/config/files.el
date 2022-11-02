;;; mantle/config/files.el --- File & Directory Settings -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2022-07-29
;; Modified:   2022-07-29
;; URL:        https://github.com/cole-brown/.config-emacs
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;  File & Directory Settings
;;
;;; Code:

;;------------------------------------------------------------------------------
;; recentf for recent files
;;------------------------------------------------------------------------------

;; http://pages.sachachua.com/.emacs.d/Sacha.html#org0676afd
(imp:use-package recentf
  ;;--------------------
  :custom
  ;;--------------------

  ;; Clean up the recent list when Emacs has been idle for over 30 seconds.
  (recentf-auto-cleanup    30)

  ;; Default is 20. Doom sets to 200 and that was occasionally too low.
  (recentf-max-saved-items 1000)

  ;; How many saved items to /show/.
  (recentf-max-menu-items  20)


  ;;--------------------
  :config
  ;;--------------------

  ;; Periodically save the list of recent files: https://www.emacswiki.org/emacs/RecentFiles#toc1
  ;; Otherwise they're only saved during a graceful shutdown.
  (run-with-timer (* 30 60) ;; Wait 30 mins to run.
                  (* 30 60) ;; Repeat every 30 mins.
                  'recentf-save-list)

  ;; Recentf and TRAMP need some peace-keeping to get along.
  ;; https://lists.gnu.org/archive/html/bug-gnu-emacs/2007-07/msg00019.html
  (add-to-list 'recentf-keep 'file-remote-p)

  ;; Excluded Files/Dirs:
  ;;---
  ;; NOTE: `no-littering' has the set-up for adding its dirs to `recentf-exclude'.
  ;;   See:    core/boot/10-init/00-bootstrap.el
  ;;   Search: recentf-exclude
  ;;---
  ;;
  ;; No others to exclude that I know of, currently.
  ;; (add-to-list 'recentf-exclude <path>)

  ;; Enable!
  (recentf-mode))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'config 'user 'files)

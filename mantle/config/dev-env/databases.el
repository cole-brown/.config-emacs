;;; mantle/config/dev-env/databases.el --- DB languages, etc -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2022-12-08
;; Modified:   2022-12-08
;; URL:        https://github.com/cole-brown/.config-emacs
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;  DB languages, etc
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Microsoft Azure Cloud
;;------------------------------------------------------------------------------

;; Query language for Application Insights is called `kusto':
;;   https://learn.microsoft.com/en-us/azure/data-explorer/kusto/query/
;;
;; And this is the only emacs package I could find about it:
;;   https://github.com/ration/kusto-mode.el
(imp:use-package kusto-mode
  :straight (:type git
             :host github
             :repo "ration/kusto-mode.el")

  ;;--------------------
  :custom
  ;;--------------------

  ;; Alist mapping forge `:host' symbols to username strings. Used to compute
  ;; the repo URL when the `:fork' keyword is used in a recipe.
  ;;   https://github.com/radian-software/straight.el#but-what-about-my-fork-of-obscure-el-package
  (straight-host-usernames '((github . "cole-brown")
                             ;; (gitlab . "cole-brown")
                             ;; (codeberg . "cole-brown")
                             ;; (sourcehut . "cole-brown")
                             ;; (bitbucket . "cole-brown")
                             )))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'config 'user 'dev-env 'databases)

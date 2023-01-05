;;; mantle/config/dev-env/databases.el --- DB languages, etc -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2022-12-08
;; Modified:   2023-01-05
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
;; Kusto (Microsoft Azure Cloud)
;;------------------------------------------------------------------------------

;; Query language for Application Insights is called `kusto':
;;   https://learn.microsoft.com/en-us/azure/data-explorer/kusto/query/
;;
;; And this is the only emacs package I could find about it:
;;   https://github.com/ration/kusto-mode.el
(imp:use-package kusto-mode
  :straight (:type git
             :host github
             :repo "ration/kusto-mode.el"))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'config 'user 'dev-env 'databases)

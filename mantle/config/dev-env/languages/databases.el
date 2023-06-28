;;; mantle/config/dev-env/databases.el --- DB languages, etc -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2022-12-08
;; Timestamp:  2023-06-28
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; DB languages, etc
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
;; SQL
;;------------------------------------------------------------------------------

;;------------------------------
;; Microsoft SQL
;;------------------------------

;; TODO-lsp: Use this LSP for MS SQL?
;;   https://github.com/emacs-lsp/lsp-mssql


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'config 'user 'dev-env 'languages 'databases)

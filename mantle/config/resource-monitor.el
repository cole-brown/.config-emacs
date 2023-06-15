;;; mantle/config/resource-monitor.el --- Monitor Emacs' Resource Usage -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2023-06-15
;; Timestamp:  2023-06-15
;; URL:        https://github.com/cole-brown/.config-emacs
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Monitor Emacs' Resource Usage
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Memory Usage
;;------------------------------------------------------------------------------

(imp:use-package memory-usage)


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'config 'resource-monitor)

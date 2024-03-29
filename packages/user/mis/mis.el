;;; mis.el --- Make It So -*- lexical-binding: t; -*-
;;
;; Author:           Cole Brown <code@brown.dev>
;; Maintainer:       Cole Brown <code@brown.dev>
;; URL:              https://github.com/cole-brown/.config-emacs
;; Created:          2019-10-23
;; Timestamp:        2023-06-29
;; Version:          4.0.0
;; Keywords:         convenience extensions faces
;; Package-Requires: ((emacs "24.3"))
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; mis ("Make It So") is a rich(er) text cousin to `message'.
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Mis Custom Group
;;------------------------------------------------------------------------------

(defgroup mis:group nil
  "Automatically-ish commit/push git repos for note, docs, etc."
  :prefix "mis:"
  :group 'text)


;;------------------------------------------------------------------------------
;; Mis Files
;;------------------------------------------------------------------------------

;;------------------------------
;; Internal
;;------------------------------
(require 'mis-error)
(require 'mis-int-string)
(require 'mis-tree-syntax)
(require 'mis-valid)
(require 'mis-tree-output)
(require 'mis-parse)
(require 'mis-buffer)

(require 'mis-compile)

;;------------------------------
;; Output Printing & Output API
;;------------------------------
(require 'mis-print)

;;------------------------------
;; Input API & Output Styler
;;------------------------------
(require 'mis-style)

;;------------------------------
;; Input APIs, Type Compilers, & Stylers
;;------------------------------
(require 'mis-string)
(require 'mis-format)
(require 'mis-trim)
(require 'mis-indent)
(require 'mis-align)
(require 'mis-art)
(require 'mis-comment)

;;------------------------------
;; Mis' Top-Level API
;;------------------------------
(require 'mis-mis)


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'mis)
;;; mis.el ends here

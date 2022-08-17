;;; mis.el --- Make It So -*- lexical-binding: t; -*-
;;
;; Author: Cole Brown <code@brown.dev>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:  2019-10-23
;; Modified: 2022-08-08
;; Version:  4.0.0
;; Keywords: convenience extensions faces
;; Homepage: https://github.com/work/mis
;; Package-Requires: ((emacs "24.3"))
;;
;;; Commentary:
;;
;; mis ("Make It So") is a rich(er) text cousin to `message'.
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Mis Files
;;------------------------------------------------------------------------------

;; Low level funcs.
(require 'mis-error)
(require 'mis-valid)
(require 'mis-buffer)
(require 'mis-format)

;; Contains at least one public APIs.
;; Possibly some low level funcs.
(require 'mis-align)
(require 'mis-style)
(require 'mis-art)
(require 'mis-comment)

;; Top level.
(require 'mis-message)


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'mis)
;;; mis.el ends here

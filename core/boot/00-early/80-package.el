;;; core/boot/00-early/80-package.el --- Early Init: Packages -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2023-06-20
;; Timestamp:  2023-06-20
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; NOTE: This is during 'early-init.el'!
;;
;; Any 'early-init.el' parts of initializing Emacs package system(s).
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Package Set-Up
;;------------------------------------------------------------------------------

;; `package-initialize` is called between "early-init.el" and "init.el", so we
;; need to be all set-up and ready by the end of "early-init.el".
(innit:package:init/early)


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :core 'boot 'early 'package)

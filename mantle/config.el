;;; mantle/init.el --- User-Level Emacs Initialization -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2022-03-25
;; Timestamp:  2023-06-29
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; User-Level Emacs Configuration
;;
;; The user's `use-package', settings, etc. should happen in this part of
;; start-up.
;;
;;
;;; Code:


;;------------------------------------------------------------------------------
;; User Config
;;------------------------------------------------------------------------------

;; Give user a folder & files to do with whatever they want.
(imp:load :feature  '(:mantle config)
          :path     (imp:path:join innit:path:mantle "config")
          :filename "init")


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'lower 'config)

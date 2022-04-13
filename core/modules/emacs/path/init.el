;;; init.el --- Init for :emacs/path module. -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Cole Brown
;;
;; Author: Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created: October 22, 2020
;; Modified: 2022-04-13 09:13:50
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Initialize the spy/path module.
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Set imp Root
;;------------------------------------------------------------------------------

(imp:path:root :path
               (imp:path:current:dir)
               "init.el")


;;------------------------------------------------------------------------------
;; Load Files
;;------------------------------------------------------------------------------

;; Always load.
(imp:load :feature  '(:path path)
          :filename "path")
(imp:load :feature  '(:path files)
          :filename "files")
(imp:load :feature  '(:path regex)
          :filename "regex")


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :path)

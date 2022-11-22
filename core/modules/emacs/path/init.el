;;; core/modules/emacs/path/init.el --- Init for `:path' module. -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <code@brown.dev>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2020-10-22
;; Modified:   2022-11-22
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Initialize the `:path' module.
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

(imp:timing
    :path
    "init.el"
    (imp:path:current:dir)


  ;; Always load.
  (imp:load :feature  '(:path path)
            :filename "path")
  (imp:load :feature  '(:path files)
            :filename "files")
  (imp:load :feature  '(:path regex)
            :filename "regex")
  (imp:load :feature  '(:path git)
            :filename "git")
  (imp:load :feature  '(:path buffer)
            :filename "buffer")


  ;; End load timing.
  )


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :path)

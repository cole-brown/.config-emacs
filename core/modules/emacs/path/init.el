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
;; Does `imp' exist?
;;------------------------------------------------------------------------------
;; Let's be complicated...
;;   - If `imp' already exists, use it for our file loading.
;;     - We're a regular ol' package/module/whatever.
;;   - Otherwise use Emacs' `load' function to load our files.
;;     - We're a super-early-in-init package/module/whatever.

(if (featurep 'imp)
    ;;--------------------------------------------------------------------------
    ;; Use `imp'
    ;;--------------------------------------------------------------------------
    (progn
      ;;------------------------------
      ;; Set up root.
      ;;------------------------------
      (imp:path:root :path
                     (imp:path:join doom-private-dir
                                    "modules"
                                    "emacs"
                                    "path")
                     "init.el")

      ;;------------------------------
      ;; Load Files
      ;;------------------------------
      (imp:load :feature  '(:path path)
                :filename "path")
      (imp:load :feature  '(:path files)
                :filename "files")
      (imp:load :feature  '(:path regex)
                :filename "regex"))

  ;;----------------------------------------------------------------------------
  ;; No `imp', load files ourself.
  ;;----------------------------------------------------------------------------
  ;; This `cond' is basically `path:current:file'.
  (if-let* ((current-file (cond
                           ;;------------------------------
                           ;; Look for a valid "current file" variable.
                           ;;------------------------------
                           ((bound-and-true-p byte-compile-current-file))

                           (load-file-name)

                           ((stringp (car-safe current-load-list))
                            (car current-load-list))

                           (buffer-file-name)

                           ;;------------------------------
                           ;; Error: Didn't find anything valid.
                           ;;------------------------------
                           ((error "path/init.el: Cannot get this file's path"))))
            (current-dir (directory-file-name (file-name-directory current-file))))
      (progn
        ;;------------------------------
        ;; Load Files
        ;;------------------------------
        ;; Load same files in same order as above in `imp' section.
        (load (expand-file-name "path" current-dir))
        (load (expand-file-name "files" current-dir))
        (load (expand-file-name "regex" current-dir)))

    (error "path/init.el: Cannot get this file's directory path")))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(if (featurep 'imp)
    (imp:provide:with-emacs :path)
  (provide 'path))

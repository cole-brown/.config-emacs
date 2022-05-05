;;; 20-load.el --- Set up loads. -*- lexical-binding: t; -*-
;;
;; Author: Cole Brown <code@brown.dev>
;; URL:    https://github.com/cole-brown/.config-emacs
;;
;;; Commentary:
;;; Code:


;;------------------------------------------------------------------------------
;; `imp' root paths
;;------------------------------------------------------------------------------

;; NOTE: 'mantle/' directory is available/encouraged for users to put their
;; Emacs init stuff in.
(imp:path:root :mantle
               (imp:path:join user-emacs-directory "mantle")
               "init.el")


;; NOTE: `:core' and `:modules' already exist for `user-emacs-directory'
;; subdirectories 'core/' and 'modules/', respectively.


;;------------------------------------------------------------------------------
;; Run 'mantle/' init.
;;------------------------------------------------------------------------------

(imp:timing
    '(:mantle init)
    (imp:file:current)
    (imp:path:current:dir)

  (imp:load :feature  '(:mantle init)
            :filename "init"))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :dot-emacs 'core 'boot '10-init 'load)

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
;; Add `imp' root keywords to paths:
;;   (imp:path:root :foo  (imp:path:join user-emacs-directory "mantle" :foo))
;;   (imp:path:root :quxx (imp:path:join user-emacs-directory "mantle" "foo-barbar" "baz"))
;;
;; NOTE: "mantle" directory is available/encouraged for you to put your stuff in.
;; Your imp root should be a subdirectory, as in examples above.
;;
;; NOTE: `:core' and `:modules' already exist for `user-emacs-directory'
;; subdirectories "core/" and "modules/", respectively.



;;------------------------------------------------------------------------------
;; "init.el" and "config.el"
;;------------------------------------------------------------------------------
;; Add `imp' feature lists to be loaded if correct file is present at
;; imp root path.
;;   - "init.el" will be checked for after core init is run.
;;   - "config.el" will be checked for after core config is run.
;;
;; (init:feature:mantle:add "core/10-init/20-load.el" :foo)
;; (init:feature:mantle:add "core/10-init/20-load.el" :zort narf)


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :dot-emacs 'core 'boot '10-init 'load)

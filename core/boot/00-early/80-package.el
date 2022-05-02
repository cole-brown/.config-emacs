;;; core/boot/00-early/80-package.el -*- lexical-binding: t; -*-


;;------------------------------------------------------------------------------
;; Package Set-Up
;;------------------------------------------------------------------------------

;; `package-initialize` is called between "early-init.el" and "init.el", so we
;; need to be all set-up and ready by the end of "early-init.el".
(innit:package:init/early)


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :dot-emacs 'core 'boot '00-early 'package)

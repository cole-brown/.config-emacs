;;; mantle/config/dev-env/languages/init.el --- Config the Lingua Franca -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2022-12-12
;; Modified:   2023-01-05
;; URL:        https://github.com/cole-brown/.config-emacs
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;  Config the Lingua Franca
;;
;;; Code:


;;------------------------------------------------------------------------------
;; General / Common / Whatever
;;------------------------------------------------------------------------------

(imp:load :feature  '(:mantle config user dev-env languages common)
          :path     (imp:path:current:dir/relative :mantle)
          :filename "common")


;;------------------------------------------------------------------------------
;; Programming Languages
;;------------------------------------------------------------------------------

(imp:load :feature  '(:mantle config user dev-env languages c-and-cpp)
          :path     (imp:path:current:dir/relative :mantle)
          :filename "c-and-cpp")


(imp:load :feature  '(:mantle config user dev-env languages csharp)
          :path     (imp:path:current:dir/relative :mantle)
          :filename "csharp")


(imp:load :feature  '(:mantle config user dev-env languages emacs-lisp)
          :path     (imp:path:current:dir/relative :mantle)
          :filename "emacs-lisp")


(imp:load :feature  '(:mantle config user dev-env languages python)
          :path     (imp:path:current:dir/relative :mantle)
          :filename "python")


(imp:load :feature  '(:mantle config user dev-env languages shell)
          :path     (imp:path:current:dir/relative :mantle)
          :filename "shell")


;;------------------------------------------------------------------------------
;; Markup Languages
;;------------------------------------------------------------------------------

(imp:load :feature  '(:mantle config user dev-env languages json)
          :path     (imp:path:current:dir/relative :mantle)
          :filename "json")


(imp:load :feature  '(:mantle config user dev-env languages markdown)
          :path     (imp:path:current:dir/relative :mantle)
          :filename "markdown")


(imp:load :feature  '(:mantle config user dev-env languages toml)
          :path     (imp:path:current:dir/relative :mantle)
          :filename "toml")


(imp:load :feature  '(:mantle config user dev-env languages yaml)
          :path     (imp:path:current:dir/relative :mantle)
          :filename "yaml")


;;------------------------------------------------------------------------------
;; Database Languages
;;------------------------------------------------------------------------------

(imp:load :feature  '(:mantle config user dev-env languages databases)
          :path     (imp:path:current:dir/relative :mantle)
          :filename "databases")


;;------------------------------------------------------------------------------
;; Web
;;------------------------------------------------------------------------------

(imp:load :feature  '(:mantle config user dev-env languages http)
          :path     (imp:path:current:dir/relative :mantle)
          :filename "http")


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'config 'user 'dev-env 'languages)

;;; mantle/config/dev-env/languages/init.el --- Config the Lingua Franca -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2022-12-12
;; Modified:   2022-12-12
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
;; Languages
;;------------------------------------------------------------------------------

(imp:load :feature  '(:mantle config user dev-env c-and-cpp)
          :path     (imp:path:current:dir/relative :mantle)
          :filename "c-and-cpp")


(imp:load :feature  '(:mantle config user dev-env csharp)
          :path     (imp:path:current:dir/relative :mantle)
          :filename "csharp")


(imp:load :feature  '(:mantle config user dev-env emacs-lisp)
          :path     (imp:path:current:dir/relative :mantle)
          :filename "emacs-lisp")


(imp:load :feature  '(:mantle config user dev-env http)
          :path     (imp:path:current:dir/relative :mantle)
          :filename "http")


(imp:load :feature  '(:mantle config user dev-env json)
          :path     (imp:path:current:dir/relative :mantle)
          :filename "json")


(imp:load :feature  '(:mantle config user dev-env markdown)
          :path     (imp:path:current:dir/relative :mantle)
          :filename "markdown")


(imp:load :feature  '(:mantle config user dev-env python)
          :path     (imp:path:current:dir/relative :mantle)
          :filename "python")


(imp:load :feature  '(:mantle config user dev-env toml)
          :path     (imp:path:current:dir/relative :mantle)
          :filename "toml")


(imp:load :feature  '(:mantle config user dev-env yaml)
          :path     (imp:path:current:dir/relative :mantle)
          :filename "yaml")


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'config 'user 'dev-env 'languages)

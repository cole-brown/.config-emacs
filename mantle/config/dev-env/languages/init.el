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
;; Lispses
;;------------------------------------------------------------------------------

(imp:load :feature  '(:mantle config user dev-env emacs-lisp)
          :path     (imp:path:current:dir/relative :mantle)
          :filename "emacs-lisp")


;;------------------------------------------------------------------------------
;; C and Friends
;;------------------------------------------------------------------------------

(imp:load :feature  '(:mantle config user dev-env c-and-cpp)
          :path     (imp:path:current:dir/relative :mantle)
          :filename "c-and-cpp")


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'config 'user 'dev-env 'languages)
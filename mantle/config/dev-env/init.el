;;; mantle/config/dev-env/init.el --- Config the Development Environment -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2022-08-05
;; Modified:   2022-08-05
;; URL:        https://github.com/cole-brown/.config-emacs
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;  Config the Development Environment
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Taskspace
;;------------------------------------------------------------------------------

(imp:load :feature  '(:mantle config user dev-env taskspace)
          :path     (imp:path:current:dir/relative :mantle)
          :filename "taskspace")


;;------------------------------------------------------------------------------
;; General / Common / Whatever
;;------------------------------------------------------------------------------

(imp:load :feature  '(:mantle config user dev-env common)
          :path     (imp:path:current:dir/relative :mantle)
          :filename "common")

(imp:load :feature  '(:mantle config user dev-env snippets)
          :path     (imp:path:current:dir/relative :mantle)
          :filename "snippets")

(imp:load :feature  '(:mantle config user dev-env compile)
          :path     (imp:path:current:dir/relative :mantle)
          :filename "compile")


;;------------------------------------------------------------------------------
;; Version Control
;;------------------------------------------------------------------------------
;; Git and... well just Git, really.
;; And by "Git" I mean, of course, Magit (& friends).

(imp:load :feature  '(:mantle config user dev-env version-control)
          :path     (imp:path:current:dir/relative :mantle)
          :filename "version-control")


;;------------------------------------------------------------------------------
;; Languages
;;------------------------------------------------------------------------------

(imp:load :feature  '(:mantle config user dev-env languages)
          :path     (imp:path:join (imp:path:current:dir/relative :mantle) "languages")
          :filename "init")


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'config 'user 'dev-env)

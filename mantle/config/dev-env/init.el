;;; mantle/config/dev-env/init.el --- Config the Development Environment -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2022-08-05
;; Timestamp:  2023-06-29
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Config the Development Environment
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Taskspace
;;------------------------------------------------------------------------------

(imp:load :feature  '(:mantle config dev-env taskspace)
          :path     (imp:path:current:dir/relative :mantle)
          :filename "taskspace")


;;------------------------------------------------------------------------------
;; General / Common / Whatever
;;------------------------------------------------------------------------------

(imp:load :feature  '(:mantle config dev-env common)
          :path     (imp:path:current:dir/relative :mantle)
          :filename "common")

(imp:load :feature  '(:mantle config dev-env snippets)
          :path     (imp:path:current:dir/relative :mantle)
          :filename "snippets")

(imp:load :feature  '(:mantle config dev-env compile)
          :path     (imp:path:current:dir/relative :mantle)
          :filename "compile")


;;------------------------------------------------------------------------------
;; Version Control
;;------------------------------------------------------------------------------
;; Git and... well just Git, really.
;; And by "Git" I mean, of course, Magit (& friends).

(imp:load :feature  '(:mantle config dev-env version-control)
          :path     (imp:path:current:dir/relative :mantle)
          :filename "version-control")


;;------------------------------------------------------------------------------
;; Languages
;;------------------------------------------------------------------------------

(imp:load :feature  '(:mantle config dev-env languages)
          :path     (imp:path:join (imp:path:current:dir/relative :mantle) "languages")
          :filename "init")


;;------------------------------------------------------------------------------
;; Robot Overlords
;;------------------------------------------------------------------------------

(imp:load :feature  '(:mantle config dev-env ai)
          :path     (imp:path:current:dir/relative :mantle)
          :filename "ai")


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'config 'dev-env)

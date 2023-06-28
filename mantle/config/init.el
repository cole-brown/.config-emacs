;;; mantle/config/init.el --- Configure User Stuff -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2022-04-26
;; Timestamp:  2023-06-27
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Hello again, <insert_username_here>!
;;
;; This is where we configure all your stuff.
;;
;; The bulk of your settings/setup/config should be loaded from this file.
;; `use-package' or other package loading/config should be like 90% here.
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Basics & Prereqs
;;------------------------------------------------------------------------------

;; Which-Key & Helpful
(imp:load :feature  '(:mantle config user help)
          :path     (imp:path:current:dir/relative :mantle)
          :filename "help")

(imp:load :feature  '(:mantle config user all-the-icons)
          :path     (imp:path:current:dir/relative :mantle)
          :filename "all-the-icons")


;;------------------------------------------------------------------------------
;; Operating System Things? Emacs Itself, Files, Etc
;;------------------------------------------------------------------------------

(imp:load :feature  '(:mantle config user emacs)
          :path     (imp:path:current:dir/relative :mantle)
          :filename "emacs")

(imp:load :feature  '(:mantle config user resource-monitor)
          :path     (imp:path:current:dir/relative :mantle)
          :filename "resource-monitor")

(imp:load :feature  '(:mantle config user files)
          :path     (imp:path:current:dir/relative :mantle)
          :filename "files")

(imp:load :feature  '(:mantle config user project)
          :path     (imp:path:current:dir/relative :mantle)
          :filename "project")

(imp:load :feature  '(:mantle config user ui)
          :path     (imp:path:current:dir/relative :mantle)
          :filename "ui")


;;------------------------------------------------------------------------------
;; Input
;;------------------------------------------------------------------------------

(imp:load :feature  '(:mantle config user hydra)
          :path     (imp:path:join (imp:path:current:dir/relative :mantle)
                                   "hydra")
          :filename "init")

;; Sets an `imp:flag', which is used by `evil' config to figure out which optional
;; undo system to use.
(imp:load :feature  '(:mantle config user undo-tree)
          :path     (imp:path:current:dir/relative :mantle)
          :filename "undo-tree")

(imp:load :feature  '(:mantle config user keybinds)
          :path     (imp:path:join (imp:path:current:dir/relative :mantle)
                                   "keybinds")
          :filename "init")


;;------------------------------------------------------------------------------
;; Emacs
;;------------------------------------------------------------------------------

(imp:load :feature  '(:mantle config user whitespace)
          :path     (imp:path:current:dir/relative :mantle)
          :filename "whitespace")

(imp:load :feature  '(:mantle config user perspective)
          :path     (imp:path:current:dir/relative :mantle)
          :filename "perspective")


;;------------------------------------------------------------------------------
;; Completion Frameworks
;;------------------------------------------------------------------------------
;; There's the standards like `ivy', `helm', `ido', etc...
;;
;; Or there's the new kids like the "SMOCE stack".

(imp:load :feature  '(:mantle config user completion)
          :path     (imp:path:current:dir/relative :mantle)
          :filename "completion")


;;------------------------------------------------------------------------------
;; Secrets (Consts, Vars, Etc.)
;;------------------------------------------------------------------------------

(imp:load :feature  '(:mantle config user secret)
          :path     (imp:path:current:dir/relative :mantle)
          :filename "secret")


;;------------------------------------------------------------------------------
;; Modes
;;------------------------------------------------------------------------------

(imp:load :feature  '(:mantle config user org)
          :path     (imp:path:join (imp:path:current:dir/relative :mantle)
                                   "org")
          :filename "init")


;;------------------------------------------------------------------------------
;; Development Environment
;;------------------------------------------------------------------------------

(imp:load :feature  '(:mantle config user dev-env)
          :path     (imp:path:join (imp:path:current:dir/relative :mantle)
                                   "dev-env")
          :filename "init")


;;------------------------------------------------------------------------------
;; Entertainment
;;------------------------------------------------------------------------------

(imp:load :feature  '(:mantle config spotify)
          :path     (imp:path:current:dir/relative :mantle)
          :filename "spotify")


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'config 'user)

;;; config/init.el --- Configure User Stuff -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2022-04-26
;; Modified:   2022-08-05
;; URL:        https://github.com/cole-brown/.config-emacs
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;  Configure User Stuff in This Order
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
;; Emacs, Files, Etc
;;------------------------------------------------------------------------------

(imp:load :feature  '(:mantle config user files)
          :path     (imp:path:current:dir/relative :mantle)
          :filename "files")

(imp:load :feature  '(:mantle config user ui)
          :path     (imp:path:current:dir/relative :mantle)
          :filename "ui")


;;------------------------------------------------------------------------------
;; Input
;;------------------------------------------------------------------------------

(imp:load :feature  '(:mantle config user hydra)
          :path     (imp:path:current:dir/relative :mantle)
          :filename "hydra")

(imp:load :feature  '(:mantle config user general)
          :path     (imp:path:current:dir/relative :mantle)
          :filename "general")

;; Sets `imp:flag', which is used by `evil' config to figure out which optional
;; undo system to use.
(imp:load :feature  '(:mantle config user undo-tree)
          :path     (imp:path:current:dir/relative :mantle)
          :filename "undo-tree")

;; Set up `evil' et al. Do early in config in case we want to tweak a lot along
;; the way.
(imp:load :feature  '(:mantle config user evil)
          :path     (imp:path:current:dir/relative :mantle)
          :filename "evil")


;;------------------------------
;; Keyboard Layout
;;------------------------------
;; I use Dvorak, plus I like to tweak things, so... change the basic Vim
;; keybinds and such:
(imp:load :feature  '(:mantle config user keyboard)
          :path     (imp:path:current:dir/relative :mantle)
          :filename "keyboard")


;;------------------------------
;; Keybinds
;;------------------------------

(imp:load :feature  '(:mantle config user keybinds)
          :path     (imp:path:join (imp:path:current:dir/relative :mantle)
                                   "keybinds")
          :filename "init")


;;------------------------------------------------------------------------------
;; General
;;------------------------------------------------------------------------------

;;------------------------------
;; Emacs
;;------------------------------

(imp:load :feature  '(:mantle config user whitespace)
          :path     (imp:path:current:dir/relative :mantle)
          :filename "whitespace")


;;------------------------------
;; Completion Frameworks
;;------------------------------
;; There's the standards like `ivy', `helm', `ido', etc...
;;
;; Or there's the new kids like the "SMOCE stack".

(imp:load :feature  '(:mantle config user completion)
          :path     (imp:path:current:dir/relative :mantle)
          :filename "completion")


;;------------------------------
;; Secrets (Consts, Vars, Etc.)
;;------------------------------

(imp:load :feature  '(:mantle config user secret)
          :path     (imp:path:current:dir/relative :mantle)
          :filename "secret")


;;------------------------------
;; Modes
;;------------------------------

(imp:load :feature  '(:mantle config user org-mode)
          :path     (imp:path:current:dir/relative :mantle)
          :filename "org-mode")


;;------------------------------
;; Development Environment
;;------------------------------

(imp:load :feature  '(:mantle config user dev-env)
          :path     (imp:path:join (imp:path:current:dir/relative :mantle)
                                   "dev-env")
          :filename "init")


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'config 'user)

;;; mantle/init/init.el --- User's Emacs Initialization -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2022-03-25
;; Timestamp:  2023-06-27
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Hello <insert_username_here>!
;;
;; This is for any early initialization that you want to happen before the bulk
;; of your set-up, which should happen in & under 'mantle/init/config.el'.
;; Examples:
;;   - Load any of your lisp code (helpers, etc) you want available for all of
;;     package config.
;;   - Load some secrets maybe so they're available when you need them?
;;     - e.g. API keys
;;   - `use-package' (or other package setup) for "must exist for everyone else"
;;     packages.
;;     - e.g. keybind packages like `evil', `meow', etc.
;;   - Any debugging you want maybe?
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Init: Input
;;------------------------------------------------------------------------------

;; Get the `:input' root set up.
(imp:load :feature  '(:input)
          :path     (imp:path:join innit:path:module "input")
          :filename "init")


;;------------------------------
;; General, Evil, Meow, etc...
;;------------------------------
(imp:load :feature  '(:mantle init keybinds)
          :path     (imp:path:join (imp:path:current:dir/relative :mantle)
                                   "keybinds")
          :filename "init")


;;------------------------------------------------------------------------------
;; Init: Systems & Secrets?
;;------------------------------------------------------------------------------

;;------------------------------
;; Prereq Modules
;;------------------------------

(imp:load :feature  '(:taskspace)
          :path     (imp:path:join innit:path:module "dev-env" "taskspace")
          :filename "init") ; Needed by ':mantle/theme/init'.


;;------------------------------
;; User / Identity
;;------------------------------
;; Needed early so that secret init can init signatures/identities too.

;;---
;; Modules
;;---
(imp:load :feature  '(:tools)
          :path     (imp:path:join innit:path:module "tools")
          :filename "init")

(imp:load :feature  '(:tools signature)
          :path     (imp:path:join innit:path:module "tools" "signature")
          :filename "init")

;;---
;; User Init
;;---
(imp:load :feature  '(:mantle init user identity)
          :path     (imp:path:current:dir/relative :mantle)
          :filename "identity")


;;------------------------------
;; Systems
;;------------------------------

(imp:load :feature  '(:mantle init user system)
          :path     (imp:path:current:dir/relative :mantle)
          :filename "system")


;;------------------------------
;; Secrets
;;------------------------------

(imp:load :feature  '(:mantle init user secret)
          :path     (imp:path:current:dir/relative :mantle)
          :filename "secret")


;;------------------------------------------------------------------------------
;; Init: Modules
;;------------------------------------------------------------------------------

(imp:load :feature  '(:mode org)
          :path     (imp:path:join innit:path:module "mode" "org")
          :filename "init") ; Needed by ':mantle/theme/init'.


;;------------------------------------------------------------------------------
;; Init: Theme
;;------------------------------------------------------------------------------

(imp:load :feature  '(:mantle theme init)
          :path     innit:theme:path
          :filename "init")


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'init 'user)

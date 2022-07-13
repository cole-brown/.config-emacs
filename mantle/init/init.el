;;; init/init.el --- Initialize User Stuff -*- lexical-binding: t; -*-
;;
;; Author: Cole Brown <code@brown.dev>
;; URL:    https://github.com/cole-brown/.config-emacs
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;  Initialize User Stuff in This Order
;;
;;; Code:


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
;; Init: Input
;;------------------------------------------------------------------------------

;; TODO: is this module used or is it all in "mantle/init/evil.el" instead?
;; Get the `:input' root set up.
(imp:load :feature  '(:input)
          :path     (imp:path:join innit:path:module "input")
          :filename "init")

;; TODO: is this module used or is it all in "mantle/init/evil.el" instead?
;; Initialize `evil' et al.
(imp:load :feature  '(:input keyboard evil init)
          :path     (imp:path:join innit:path:module "input" "keyboard" "evil")
          :filename "init")


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

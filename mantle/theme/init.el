;;; init.el --- Init User's Theme -*- lexical-binding: t; -*-
;;
;; Author: Cole Brown <code@brown.dev>
;; URL:    https://github.com/cole-brown/.config-emacs
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;; Code:


(nub:out :innit
         :debug
         (imp:file:current)
         "mantle/theme/init: Start")


;;------------------------------------------------------------------------------
;; Set Theme
;;------------------------------------------------------------------------------
;; <mantle-user-set-up>

;; ;; Set to your theme's directory.
;; ;; Default is here / (path:join innit:path:mantle "theme")
;; (customize-set-variable 'innit:theme:path    (path:join user-emacs-directory
;;                                                         "somewhere-else"
;;                                                         "theme"))

;; Set to your theme's filename.
(customize-set-variable 'innit:theme:file    "zenburn")

;; Set to your theme's `imp' feature name.
;; example:
;;   (imp:provide :mantle 'theme 'zenburn)
;;   (customize-set-variable 'mantle:theme:feature '(:mantle 'theme 'zenburn))
(customize-set-variable 'innit:theme:feature '(:mantle theme zenburn))


;; </mantle-user-set-up>
;;------------------------------------------------------------------------------
;; Initialize `innit' For Theming
;;------------------------------------------------------------------------------

;; `innit:theme:path' and `innit:theme:file' are already set to something,
;; either when defined or in e.g. "settings.el". If you want to, redefine
;; here/now instead.
;;
;; `innit:theme:init' sets Emacs' theme variables from `innit:theme:path', so
;; they must be correct for your theme by the time `innit:theme:init' runs right
;; about.... now:
(innit:theme:init)


;;------------------------------------------------------------------------------
;; Load User's Theme(s)
;;------------------------------------------------------------------------------

(nub:out :innit
         :debug
         (imp:file:current)
         "mantle/theme/init: load theme %S?"
         innit:theme:feature)

;; No theme is defined by default, so this load must be optional.
(imp:load :feature  innit:theme:feature
          :path     innit:theme:path
          :filename innit:theme:file
          :optional t)


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(nub:out :innit
         :debug
         (imp:file:current)
         "mantle/theme/init: End")
(imp:provide :mantle 'theme 'init)

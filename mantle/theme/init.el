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


(let ((file/this (imp:file:current))
      (tags/this '(:innit :mantle :theme :init)))
  (nub:debug
      :innit
      file/this
      tags/this
    "[BEGIN]")


  ;;------------------------------------------------------------------------------
  ;; Set Theme
  ;;------------------------------------------------------------------------------
  (nub:debug
      :innit
      file/this
      (append tags/this '(:user))
    "[BEGIN]")
  ;;---
  ;; <mantle-user-set-up>
  ;;---
  ;; `innit:theme:path' and `innit:theme:file' are already set to something,
  ;; either when defined or in e.g. "settings.el". If you want to, redefine
  ;; here/now instead.

  ;; Set to your theme's directory?
  ;; Default is here AKA `innit:theme:path'
  (innit:customize-set-variable innit:theme:path (path:join innit:theme:path
                                                            "zenburn"))


  ;; Set to your theme's filename.
  (innit:customize-set-variable innit:theme:file "init")


  ;; Set to your theme's `imp' feature name.
  ;; Example:
  ;;   - If theme does this:
  ;;     (imp:provide :mantle 'theme 'zenburn)
  ;;   - You should do this:
  ;;     (innit:customize-set-variable mantle:theme:feature '(:mantle theme zenburn))
  (innit:customize-set-variable innit:theme:feature '(:mantle theme zenburn))

  ;;---
  ;; </mantle-user-set-up>
  ;;---
 (nub:debug
      :innit
      file/this
      (append tags/this '(:user))
    "[END]")

  ;;------------------------------------------------------------------------------
  ;; Initialize `innit' For Theming
  ;;------------------------------------------------------------------------------

  ;; `innit:theme:init' sets Emacs' theme variables from `innit:theme:path', so
  ;; they must be correct for your theme by the time `innit:theme:init' runs right
  ;; about.... now:
  (innit:theme:init)


  ;;------------------------------------------------------------------------------
  ;; Load User's Theme(s)
  ;;------------------------------------------------------------------------------

  (nub:debug
      :innit
      file/this
      tags/this
    "mantle/theme/init: load theme %S? %s: %s, %s: %s"
    innit:theme:feature
    (if (path:exists? innit:theme:path :dir)
        "dir"
      "dir(DNE!)")
    innit:theme:path
    (if (path:exists? (path:join innit:theme:path (concat innit:theme:file ".el")) :file)
        "file"
      "file(DNE!")
    (path:join innit:theme:path (concat innit:theme:file ".el")))

  ;; No theme is defined by default, so this load must be optional.
  (imp:load :feature  innit:theme:feature
            :path     innit:theme:path
            :filename innit:theme:file
            :optional t)


  ;;------------------------------------------------------------------------------
  ;; The End.
  ;;------------------------------------------------------------------------------
  (nub:debug
      :innit
      file/this
      tags/this
    "[END]"))
(imp:provide :mantle 'theme 'init)

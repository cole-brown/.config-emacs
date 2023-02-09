;;; mantle/init/keybinds/init.el --- Bind Keys to Your Will -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2022-07-21
;; Modified:   2022-07-21
;; URL:        https://github.com/cole-brown/.config-emacs
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Bind Keys to Your Will
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Load Files
;;------------------------------------------------------------------------------

(imp:timing
    '(:mantle user init keybinds)
    (imp:file:current)
    (imp:path:current:dir)

  ;;----------------------------------------------------------------------------
  ;; Fancy Keybind Helper `general'
  ;;----------------------------------------------------------------------------
  ;; All this stuff needs `general' and won't be loaded until after it has been.
  ;; So if you use it, guard your code like so:
  ;;   (imp:eval:after (:keybinds user general)
  ;;     ...)
  ;; Or if you don't use `imp' for loading, the Emacs feature symbol is:
  ;;  (imp:feature:normalize:imp->emacs :mantle 'user 'init 'keybinds 'general 'ready)
  ;;    -> `keybinds:general:ready'
  (imp:load :feature  '(:mantle user init keybinds general)
            :path     "general"
            :filename "init")


  ;;------------------------------------------------------------------------------
  ;; Optional: Modal Input System?
  ;;------------------------------------------------------------------------------

  ;; TODO: `imp:load' keyword for "if flagged"... like `:flagged?'?
  ;;   (imp:load :if '(:flag? (:keybinds +evil)) ...)
  ;;   (imp:load :flagged? '(:keybinds +evil) ...)
  ;;   (imp:load :if '(:flagged? . (:keybinds +evil)) ...)

  ;;------------------------------
  ;; Evil?
  ;;------------------------------
  (when (imp:flag? :keybinds +evil)
    (imp:load :feature  '(:mantle config user keyboard +evil)
              :path     (imp:path:current:dir/relative :mantle)
              :filename "+evil"))

  ;;------------------------------
  ;; Meow?
  ;;------------------------------
  (when (imp:flag? :keybinds +meow)
    (imp:load :feature  '(:mantle config user keyboard +meow)
              :path     (imp:path:current:dir/relative :mantle)
              :filename "+meow"))


  ;;----------------------------------------------------------------------------
  ;; Standard Keybind Stuff
  ;;----------------------------------------------------------------------------

  ;; Nothing currently for init.
  )


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide:with-emacs :mantle 'user 'init 'keybinds)

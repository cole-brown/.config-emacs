;;; mantle/config/keyboard/init.el --- Keyboard Input Stuff. -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2023-02-07
;; Modified:   2023-02-07
;; URL:        https://github.com/cole-brown/.config-emacs
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Keyboard input stuff like `evil' or `meow' or whatever.
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Keyboard Things
;;------------------------------------------------------------------------------

;; They might need to do keybinds as part of set-up, so...
;; let them use `general'.
(imp:eval:after (:keybind general ready)

  ;;------------------------------------------------------------------------------
  ;; Modal Input Systems
  ;;------------------------------------------------------------------------------

  ;; TODO: `imp:load' keyword for "if flagged"... like `:flagged?'?
  ;;   (imp:load :if '(:flag? (:keyboard +evil)) ...)
  ;;   (imp:load :flagged? '(:keyboard +evil) ...)
  ;;   (imp:load :if '(:flagged? . (:keyboard +evil)) ...)

  ;;------------------------------
  ;; Evil?
  ;;------------------------------
  (when (imp:flag? :keyboard +evil)
    (imp:load :feature  '(:mantle config user keyboard +evil)
              :path     (imp:path:current:dir/relative :mantle)
              :filename "+evil"))

  ;;------------------------------
  ;; Meow?
  ;;------------------------------
  (when (imp:flag? :keyboard +meow)
    (imp:load :feature  '(:mantle config user keyboard +meow)
              :path     (imp:path:current:dir/relative :mantle)
              :filename "+meow")))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'config 'user 'keyboard)

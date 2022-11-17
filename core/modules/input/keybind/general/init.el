;;; core/modules/input/keybind/general/init.el --- The General in Your Keybind Army -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2022-07-21
;; Modified:   2022-11-17
;; URL:        https://github.com/cole-brown/.config-emacs
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; The General in Your Keybind Army
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Load Files
;;------------------------------------------------------------------------------

(imp:timing
    '(:keybind general)
    (imp:file:current)
    (imp:path:current:dir)


  ;;----------------------------------------------------------------------------
  ;; Pre-General
  ;;----------------------------------------------------------------------------

  (imp:load :feature  '(:keybind general constants)
            :path     (imp:path:current:dir/relative :keybind)
            :filename "constants.el")


  ;;------------------------------------------------------------------------------
  ;; Post-General
  ;;------------------------------------------------------------------------------
  ;; Need to determine path now while still loading; once `imp:eval:after' runs
  ;; it won't be able to determine the correct path.
  (let ((path/rel (imp:path:current:dir/relative :keybind)))
    ;; Delay everything in here until after general.
    (imp:eval:after general

      ;;--------------------------------------------------------------------------
      ;; Leaders
      ;;--------------------------------------------------------------------------

      (imp:load :feature  '(:keybind general leaders)
                :path     path/rel
                :filename "leaders.el")


      ;;------------------------------
      ;; INSERT HERE
      ;;------------------------------


      ;;--------------------------------------------------------------------------
      ;; Done / Loaded / Ready / Etc.
      ;;--------------------------------------------------------------------------
      ;; Provide a symbol that others can await for setting up their keybinds.
      (imp:provide:with-emacs :keybind 'general 'ready))))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :keybind 'general)

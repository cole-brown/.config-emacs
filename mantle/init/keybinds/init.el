;;; mantle/init/keybinds/init.el --- Bind Keys to Your Will -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2022-07-21
;; Timestamp:  2023-06-29
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Terminology:
;;   - keyboard input "systems":
;;     1. `evil' (modal input)
;;     2. `meow' (modal input)
;;     3. "vanilla" Emacs (non-modal) aka just normal out-of-the-box Emacs
;;
;; I have to change... most bindings.
;;   - because Dvoark
;;   - because who the hell picked these as the keys?!
;;   - etc.
;; I'm using `general' for this as it has good support for `evil', vanilla
;; Emacs, and `use-package'.
;;
;; I'm trying out modal inputs.
;;   - `evil' is ok... but:
;;     - I'm not a Vim refugee so I don't know the deep magic.
;;     - I have strong opinions on the usefulness of WASD (or actually ESDF),
;;       and there's no easy way to just tell `evil' "no, the movement keys for
;;       everyone are this instead of that".
;;     - No Dvoark support.
;;   - `meow' is much simpler, so easier to bend to my will... but:
;;     - It's very against owning any keybinds, so local keybinds are
;;       impossible*.
;;       - *I've hacked them in but it's obviously hacky.
;;       - `meow' basically just wants to be a realtime keybind interpreter and
;;       - translate a `meow' keybind into a vanilla Emacs keybind.
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Load Files
;;------------------------------------------------------------------------------

(imp:timing
    '(:mantle init keybinds)
    (imp:file:current)
    (imp:path:current:dir)

  ;;----------------------------------------------------------------------------
  ;; Fancy Keybind Helper `general'
  ;;----------------------------------------------------------------------------
  ;; All this stuff needs `general' and won't be loaded until after it has been.
  ;; So if you use it, guard your code something like...
  ;;   1. (imp:eval:after (:keybinds user general) ...)
  ;;   2. (imp:eval:after (:keybinds user general meow) ...)
  ;;   3. (imp:eval:after (:keybinds user general evil) ...)
  ;; Or if you don't use `imp' for loading, the Emacs feature symbol is:
  ;;  (imp:feature:normalize:imp->emacs :keybinds 'general)
  ;;    -> `keybinds:user:general'
  (imp:load :feature  '(:mantle init keybinds general)
            :path     (imp:path:join (imp:path:current:dir/relative :mantle)
                                     "general")
            :filename "init")


  ;;----------------------------------------------------------------------------
  ;; Vanilla Emacs
  ;;----------------------------------------------------------------------------

  ;;------------------------------
  ;; REQUIRED: Binds for even when using Meow/Evil/etc.
  ;;------------------------------

  (imp:load :feature  '(:mantle init keybinds emacs)
            :path     (imp:path:current:dir/relative :mantle)
            :filename "emacs")


  ;;----------------------------------------------------------------------------
  ;; Optional: Modal Input System?
  ;;----------------------------------------------------------------------------

  ;; TODO: `imp:load' keyword for "if flagged"... like `:flagged?'?
  ;;   (imp:load :if '(:flag? (:keybinds +evil)) ...)
  ;;   (imp:load :flagged? '(:keybinds +evil) ...)
  ;;   (imp:load :if '(:flagged? . (:keybinds +evil)) ...)

  ;;------------------------------
  ;; Evil?
  ;;------------------------------
  (when (imp:flag? :keybinds +evil)
    (imp:load :feature  '(:mantle init keybinds +evil)
              :path     (imp:path:current:dir/relative :mantle)
              :filename "+evil"))

  ;;------------------------------
  ;; Meow?
  ;;------------------------------
  (when (imp:flag? :keybinds +meow)
    (imp:load :feature  '(:mantle init keybinds +meow)
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
(imp:provide :mantle 'init 'keybinds)

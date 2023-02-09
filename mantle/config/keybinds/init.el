;;; mantle/config/keybinds/init.el --- Set up custom keybinds. -*- lexical-binding: t; -*-
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
;;  Set up custom keybinds.
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Load Keybind Configs
;;------------------------------------------------------------------------------
(message "\n\n[CONFIG] config/keydinds!!!\n\n")

(let ((path/here (imp:path:current:dir/relative :mantle)))

  ;; Just assume all keybinds in here need `general'. Most of them will.
  (message "\n\n[CONFIG] eval rest after `:keydinds/user/general'...\n\n")
  (imp:eval:after (:keybinds user general)
    (message "\n\nHELLO THERE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n\n")
    (message "\n\nHELLO THERE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n\n")
    (message "\n\nHELLO THERE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n\n")
    (message "\n\nHELLO THERE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n\n")

    ;;------------------------------------------------------------------------------
    ;; Prereqs
    ;;------------------------------------------------------------------------------

    ;; Set up local & global leaders' common infix menus.
    (imp:load :feature  '(:mantle config user keybinds infixes)
              :path     path/here
              :filename "infixes")

    ;;------------------------------------------------------------------------------
    ;; Optional: Modal Input System?
    ;;------------------------------------------------------------------------------

    ;; TODO: `imp:load' keyword for "if flagged"... like `:flagged?'?
    ;;   (imp:load :if '(:flag? (:keybinds +evil)) ...)
    ;;   (imp:load :flagged? '(:keybinds +evil) ...)
    ;;   (imp:load :if '(:flagged? . (:keybinds +evil)) ...)
    ;; TODO: May also an `:after' keyword for stuffing the load inside an `imp:eval:after'?

    ;;------------------------------
    ;; Evil?
    ;;------------------------------
    (when (imp:flag? :keybinds +evil)
      (message "\n\n[CONFIG] when evil...\n\n")
      (imp:eval:after (:and evil evil-collection)
        (message "\n\n[CONFIG] Eval after evil, evil-collection!\n\n")
        (imp:load :feature  '(:mantle config keybinds +evil)
                  :path     path/here
                  :filename "+evil")))

    ;;------------------------------
    ;; Meow?
    ;;------------------------------
    ;; TODO-meow: make this file
    (when (imp:flag? :keybinds +meow)
      (imp:eval:after meow
        (imp:load :feature  '(:mantle config keybinds +meow)
                  :path     path/here
                  :filename "+meow")))


    ;;------------------------------------------------------------------------------
    ;; Common or Smart Keybinds
    ;;------------------------------------------------------------------------------
    ;; Keybinds that don't care about Emacs/Evil, or can figure out which kind to create.

    (imp:load :feature  '(:mantle config user keybinds signature)
              :path     path/here
              :filename "signature")

    (imp:load :feature  '(:mantle config user keybinds text)
              :path     path/here
              :filename "text")

    (imp:load :feature  '(:mantle config user keybinds file)
              :path     path/here
              :filename "file")

    (imp:load :feature  '(:mantle config user keybinds buffer)
              :path     path/here
              :filename "buffer")


    ;;------------------------------------------------------------------------------
    ;; Emacs Keybinds
    ;;------------------------------------------------------------------------------
    ;; TODO: Do these need to be after anything?
    ;; TODO:   - No `evil' to load...
    ;; TODO:   - `general' is already loaded...
    ;; (imp:eval:after TODO-something-or-other-maybe?
    ;;
    ;;  (imp:load :feature  '(:mantle config user keybinds emacs)
    ;;            :path     (imp:path:join path/here "emacs")
    ;;            :filename "init"))
    ))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'config 'user 'keybinds)

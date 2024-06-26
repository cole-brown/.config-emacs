;;; mantle/config/keybinds/init.el --- Set up custom keybinds. -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2022-07-21
;; Timestamp:  2024-04-17
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
  (imp:eval:after (:and evil evil-collection)
    (imp:load :feature  '(:mantle config keybinds +evil)
              :path     (imp:path:current:dir/relative :mantle)
              :filename "+evil")))

;;------------------------------
;; Meow?
;;------------------------------
(when (imp:flag? :keybinds +meow)
  (imp:eval:after meow
    (imp:load :feature  '(:mantle config keybinds +meow)
              :path     (imp:path:current:dir/relative :mantle)
              :filename "+meow")))


;;------------------------------------------------------------------------------
;; Common or Smart Keybinds
;;------------------------------------------------------------------------------
;; Keybinds that don't care about Emacs/Evil, or can figure out which kind to create.

(imp:load :feature  '(:mantle config keybinds signature)
          :path     (imp:path:current:dir/relative :mantle)
          :filename "signature")

(imp:load :feature  '(:mantle config keybinds text)
          :path     (imp:path:current:dir/relative :mantle)
          :filename "text")

(imp:load :feature  '(:mantle config keybinds file)
          :path     (imp:path:current:dir/relative :mantle)
          :filename "file")

(imp:load :feature  '(:mantle config keybinds buffer)
          :path     (imp:path:current:dir/relative :mantle)
          :filename "buffer")


(imp:load :feature  '(:mantle config keybinds window)
          :path     (imp:path:current:dir/relative :mantle)
          :filename "window")


;;------------------------------------------------------------------------------
;; Emacs Keybinds
;;------------------------------------------------------------------------------
;; TODO: Do these need to be after anything?
;; TODO:   - No `evil' to load...
;; TODO:   - `general' is already loaded...
;; (imp:eval:after TODO-something-or-other-maybe?
;;
;;  (imp:load :feature  '(:mantle config keybinds emacs)
;;            :path     (imp:path:join (imp:path:current:dir/relative :mantle) "emacs")
;;            :filename "init"))


;;--------------------------------------------------------------------------------
;; Keybind Packages
;;--------------------------------------------------------------------------------

(imp:load :feature  '(:mantle config keybinds mwim)
          :path     (imp:path:current:dir/relative :mantle)
          :filename "mwim")


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'config 'keybinds)

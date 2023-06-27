;;; mantle/init/keybinds/general/init.el --- General of the Keybind Army? -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2022-07-21
;; Timestamp:  2023-06-27
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Initialize the General of the Keybind Army...
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Load Files
;;------------------------------------------------------------------------------

(imp:timing
    '(:mantle user init keybinds general)
    (imp:file:current)
    (imp:path:current:dir)

  (imp:load :feature  '(:mantle init keybinds general general)
            :path     (imp:path:current:dir/relative :mantle)
            :filename "general")

  (when (imp:flag? :keybinds +evil)
    (imp:load :feature  '(:mantle init keybinds general +evil)
              :path     (imp:path:current:dir/relative :mantle)
              :filename "+evil"))

  (when (imp:flag? :keybinds +meow)
    (imp:load :feature  '(:mantle init keybinds general +meow)
              :path     (imp:path:current:dir/relative :mantle)
              :filename "+meow"))


  ;;------------------------------------------------------------------------------
  ;; Special Feature Name
  ;;------------------------------------------------------------------------------
  ;; Provide a special feature name so that other parts of init can demand to
  ;; run after we have created our custom definers and such for general.
  ;;
  ;; Putting it here instead of in "general.el" so it's after the optional loads
  ;; too. "+meow.el"/"+evil.el" define their own specific to them anyways.
  (imp:use-package general
    :demand t ;; Always load, if when/after/etc are valid.
    ;;------------------------------
    :init
    ;;------------------------------
    ;; TODO: Have imp provide all of everything to Emacs?
    ;;       - That is, replace `imp:provide' with `imp:provide:with-emacs' in imp.
    (imp:provide:with-emacs :keybinds 'user 'general)))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide:with-emacs :mantle 'init 'keybinds 'general)

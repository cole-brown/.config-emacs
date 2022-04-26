;;; early-init.el --- Early Init. -*- lexical-binding: t; -*-
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
;; Emacs 27.1 introduced early-init.el, which is run before init.el, before
;; package and UI initialization happens, and before site files are loaded.
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Modules Required for Init
;;------------------------------------------------------------------------------

(let ((path-core-modules (expand-file-name "core/modules/" user-emacs-directory)))
  ;; Order matters here. These build on each other.
  (load (expand-file-name "emacs/imp/init" path-core-modules))

  ;; TODO: Make timing optional. Don't enable here, and let it be set or not in "settings.el"?
  (customize-set-variable 'imp:timing:enabled? t)

  ;; Group all the required-for-innit modules together in timing info.
  (imp:timing
      '(:innit early-init modules)
      "early-init.el"
      (imp:path:current:dir)
    (load (expand-file-name "emacs/str/init"   path-core-modules))
    (load (expand-file-name "emacs/alist/init" path-core-modules))
    (load (expand-file-name "emacs/path/init"  path-core-modules))
    (load (expand-file-name "output/nub/init"  path-core-modules))
    (load (expand-file-name "emacs/innit/init" path-core-modules))))


(let ((file/this (imp:file:current)))
  ;;------------------------------------------------------------------------------
  ;; Settings & Overrides
  ;;------------------------------------------------------------------------------

  ;; Load these settings first so they can override any `defvar' and be prepared
  ;; for wherever they're needed in the init sequence.
  (imp:path:root :settings user-emacs-directory)
  ;; Don't want to error if file isn't there, but do want to error if loading the
  ;; file causes an error, so `:optional t` should be perfect.
  (imp:load :feature  '(:settings)
            :optional t
            :filename "settings")


  ;;------------------------------------------------------------------------------
  ;; Initialize `innit'
  ;;------------------------------------------------------------------------------

  ;; Set up `nub' for use by `innit'.
  (innit:nub:init)

  (nub:out :innit
           :debug
           file/this
           ;; Is "Power On Self Test" a good term to steal?
           "POST")

  ;; Set up `innit' & Emacs' debug flags based on one another.
  (innit:debug:init)


  ;;------------------------------------------------------------------------------
  ;; Load Early-Init Files
  ;;------------------------------------------------------------------------------

  (nub:out :innit
           :debug
           file/this
           "Boot Loader: 00 Early")

  (imp:timing
      '(:innit early-init load)
      file/this
      (imp:path:current:dir)
    (innit:load file/this "00-early")))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
;; TODO: Provide anything? Push to some list or whatever that can be added to imp after the fact?

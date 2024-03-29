;;; early-init.el --- Early Init -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2022-03-23
;; Timestamp:  2023-09-26
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

    (imp:load :feature  '(:elisp utils)
              :path     (imp:path:join path-core-modules "elisp" "utils")
              :filename "init")

    (imp:load :feature  '(:str)
              :path     (imp:path:join path-core-modules "emacs" "str")
              :filename "init")

    (imp:load :feature  '(:buffer)
              :path     (imp:path:join path-core-modules "emacs" "buffer")
              :filename "init")

    (imp:load :feature  '(:chrome)
              :path     (imp:path:join path-core-modules "emacs" "chrome")
              :filename "init")

    (imp:load :feature  '(:alist)
              :path     (imp:path:join path-core-modules "emacs" "alist")
              :filename "init")

    (imp:load :feature  '(:nub)
              :path     (imp:path:join path-core-modules "output" "nub")
              :filename "init")

    (imp:load :feature  '(:path)
              :path     (imp:path:join path-core-modules "emacs" "path")
              :filename "init")

    (imp:load :feature  '(:innit)
              :path     (imp:path:join path-core-modules "emacs" "innit")
              :filename "init")

    (imp:load :feature  '(:dlv)
              :path     (imp:path:join path-core-modules "emacs" "dlv")
              :filename "init")

    (imp:load :feature  '(:template)
              :path     (imp:path:join path-core-modules "emacs" "template")
              :filename "init")

    (imp:load :feature  '(:jerky)
              :path     (imp:path:join path-core-modules "elisp" "jerky")
              :filename "init")

    (imp:load :feature  '(:datetime)
              :path     (imp:path:join path-core-modules "elisp" "datetime")
              :filename "init")

    (imp:load :feature  '(:system)
              :path     (imp:path:join path-core-modules "system")
              :filename "init")

    (imp:load :feature  '(:system multiplexer)
              :path     (imp:path:join path-core-modules "system" "multiplexer")
              :filename "init")

    (imp:load :feature  '(:system secret)
              :path     (imp:path:join path-core-modules "system" "secret")
              :filename "init")))


;;------------------------------------------------------------------------------
;; Set-Up `imp' Roots for Initialization Directories
;;------------------------------------------------------------------------------
;; `imp' is now loaded, so we can set ourselves up to use it.

;; 'early-init.el' and 'init.el' only, probably...
(imp:path:root/set :root
                   user-emacs-directory)

;; Core elisp for `innit' to invoke during start-up.
(imp:path:root/set :core
                   innit:path:core/boot)

;; NOTE: 'mantle/' directory is available/encouraged for users to put their
;; Emacs init stuff in. `innit' will call 'mantle/init.el' during one phase of
;; 'init.el', and then will call 'mantle/config.el' during a later phase of
;; 'init.el'.
(imp:path:root/set :mantle
                   (imp:path:join user-emacs-directory "mantle")
                   "init.el")

;; User's modules; not our "core/modules/" directory.
(imp:path:root/set :module innit:path:module)


(let ((file/this (imp:file:current))
      (tags/this '(:innit :early)))
  ;;------------------------------------------------------------------------------
  ;; Settings & Overrides
  ;;------------------------------------------------------------------------------

  ;; Load these settings first so they can override any `defvar' and be prepared
  ;; for wherever they're needed in the init sequence.
  (imp:path:root/set :settings user-emacs-directory)
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

  (nub:debug
      :innit
      file/this
      tags/this
    ;; Is "Power On Self Test" a good term to steal?
    "POST")

  ;; Set up `innit' & Emacs' debug flags based on one another.
  (innit:debug:init)


  ;;------------------------------------------------------------------------------
  ;; Load Early-Init Files
  ;;------------------------------------------------------------------------------

  (nub:debug
      :innit
      file/this
      tags/this
    "Boot Loader: 00 Early")

  (imp:load :feature  '(:core boot early)
            :path     (imp:path:join innit:path:core/boot "00-early")
            :filename "00-init.el"))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :root 'init 'early)

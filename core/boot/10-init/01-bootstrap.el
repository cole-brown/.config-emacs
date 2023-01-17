;;; 00-bootstrap.el --- Super early stuff requried for "all" of init. -*- lexical-binding: t; -*-
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


;;------------------------------------------------------------------------------
;; Load Paths
;;------------------------------------------------------------------------------

;; TODO: Do I want to add anything to the load-path directly, or just use `imp'
;; et al?
;; (add-to-list 'load-path (path:join user-emacs-directory "..."))


;;------------------------------------------------------------------------------
;; Packages & Modules: Part 01
;;------------------------------------------------------------------------------
;; Set up for packages via `use-package', then immediately install
;; `no-littering' before anything can think about littering our beautiful
;; `user-emacs-directory'.

;;------------------------------
;; `package.el' & `use-package'
;;------------------------------
;; https://github.com/jwiegley/use-package

;; Finish `package.el' init; install & require `use-package'.
(innit:package:init/standard)


;;------------------------------
;; `no-littering'
;;------------------------------
;; Keep the `user-emacs-directory' clean by changing where Emacs & packages
;; store their data. Move it from various & sundry places in and under
;; `user-emacs-directory' to be in one of two `user-emacs-directory'
;; sub-directories:
;;   - `no-littering-etc-directory'
;;   - `no-littering-var-directory'
(imp:use-package no-littering
  ;; Make sure this loads ASAP. It's used for init/config of other packages.
  :demand t


  ;;------------------------------
  :custom
  ;;------------------------------
  ;; Set the `no-littering' directory paths.
  (no-littering-etc-directory innit:path:etc)
  (no-littering-var-directory innit:path:var)


  ;;------------------------------
  :config
  ;;------------------------------
  ;; Suggested settings: https://github.com/emacscollective/no-littering#suggested-settings

  ;; `recentf' should ignore the files in the `no-littering' dirs.
  (imp:eval:after recentf
    (add-to-list 'recentf-exclude no-littering-etc-directory)
    (add-to-list 'recentf-exclude no-littering-var-directory))

  ;; Auto-saves should go in the `no-littering' directory.
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

  ;; Native Compliation (Emacs 28+):
  (when (fboundp 'startup-redirect-eln-cache)
    (startup-redirect-eln-cache
     (convert-standard-filename
      (no-littering-expand-var-file-name "eln-cache/")))))


;;------------------------------
;; WARNING: Do _NOT_ put any more packages here!!!
;;------------------------------
;; Before any other packages happen, we also need to deal with 'custom.el',
;; which is bad and should feel bad.


;;------------------------------------------------------------------------------
;; Go away, "custom.el"...            ಠ_ಠ
;;------------------------------------------------------------------------------
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Saving-Customizations.html
;;
;; We won't support `customize'/"custom.el" and never will. It's a clumsy
;; interface that sets variables at a time where it can be easily and
;; unpredictably overwritten. Always configure things in the Emacs init files.
(let ((path-for-message (path:join user-emacs-directory "mantle/*.el")))
  ;;------------------------------
  ;; Disable some 'custom'/'customize' functions.
  ;;------------------------------
  ;; Disabling like this won't affect using them during init, but will show the
  ;; user the message we assign if they call the interactively.
  (dolist (symbol '(customize-option          customize-browse
                    customize-group           customize-face
                    customize-rogue           customize-saved
                    customize-apropos         customize-changed
                    customize-unsaved         customize-variable
                    customize-set-value       customize-customized
                    customize-set-variable    customize-apropos-faces
                    customize-save-variable   customize-apropos-groups
                    customize-apropos-options customize-changed-options
                    customize-save-customized))
    (put symbol
         'disabled
         (format "`innit' doesn't support `customize'; configure Emacs from '%s' instead"
                 path-for-message)))

  ;;------------------------------
  ;; Disable `customize-themes'?
  ;;------------------------------
  ;; Use `load-theme` to set your theme. See: '/mantle/theme/init.el'
  (put 'customize-themes 'disabled
       (format "`innit' doesn't support `customize'; set your theme with `load-theme' in '%s' instead"
               path-for-message))

  ;;------------------------------
  ;; Custom File
  ;;------------------------------
  ;; Move all the customizations to null device or a file that is never loaded.
  ;;   https://www.reddit.com/r/emacs/comments/9rrhy8/emacsers_with_beautiful_initel_files_what_about/e8juc8v
  ;;
  ;; Two options from that reddit thread:


  ;;---
  ;; 1. Null device aka '/dev/null':
  ;;---
  (setq custom-file null-device)

  ;; If you get some weird error like this:
  ;;   > custom-initialize-reset: Renaming: Invalid argument, \
  ;;   >   c:/path/to/tmpasnG58, c:/path/to/NUL
  ;; You should try option #2 instead.


  ;;---
  ;; 2. Move all the customizations to a file that is never loaded.
  ;;---
  ;; NOTE: `null-device' didn't work on Windows with Emacs ~26. Haven't tried in
  ;; a few Emacs versions, but if that's still the case, do one of these
  ;; instead:
  ;;
  ;; ;; Using `no-littering':
  ;; (setq custom-file (no-littering-expand-etc-file-name "custom.ignored.el"))
  ;;
  ;; ;; Not using no-littering:
  ;; (setq custom-file (path:join user-emacs-directory "custom.ignored.el"))


  ;;---
  ;; 3. DEBUGGING:
  ;;---
  ;; Set it back to normal if you need to figure out what Emacs is writing to '/dev/null'.
  ;; (setq custom-file (path:join user-emacs-directory "custom.el"))
  ;; ...Just don't leave it like that.
  )


;;------------------------------------------------------------------------------
;; Packages & Modules: Part 02
;;------------------------------------------------------------------------------
;; Load a few "must exist ASAP" things that we'll use for the rest of Emacs
;; start-up.

;;------------------------------
;; Garbage Collector Magic Hacks (`gcmh')
;;------------------------------
;; https://github.com/emacsmirror/gcmh


;; NOTE: WARNING!
;;   If you do not want `gcmh', be sure to set this back to something reasonable!
;;   It was set to `most-positive-fixnum' during early-init in
;;   "core/boot/00-early/00-bootstrap.el"!
;; (setq gc-cons-threshold 800000) ;; Emacs' default value


;; The GC introduces annoying pauses and stuttering into our Emacs experience,
;; so we use `gcmh' to stave off the GC while we're using Emacs, and provoke it
;; when it's idle. However, if the idle delay is too long, we run the risk of
;; runaway memory usage in busy sessions. If it's too low, then we may as well
;; not be using `gcmh' at all.
(imp:use-package gcmh

  ;;------------------------------
  :custom
  ;;------------------------------
  (gcmh-idle-delay 'auto)  ; default is 15s
  (gcmh-auto-idle-delay-factor 10)
  ;; GCMH default:  1 MB
  ;; Doom default: 16 MB
  ;; I have 32 or 64 GB of ram, depending on computer, so higher?
  (gcmh-high-cons-threshold (* 32 1024 1024))

  ;;------------------------------
  :config
  ;;------------------------------
  (gcmh-mode 1))


;;------------------------------
;; Mis (Pretty Printing)
;;------------------------------

(defvar innit:path:package:mis (path:join innit:path:packages:user "mis")
  "`use-package' doesn't like having a function call for `:load-path', thus this.")


(imp:use-package mis
  ;; This is my own package, so...
  ;;   1) Don't try to install.
  :ensure nil
  ;;   2) Here's where it is; add this dir to the `load-path'.
  :load-path innit:path:package:mis

  ;; ;;------------------------------
  ;; :custom
  ;; ;;------------------------------
  ;;
  ;; mis:buffer:name
  ;; mis:buffer:default
  ;; mis:comment:overrides
  )


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :core 'boot 'init 'bootstrap)

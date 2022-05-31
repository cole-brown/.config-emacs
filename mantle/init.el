;;; init.el --- User-level Emacs Initialization -*- lexical-binding: t; -*-
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


(let ((file/this (imp:file:current))
      (tags/this '(:innit :mantle :init)))
  (nub:debug
      :innit
      file/this
      tags/this
    "[BEGIN]")

  ;; TODO: delete?
  ;; ;;------------------------------------------------------------------------------
  ;; ;; "init.el" and "config.el"
  ;; ;;------------------------------------------------------------------------------
  ;; ;; Add `imp' feature lists to be loaded if correct file is present at
  ;; ;; imp root path.
  ;; ;;   - "init.el" will be checked for after core init is run.
  ;; ;;   - "config.el" will be checked for after core config is run.
  ;; ;;
  ;; ;; (innit:feature:mantle:add "core/10-init/20-load.el" :foo)
  ;; ;; (innit:feature:mantle:add "core/10-init/20-load.el" :zort narf)
  ;; ;; TODO: delete those functions from innit and just do things KISS here...
  ;;
  ;; ;;------------------------------------------------------------------------------
  ;; ;; Order of User Init
  ;; ;;------------------------------------------------------------------------------
  ;;
  ;; ;; TODO: something like `doom!' so user can define a load order easily?




  ;;------------------------------------------------------------------------------
  ;; Run User's Inits in This Order
  ;;------------------------------------------------------------------------------

  ;;------------------------------
  ;; [EARLY] Prereq Packages: Dash, s.el...
  ;;------------------------------

  (imp:timing
      '(:mantle packages early)
      (imp:file:current)
      (imp:path:current:dir)

    ;; https://github.com/magnars/dash.el
    (imp:use-package dash
       ;; Make sure this loads ASAP. It's used for init/config of things just after this.
       :demand t

       ;;--------------------
       :config
       ;;--------------------

       ;;---
       ;; Fontification of Special Variables
       ;;---
       ;; Font lock of special Dash variables (it, acc, etc.) in Emacs Lisp buffers
       ;; can optionally be enabled with the autoloaded minor mode
       ;; `dash-fontify-mode'.
       ;;
       ;; Automatically enable the minor mode in all Emacs Lisp buffers:
       (global-dash-fontify-mode)

       ;;---
       ;; Info Symbol Lookup
       ;;---
       ;; While editing Elisp files, you can use C-h S (info-lookup-symbol) to look
       ;; up Elisp symbols in the relevant Info manuals (see (emacs) Info Lookup).
       ;;
       ;; Enable this for Dash symbols:
       (with-eval-after-load 'info-look
         (dash-register-info-lookup))))



  ;;------------------------------
  ;; [EARLY] User Modules: Utils, etc for use in rest of `mantle'.
  ;;------------------------------
  ;; Group the early stuff under a separate timing.
  (imp:timing
      '(:mantle modules early)
      (imp:file:current)
      (imp:path:current:dir)

    (imp:load :feature  '(:emacs buffer)
              :path     (imp:path:join innit:path:module "emacs" "buffer")
              :filename "init") ; Needed by ':mode/org'.

    (imp:load :feature  '(:mode org)
              :path     (imp:path:join innit:path:module "mode" "org")
              :filename "init")) ; Needed by ':mantle/theme/init'.


  ;;------------------------------
  ;; Theme
  ;;------------------------------
  (imp:load :feature  '(:mantle theme init)
            :path     innit:theme:path
            :filename "init")

  ;; ;;------------------------------
  ;; ;; TODO: <other stuff>
  ;; ;;------------------------------
  ;; ;; Group this stuff under a separate timing.
  ;; (imp:timing
  ;;     '(:mantle modules init)
  ;;     (imp:file:current)
  ;;     (imp:path:current:dir)
  ;;   ...
  ;; )
  ;;
  ;; ;; Group this stuff under a separate timing.
  ;; (imp:timing
  ;;     '(:mantle packages init)
  ;;     (imp:file:current)
  ;;     (imp:path:current:dir)
  ;;   ...
  ;; )


  ;;------------------------------------------------------------------------------
  ;; The End.
  ;;------------------------------------------------------------------------------
  (nub:debug
      :innit
      file/this
      tags/this
    "[END]"))
(imp:provide :mantle 'init)

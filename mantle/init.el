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


(nub:out :innit
         :debug
         (imp:file:current)
         "mantle/init: Start")


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

;;--------------------
;; [EARLY] User Modules: Utils, etc for use in rest of `mantle'.
;;--------------------
;; Group the early stuff under a separate timing.
(imp:timing
    '(:mantle modules)
    (imp:file:current)
    (imp:path:current:dir)

  (imp:load :feature  '(:module elisp utils)
            :path     (imp:path:join innit:path:module "elisp" "utils")
            :filename "init") ; Needed by ':module/elisp/jerky'.

  (imp:load :feature  '(:module elisp jerky)
            :path     (imp:path:join innit:path:module "elisp" "jerky")
            :filename "init") ; Needed by ':module/mode/org'.

  (imp:load :feature  '(:module emacs buffer)
            :path     (imp:path:join innit:path:module "emacs" "buffer")
            :filename "init") ; Needed by ':module/mode/org'.

  (imp:load :feature  '(:module mode org)
            :path     (imp:path:join innit:path:module "mode" "org")
            :filename "init")) ; Needed by ':mantle/theme/init'.


;;--------------------
;; Theme
;;--------------------
(imp:load :feature  '(:mantle theme init)
          :path     innit:theme:path
          :filename "init")

;; ;;--------------------
;; ;; TODO: <other stuff>
;; ;;--------------------
;;
;; ;; Group this stuff under a separate timing.
;; (imp:timing
;;   :module
;;   (imp:file:current)
;;   (imp:path:current:dir)
;;   ...
;; )

;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(nub:out :innit
         :debug
         (imp:file:current)
         "mantle/init: End")
(imp:provide :mantle 'init)

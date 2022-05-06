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


;;------------------------------------------------------------------------------
;; "init.el" and "config.el"
;;------------------------------------------------------------------------------
;; Add `imp' feature lists to be loaded if correct file is present at
;; imp root path.
;;   - "init.el" will be checked for after core init is run.
;;   - "config.el" will be checked for after core config is run.
;;
;; (innit:feature:mantle:add "core/10-init/20-load.el" :foo)
;; (innit:feature:mantle:add "core/10-init/20-load.el" :zort narf)


;;------------------------------------------------------------------------------
;; Order of User Init
;;------------------------------------------------------------------------------

;; TODO: something like `doom!' so user can define a load order easily.


;;------------------------------------------------------------------------------
;; Run User's Init
;;------------------------------------------------------------------------------

;;---
;; Theme
;;---
(imp:load :feature  '(:mantle theme init)
          :path     (imp:path:join (imp:path:current:dir) "theme")
          :filename "init")


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(nub:out :innit
         :debug
         (imp:file:current)
         "mantle/init: End")
(imp:provide :mantle 'init)

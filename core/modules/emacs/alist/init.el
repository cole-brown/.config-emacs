;;; init.el --- Alist Helpers -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2021-12-15
;; Modified:   2022-04-13
;;
;;; Commentary:
;;
;; Helpful alist functions.
;;
;;; Code:

;;                                 ──────────                                 ;;
;; ╔════════════════════════════════════════════════════════════════════════╗ ;;
;; ║                           Association Lists                            ║ ;;
;; ╚════════════════════════════════════════════════════════════════════════╝ ;;
;;                                   ──────                                   ;;
;;   - Namespaced so you can find related functions.                          ;;
;;   - Other useful things, probably.                                         ;;
;;                                 ──────────                                 ;;


;;------------------------------------------------------------------------------
;; Set imp Root
;;------------------------------------------------------------------------------

(imp:path:root/set :alist
                   (imp:path:current:dir)
                   "init.el")


;;------------------------------------------------------------------------------
;; Load Files
;;------------------------------------------------------------------------------

(imp:timing
    :alist
    "init.el"
    (imp:path:current:dir)

  (imp:load :feature  '(:alist internal)
            :filename "internal")

  ;;---
  ;; General/Generic Alist Functionality
  ;;---
  (imp:load :feature  '(:alist type types)
            :filename "type/types") ;; 'generic.el' needs these functions/vars.
  (imp:load :feature  '(:alist generic)
            :filename "generic")

  ;;---
  ;; Typed Alists
  ;;---
  (imp:load :feature  '(:alist type default)
            :filename "type/default")
  (imp:load :feature  '(:alist type keyword)
            :filename "type/keyword")
  (imp:load :feature  '(:alist type string)
            :filename "type/string")

  ;; End load timing.
  )


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :alist)

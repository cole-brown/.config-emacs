;;; init.el --- `:system' namespace -*- lexical-binding: t; -*-
;;
;; Author:   Cole Brown <code@brown.dev>
;; URL:      https://github.com/cole-brown/.config-emacs
;; Created:  2022-06-07
;; Modified: 2022-06-07
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;  `:system' namespace initialization
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Set-Up.
;;------------------------------------------------------------------------------

(imp:path:root :system
               (imp:path:join (imp:path:current:dir)))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
;; NOTE: Don't provide the root `:system' namespace since we aren't loading all
;; `:system' modules.
;;
;; (imp:provide :system)

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
;; Loading.
;;------------------------------------------------------------------------------

;; Currently don't load any of the `:system' modules.
;; TODO: Should we change that and load them all in here?


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :system)

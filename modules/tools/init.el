;;; init.el --- `:tools' namespace -*- lexical-binding: t; -*-
;;
;; Author:   Cole Brown <code@brown.dev>
;; URL:      https://github.com/cole-brown/.config-emacs
;; Created:  2022-07-11
;; Modified: 2022-07-11
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;  `:tools' namespace initialization
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Set-Up.
;;------------------------------------------------------------------------------

(imp:path:root :tools
               (imp:path:current:dir))


;;------------------------------------------------------------------------------
;; Loading.
;;------------------------------------------------------------------------------

;; Don't load any of the `:tools' modules - let the user decided what to do.


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :tools)

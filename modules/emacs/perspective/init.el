;;; modules/emacs/perspective/init.el --- `persp-mode' improvements -*- lexical-binding: t; -*-
;;
;; Author:   Cole Brown <code@brown.dev>
;; URL:      https://github.com/cole-brown/.config-emacs
;; Created:  2023-01-20
;; Modified: 2023-01-20
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;  `persp-mode' improvements
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Set `imp' Root
;;------------------------------------------------------------------------------

(imp:path:root/set :perspective
                   (imp:path:current:dir))


;;------------------------------------------------------------------------------
;; Load Files
;;------------------------------------------------------------------------------

(imp:timing
    '(:perspective)
    (imp:file:current)
    (imp:path:current:dir)

  (imp:load :feature  '(:perspective perspective)
            :filename "perspective"))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :perspective)

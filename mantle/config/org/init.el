;;; mantle/config/org/init.el --- Configure Org & Friends -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2023-03-16
;; Modified:   2023-03-16
;; URL:        https://github.com/cole-brown/.config-emacs
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;  Configure `org-mode', `org-journal', etc.
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Note Taking and Pretty Much Everything Else...
;;------------------------------------------------------------------------------

(imp:load :feature  '(:mantle config user org-mode)
          :path     (imp:path:current:dir/relative :mantle)
          :filename "org-mode")


;; (imp:load :feature  '(:mantle config user emacs)
;;           :path     (imp:path:current:dir/relative :mantle)
;;           :filename "emacs")


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'config 'user 'org)

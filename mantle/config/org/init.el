;;; mantle/config/org/init.el --- Configure Org & Friends -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2023-03-16
;; Timestamp:  2023-06-28
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Configure `org-mode', `org-journal', etc.
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Org-Mode Itself
;;------------------------------------------------------------------------------

(imp:load :feature  '(:mantle config user org mode)
          :path     (imp:path:current:dir/relative :mantle)
          :filename "mode")

(imp:load :feature  '(:mantle config user org agenda)
          :path     (imp:path:current:dir/relative :mantle)
          :filename "agenda")


;;------------------------------------------------------------------------------
;; Org-Journal
;;------------------------------------------------------------------------------

(imp:load :feature  '(:mantle config user org journal)
          :path     (imp:path:current:dir/relative :mantle)
          :filename "journal")


;;------------------------------------------------------------------------------
;; Etc.
;;------------------------------------------------------------------------------

(imp:load :feature  '(:mantle config user org contacts)
          :path     (imp:path:current:dir/relative :mantle)
          :filename "contacts")

(imp:load :feature  '(:mantle config user org pretty)
          :path     (imp:path:current:dir/relative :mantle)
          :filename "pretty")


;;------------------------------------------------------------------------------
;; Integration with Other Parts of Emacs
;;------------------------------------------------------------------------------

(imp:load :feature  '(:mantle config user org version-control)
          :path     (imp:path:current:dir/relative :mantle)
          :filename "version-control")


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'config 'user 'org)

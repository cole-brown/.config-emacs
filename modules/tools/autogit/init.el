;;; tools/autogit/init.el --- auto-commit git repos -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2020-10-28
;; Modified:   2022-07-25
;; URL:        https://github.com/cole-brown/.config-emacs
;; Version:    1.0.0
;; Keywords:   vc tools
;; Homepage:   https://github.com/cole-brown/.config-secret
;; Package-Requires: ((emacs "27.1") (magit "3.3.0") (deferred "0.5.1"))
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Commands for:
;;   1) Auto-committing changes to certain git repos.
;;      - E.g. notes repositories.
;;   2) Getting general status for certain other git repos.
;;      - E.g. get status for your notes repo(s) and your code repo(s) in one go.
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Set imp Root.
;;------------------------------------------------------------------------------

(imp:path:root :autogit
               (imp:path:current:dir)
               "init.el")


;;------------------------------------------------------------------------------
;; Load Files
;;------------------------------------------------------------------------------

(imp:timing
    '(:autogit)
    (imp:file:current)
    (imp:path:current:dir)

  (imp:load :feature  '(:autogit variables)
            :filename "variables")

  (imp:load :feature  '(:autogit internal)
            :filename "internal")

  (imp:load :feature  '(:autogit api)
            :filename "api")

  (imp:load :feature  '(:autogit commands)
            :filename "commands"))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide:with-emacs :autogit)

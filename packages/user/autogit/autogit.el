;;; autogit.el --- auto-commit git repos -*- lexical-binding: t; -*-
;;
;; Author:           Cole Brown <http://github/cole-brown>
;; Maintainer:       Cole Brown <code@brown.dev>
;; Created:          2020-10-28
;; Modified:         2022-07-27
;; Version:          1.0.0
;; Package-Requires: ((emacs "28.1") (magit "3.3.0") (deferred "0.5.1"))
;; Keywords:         git tools vc
;; Homepage:         http://github/cole-brown
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Auto-commit git repos & view status on git repos.
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Autogit Files
;;------------------------------------------------------------------------------

(require 'autogit-variables)
(require 'autogit-path)
(require 'autogit-buffer)
(require 'autogit-magit)
(require 'autogit-output)
(require 'autogit-api)
(require 'autogit-commands)


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'autogit)
;;; autogit.el ends here

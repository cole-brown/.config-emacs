;;; autogit.el --- auto-commit git repos -*- lexical-binding: t; -*-
;;
;; Author:           Cole Brown <http://github/cole-brown>
;; Maintainer:       Cole Brown <code@brown.dev>
;; URL:              https://github.com/cole-brown/.config-emacs
;; Created:          2020-10-28
;; Timestamp:        2023-06-29
;; Version:          1.0.0
;; Package-Requires: ((emacs "28.1") (magit "3.3.0") (deferred "0.5.1"))
;; Keywords:         git tools vc
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
(require 'autogit-output)
(require 'autogit-magit)
(require 'autogit-api)
(require 'autogit-commands)


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'autogit)
;;; autogit.el ends here

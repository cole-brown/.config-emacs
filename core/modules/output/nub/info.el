;;; info.el --- Nub Info Level Output -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2022-06-07
;; Modified:   2022-06-07
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Nub Info Level Output
;;
;;; Code:

;;                                 ──────────                                 ;;
;; ╔════════════════════════════════════════════════════════════════════════╗ ;;
;; ║                             Info Messages                              ║ ;;
;; ╚════════════════════════════════════════════════════════════════════════╝ ;;
;;                                   ──────                                   ;;
;;                         Insert Information Here: _                         ;;
;;                                 ──────────                                 ;;


(imp:require :path)
(imp:require :nub 'output)


;;------------------------------------------------------------------------------
;; Infos!
;;------------------------------------------------------------------------------

(defun nub:info (user caller message &rest args)
  "Output a MESSAGE for USER at `:info' level.

Format to standard message output for the USER with CALLER info, then output
the message with FORMATTING and ARGS to the correct place according to the
current verbosity of `:info' (e.g. `info', by default).

If CALLER is nil, uses relative path from `user-emacs-directory' to
the caller's file (using `path:current:file' and `path:relative').
  Examples:
    - \"init.el\"
    - \"core/modules/output/nub/foo.el\"
    - \"/some/path/outside/user-emacs-directory/file.el\"

MSG should be the `message' formatting string.

ARGS should be the `message' arguments."
  (declare (indent 2))

  (let* ((caller (or caller
                     (path:relative (path:current:file)
                                    user-emacs-directory)))
         (func/name (nub:format:callers "nub:info" caller)))

    (int<nub>:user:exists? func/name user :info)
    (apply #'nub:output
           user
           :info
           caller
           message
           args)))
;; (nub:info :default "test-func" "hello %s" "there")
;; (nub:info :default "test-func" '(:derive) (message "test"))
;; (nub:info :default "test-func" '(:jeff) (message "test"))
;; (let ((caller "test-func")
;;       (tags '(:derive))
;;       (msg "test message"))
;;   (nub:debug caller tags msg))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :nub 'info)

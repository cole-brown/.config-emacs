;;; warning.el --- Nub Warning Level Output -*- lexical-binding: t; -*-
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
;;  Nub Warning Level Output
;;
;;; Code:

;;                                 ──────────                                 ;;
;; ╔════════════════════════════════════════════════════════════════════════╗ ;;
;; ║                            Warning Messages                            ║ ;;
;; ╚════════════════════════════════════════════════════════════════════════╝ ;;
;;                                   ──────                                   ;;
;;                          CAUTION: Warnings ahead!                          ;;
;;                                 ──────────                                 ;;


(imp:require :path)
(imp:require :nub 'output)


;;------------------------------------------------------------------------------
;; Warnings!
;;------------------------------------------------------------------------------

(defun nub:warning (user caller message &rest args)
  "Output a MESSAGE for USER at `:warning' level.

Format to standard message output for the USER with CALLER info, then output
the message with FORMATTING and ARGS to the correct place according to the
current verbosity of `:warning' (e.g. `warning', by default).

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
         (func/name (nub:format:callers "nub:warning" caller)))

    (int<nub>:user:exists? func/name user :warning)
    (apply #'nub:output
           user
           :warning
           caller
           message
           args)))
;; (nub:warning :default "test-func" "hello %s" "there")
;; (nub:warning :default "test-func" '(:derive) (message "test"))
;; (nub:warning :default "test-func" '(:jeff) (message "test"))
;; (let ((caller "test-func")
;;       (tags '(:derive))
;;       (msg "test message"))
;;   (nub:debug caller tags msg))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :nub 'warning)

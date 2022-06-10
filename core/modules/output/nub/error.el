;;; error.el --- Nub Error Level Output -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2022-05-25
;; Modified:   2022-05-25
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Nub Error Level Output
;;
;;; Code:

;;                                 ──────────                                 ;;
;; ╔════════════════════════════════════════════════════════════════════════╗ ;;
;; ║                             Error Messages                             ║ ;;
;; ╚════════════════════════════════════════════════════════════════════════╝ ;;
;;                                   ──────                                   ;;
;;                  Delete if you can make error-proof code.                  ;;
;;                                 ──────────                                 ;;


(imp:require :path)
(imp:require :nub 'output)


;;------------------------------------------------------------------------------
;; Errors!
;;------------------------------------------------------------------------------

(defun nub:error (user caller message &rest args)
  "Output a MESSAGE for USER at `:error' level.

Format to standard message output for the USER with CALLER info, then output
the message with FORMATTING and ARGS to the correct place according to the
current verbosity of `:error' (e.g. `error', by default).

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
         (func/name (nub:format:callers "nub:error" caller)))

    (int<nub>:user:exists? func/name user :error)
    (apply #'nub:output
           user
           :error
           caller
           message
           args)))
;; (nub:error :default "test-func" "hello %s" "there")
;; (nub:error :default "test-func" '(:derive) (message "test"))
;; (nub:error :default "test-func" '(:jeff) (message "test"))
;; (let ((caller "test-func")
;;       (tags '(:derive))
;;       (msg "test message"))
;;   (nub:debug caller tags msg))


(defun nub:error:sink (user caller sink msg &rest args)
  "Output a message for USER at `:error' level to `innit:nub:sink-fn'.

Format MSG with ARG, then output the formatted error message to
`innit:nub:sink-fn', as opposed to USER's usual `:error' sink(s). Notably, avoid
the default of raising an error signal via `error' sink.

If CALLER is nil, use relative path from `user-emacs-directory' to
the caller's file (using `path:current:file' and `path:relative').
  Examples:
    - \"init.el\"
    - \"core/modules/output/nub/foo.el\"
    - \"/some/path/outside/user-emacs-directory/file.el\"

MSG should be the `message' formatting string.

ARGS should be the `message' arguments.

SINK should be the 'nub' level, or the function, to use for outputting."
  (declare (indent 3))

  (let* ((caller (or caller
                     (path:relative (path:current:file)
                                    user-emacs-directory)))
         (func/name (nub:format:callers "nub:error" caller)))

    (int<nub>:user:exists? func/name user :error)
    (int<nub>:output user
                     :error
                     sink
                     caller
                     msg
                     args)))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :nub 'error)

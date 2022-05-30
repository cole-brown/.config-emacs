;;; 00-finalize.el --- Final touches -*- lexical-binding: t; -*-
;;
;; Author: Cole Brown <code@brown.dev>
;; URL:    https://github.com/cole-brown/.config-emacs
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;; Code:


;;------------------------------------------------------------------------------
;; Finalize.
;;------------------------------------------------------------------------------

;; TODO: A random koan output by `mis' instead of `nub'.
(nub:debug
    :innit
    (imp:file:current)
    '(:innit :finalize)
  "TODO: a random koan")


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(nub:debug
    :innit
    (imp:file:current)
    '(:innit :finalize)
  "Boot Loader: 99 Finalize :: Every Has Been Done.")
(imp:provide :core 'boot '99-finalize 'finalize)

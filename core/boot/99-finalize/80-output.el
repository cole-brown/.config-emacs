;;; 80-output.el --- Output some things. -*- lexical-binding: t; -*-
;;
;; Author: Cole Brown <code@brown.dev>
;; URL:    https://github.com/cole-brown/.config-emacs
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Output some things.
;;
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
;; The End is Nigh!
;;------------------------------------------------------------------------------
(nub:debug
    :innit
    (imp:file:current)
    '(:innit :finalize)
  "Boot Loader: 99 Finalize :: Everything Has Been Done.")

;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :core 'boot 'finalize 'output)

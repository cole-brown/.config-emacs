;;; core/boot/99-finalize/80-output.el --- Finally: Say Something. -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2020-11-16
;; Timestamp:  2023-06-20
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Output something to say we're done, maybe?
;;
;; Also, goodbye; you've been a lovely person.
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Finalize.
;;------------------------------------------------------------------------------

;; Tell `imp:timing' that everything's done.
(when (functionp #'imp:timing:final)
  (imp:timing:final :separator))


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

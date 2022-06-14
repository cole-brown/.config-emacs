;;; jerky/init.el --- tree/value store -*- lexical-binding: t; -*-
;;
;; Author:   Cole Brown <code@brown.dev>
;; URL:      https://github.com/cole-brown/.config-emacs
;; Created:  2020-07-14
;; Modified: 2021-05-19
;; Version:  3.2
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Tree/value store, sort of.
;; Fancy hash table, technically.
;;
;; Store values at the leaf nodes of a tree.
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Set-Up.
;;------------------------------------------------------------------------------

(imp:path:root :jerky
               (imp:path:current:dir)
               (imp:file:current))


;;------------------------------------------------------------------------------
;; Load Jerky Files.
;;------------------------------------------------------------------------------

(imp:timing
    :jerky
    (imp:file:current)
    (imp:path:current:dir)

  (imp:load :feature  '(:jerky debug)
            :filename "debug")
  ;; Initialize debugging before going any further.
  (int<jerky>:debug:init)

  (imp:load :feature  '(:jerky utils)
            :filename "utils")

  (imp:load :feature  '(:jerky jerky)
            :filename "jerky")

  ;; Always load `dlv' unless specifically removed.
  (when (int<jerky>:using:dlv?)
    (imp:load :feature  '(:jerky +dlv)
              :filename "+dlv")))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide:with-emacs :jerky)

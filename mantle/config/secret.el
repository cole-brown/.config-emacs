;;; mantle/config/secret.el --- Configure Secrets -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2023-06-28
;; Timestamp:  2023-06-28
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; ┌────────────────────────┐ Configure My Secrets ┌─────────────────────────┐
;; │               You're not allowed to know what they are.                 │
;; └─────────────────────────────┘ (shhh...) └───────────────────────────────┘
;;
;;; Code:


(imp:require :system 'secret)


;;---------------------------Configure Our Secrets------------------------------
;;--               You're not allowed to know what they are.                  --
;;---------------------------------(shhh...)------------------------------------

;; Configure our secrets if we have any.
(system:secret:config :secret 'config)


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'config 'user 'secret)

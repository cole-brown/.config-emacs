;;; 20-init.el --- Early Init -*- lexical-binding: t; -*-
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
;; Init Variables
;;------------------------------------------------------------------------------

(defconst innit:interactive? (not noninteractive)
  "If non-nil, Emacs is in interactive mode.")


(defvar innit:time nil
  "The time it took, in seconds, for Emacs & `innit' to finish set-up.")
;;------------------------------------------------------------------------------
;; The End
;;------------------------------------------------------------------------------

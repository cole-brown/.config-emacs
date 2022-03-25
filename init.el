;;; init.el --- Normal Init. -*- lexical-binding: t; -*-
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
;; Sanity Check: Has "early-init" run?
;;------------------------------------------------------------------------------
;; Two checks:
;;   1) Do we have our `init` functions?
;;   2) Do we have a successful status for early-init?
;;
;; If either check fails, error out of init.
(unless (or (fboundp 'init:status:get)
            (functionp 'init:status:get))
  (error "[ERROR] 'init.el': 'early-init.el' did not run? `init:status:get' function is not defined."))

(unless (init:status:get "00-early")
  (error "[ERROR] 'init.el': 'early-init.el' failed 'core/boot' init. `init:status' for '00-early' is: %S"
         (init:status:get "00-early")))


;;------------------------------------------------------------------------------
;; Load Init Directories in Order
;;------------------------------------------------------------------------------

(init:load "init.el" "00-bootstrap")

(init:load "init.el" "10-init")

(init:load "init.el" "20-config")

(init:load "init.el" "99-finalize")


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
;; TODO: provide imp symbol

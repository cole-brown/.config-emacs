;;; status.el --- Where were we? -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2022-04-26
;; Modified:   2022-04-26
;; URL:        https://github.com/cole-brown/.config-emacs
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;  Where were we?
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Status Variable
;;------------------------------------------------------------------------------

(defvar innit:status nil
  "Alist of innit sequence keyword to status.")


;;------------------------------------------------------------------------------
;; Status Functions
;;------------------------------------------------------------------------------

;;------------------------------
;; `innit:status' getter/setter
;;------------------------------

(defun innit:status:set (step status)
  "Set STATUS of innit STEP.

STATUS should be, at minimum, boolean.

STEP is the \"core/boot\" subdirectory path string:
  - \"[...]/core/boot/20-init/99-finalize\" -> \"20-init/99-finalize\""
  (if (null innit:status)
      (setq innit:status (list (cons step status)))
    (setf (alist-get step innit:status nil nil #'string=) status)))
;; (setq innit:status nil)
;; innit:status
;; (innit:status:set "01-test/11-foo" t)
;; (innit:status:set "01-test/11-foo" :success)
;; (innit:status:set "01-test/12-bar" :partial-success)
;; innit:status
;; (innit:status:set "01-test/13-qux" nil)


(defun innit:status:get (step)
  "Get status of innit STEP string.

nil is considered a failure state as it is the return when the sequence
keyword doesn't appear in `innit:status'.

Innit steps are \"core/boot\" subdirectory path strings:
  - \"[...]/core/boot/20-init/99-finalize\" -> \"20-init/99-finalize\""
  (alist-get step innit:status nil nil #'string=))
;; innit:status
;; (innit:status:get "01-test/11-foo")
;; (innit:status:get "01-test/12-bar")
;; (innit:status:get "01-test/13-qux")
;; (innit:status:get "01-test/14-dne")
;; (innit:status:get "98-dne/99-done")


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :innit 'status)

;;; elisp/utils/test.el --- ERT Utils -*- lexical-binding: t; -*-
;;
;; Author:   Cole Brown <code@brown.dev>
;; URL:      https://github.com/cole-brown/.config-emacs
;; Created:  2020-07-14
;; Modified: 2021-02-14
;; Version:  3.1
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; ERT (Emacs Regression Test) Utilities
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Delete ERT Tests
;;------------------------------------------------------------------------------

(defun ert:delete/all ()
  "Delete all ERT tests by calling `ert-delete-all-tests'."
  (ert-delete-all-tests))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :elisp 'utils 'test)

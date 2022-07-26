;;; tools/autogit/buffer.el -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2020-08-28
;; Modified:   2022-07-26
;; URL:        https://github.com/cole-brown/.config-emacs
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Buffer helper functions for Autogit.
;;
;;; Code:


(imp:require :autogit 'variables)
(imp:require :path)


;;------------------------------------------------------------------------------
;; Validation
;;------------------------------------------------------------------------------

;; TODO: Validate `buffer' function?
;; I know there's one somewhere that checks for a valid keyword, whereas these
;; don't. Maybe pull that out into a validation func so everyone can use it.
;; (and (keywordp buffer)
;;      (eq buffer :messages))


;;------------------------------------------------------------------------------
;; With Buffer as Context
;;------------------------------------------------------------------------------

(defun int<autogit>:macro:with-buffer/tail (buffer)
  "Make sure all windows viewing the BUFFER are viewing the tail of it.

BUFFER should be a string or buffer object."
  (let ((windows (get-buffer-window-list buffer nil t)))
    (while windows
      (set-window-point (car windows) (point-max))
      (setq windows (cdr windows)))))


(defun int<autogit>:macro:with-buffer/call (buffer body)
  "Run BODY forms then ensure the tail of the buffer is viewed.

BUFFER should be a keyword, string or buffer object.
  - If it is the keyword `:messages', use the *Messages* buffer.
  - If it is a string, insert into that named buffer.
  - Else insert into the buffer object.

Returns a sexpr of executing BODY in `with-current-buffer' block for buffer
BUFFER."
  (if (and (keywordp buffer)
           (eq buffer :messages))
      ;; Just using `message' for the *Messages* buffer.
      `(progn
         ,@body
         (int<autogit>:macro:with-buffer/tail ,buffer))
    ;; Create/get buffer to use while executing body.
    `(with-current-buffer (get-buffer-create ,buffer)
       ,@body
       (int<autogit>:macro:with-buffer/tail ,buffer))))


(defmacro int<autogit>:macro:with-buffer (buffer &rest body)
  "Run BODY then tail BUFFER.

BUFFER should be a keyword, string or buffer object.
  - If it is the keyword `:messages', use the *Messages* buffer.
  - If it is a string, insert into that named buffer.
  - Else insert into the buffer object."
  (declare (indent 1))
  (int<autogit>:macro:with-buffer/call buffer
                                        body))
;; (pp-macroexpand-expression
;;  (int<autogit>:macro:with-buffer autogit:buffer:name/push
;;                              (message "hello there")))
;; (pp-macroexpand-expression
;;  (int<autogit>:macro:with-buffer :messages
;;                              (message "hello there")))


;;------------------------------------------------------------------------------
;; Display/Switch to Buffer
;;------------------------------------------------------------------------------

(defun int<autogit>:buffer:switch (buffer)
  "Give focus to the BUFFER or not, depending on settings.

BUFFER should be a keyword, string or buffer object.
  - If it is the keyword `:messages', use the *Messages* buffer.
  - If it is a string, use that named buffer.
  - Else use the buffer object."
  (when autogit:buffer:switch?
    (pop-to-buffer (if (and (keywordp buffer)
                            (eq buffer :messages))
                       "*Messages*"
                     buffer))))
;; (int<autogit>:buffer:switch autogit:buffer:buffer/status)


(defun int<autogit>:buffer:display (buffer)
  "Display BUFFER or not, depending on settings.

BUFFER should be a keyword, string or buffer object.
  - If it is the keyword `:messages', use the *Messages* buffer.
  - If it is a string, use that named buffer.
  - Else use the buffer object."
  (when autogit:buffer:display?
    (display-buffer (if (and (keywordp buffer)
                             (eq buffer :messages))
                        "*Messages*"
                      buffer))))
;; (int<autogit>:buffer:display autogit:buffer:buffer/status)


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :autogit 'buffer)

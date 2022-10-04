;;; mis-buffer.el --- Buffer Functions -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Cole Brown
;;
;; Author: Cole Brown <code@brown.dev>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:  2019-10-23
;; Modified: 2022-08-17
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Buffer Functions
;;
;;; Code:


(require 'mis-error)
(require 'mis-tree-output)


;;------------------------------------------------------------------------------
;; Settings
;;------------------------------------------------------------------------------

(defcustom mis:buffer:name "ⓘ-mis-ⓘ"
  "Name of Mis's buffer."
  :group 'mis:group
  :type '(string))


(defcustom mis:buffer:default :messages
  "Default buffer to send Mis messages to when none is specified.

To send to the *Messages* buffer, set to one of:
  - `:messages', `messages'
  - `:message', `message'
  - nil
  - \"*Messages*\"

To send to the `mis:buffer:name' buffer, set to:
  - `mis:buffer:name'
  - `:mis', `mis'

Otherwise set to the string name of the desired buffer. Buffer will be created
if it does not exist."
  :group 'mis:group
  :type  '(choice (const :tag ":messages (*Messages*)"                      :messages)
                  (const :tag ":mis (buffer named `mis:buffer:name' value)" :mis)
                  string))


;;------------------------------------------------------------------------------
;; Buffer Functions
;;------------------------------------------------------------------------------

(defun int<mis>:buffer:type (caller &optional name)
  "Get a type keyword for buffer NAME.

NAME can be nil, a string, or a few different keywords/symbols.
If nil, use `mis:buffer:default'.
Allowable keywords/symbols:
  - `:messages', `messages'
    - Send to *Messages* buffer.
  - `:message', `message'
    - Send to *Messages* buffer.
  - `:mis', `mis'
    - Send to `mis:buffer:name' buffer.

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)

Return:
  - `:messages'
  - `:standard'"
  ;; NOTE: Keep in sync with `int<mis>:buffer:name'!
  (let ((name (or name mis:buffer:default)))
    (pcase name
      ((or :messages 'messages :message 'message)
       :messages)
      ((or :mis 'mis)
       :standard)
      ((pred bufferp)
       (if (string= "*Messages*" (buffer-name name))
           :messages
         :standard))
      ((pred stringp)
       (if (string= "*Messages*" name)
           :messages
         :standard))
      ((pred symbolp)
       :standard)
      (_
       (int<mis>:error (list 'int<mis>:buffer:name caller)
                       "Cannot determine buffer type from `%S'."
                       name)))))
;; (int<mis>:buffer:type 'test :mis)
;; (int<mis>:buffer:type 'test 'testing)
;; (int<mis>:buffer:type 'test "*Messages*")


(defun int<mis>:buffer:name (caller &optional name)
  "Get buffer name to use for output.

NAME can be nil, a string, or a few different keywords/symbols.
If nil, use `mis:buffer:default'.
Allowable keywords/symbols:
  - `:messages', `messages'
    - Send to *Messages* buffer.
  - `:message', `message'
    - Send to *Messages* buffer.
  - `:mis', `mis'
    - Send to `mis:buffer:name' buffer.

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)"
  ;; NOTE: Keep in sync with `int<mis>:buffer:name'!
  (let ((name (or name mis:buffer:default)))
    (pcase name
      ((or :messages 'messages :message 'message)
       "*Messages*")
      ((or :mis 'mis)
       mis:buffer:name)
      ((pred bufferp)
       (buffer-name name))
      ((pred stringp)
       name)
      ((pred symbolp)
       (symbol-name name))
      (_
       (int<mis>:error (list 'int<mis>:buffer:name caller)
                       "Cannot determine buffer name from `%S'."
                       name)))))
;; (int<mis>:buffer:name 'test)
;; (int<mis>:buffer:name 'test :messages)
;; (int<mis>:buffer:name 'test 'message)
;; (int<mis>:buffer:name 'test :mis)
;; (int<mis>:buffer:name 'test "jeff")
;; (int<mis>:buffer:name 'test 'jeff)
;; (int<mis>:buffer:name 'test (current-buffer))


(defun int<mis>:buffer:get-or-create (caller &optional name)
  "Get buffer object to use for output. Create if it doesn't exist.

NAME can be nil, a string, a buffer, or a few different keywords/symbols.
  - See `int<mis>:buffer:name' for details.

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)"
  (get-buffer-create
   (int<mis>:buffer:name (list 'int<mis>:buffer:get-or-create
                               caller)
                         name)))
;; (int<mis>:buffer:get-or-create 'test :messages)


(defun int<mis>:buffer:get (caller &optional name)
  "Get buffer object to use for output. Never create it.

NAME can be nil, a string, a buffer, or a few different keywords/symbols.
  - See `int<mis>:buffer:name' for details.

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)"
  (get-buffer
   (int<mis>:buffer:name (list 'int<mis>:buffer:get-or-create
                               caller)
                         name)))
;; (int<mis>:buffer:get 'test :messages)


(defun int<mis>:buffer:metadata (caller buffer output)
  "Given BUFFER name or object, put name /and/ object in Mis OUTPUT Tree.

Gets or creates the buffer object.

BUFFER should be something that `int<mis>:buffer:name' understands.

Return OUTPUT with updated metadata:
  - buffer name under `:buffer:name' key
  - buffer object under `:buffer:object' key

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)"
  (let ((caller (list 'int<mis>:buffer:metadata caller)))
    (apply #'int<mis>:output:create
           caller
           nil
           (int<mis>:output:update/metadata
            caller
            (int<mis>:output:get/metadata caller output)
            (int<mis>:output:get/metadata caller
                                          (int<mis>:output:create/entry
                                           caller
                                           nil
                                           (cons :buffer:object (int<mis>:buffer:get-or-create caller buffer))
                                           (cons :buffer:name   (int<mis>:buffer:name          caller buffer))))))))
;; (int<mis>:buffer:metadata 'test nil nil)
;; (int<mis>:output/metadata:find 'test
;;                                :buffer:name
;;                                (int<mis>:buffer:metadata 'test nil nil))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'mis-buffer)
;;; mis-buffer.el ends here

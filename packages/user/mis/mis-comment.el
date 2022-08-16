;;; mis-comment.el --- Mis API for Code Buffers/Files -*- lexical-binding: t; -*-
;;
;; Author: Cole Brown <code@brown.dev>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:  2019-10-23
;; Modified: 2022-08-11
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Mis for Code Buffers/Files
;;
;;; Code:


(require 'mis-error)
(require 'mis-valid)
(require 'mis-style)


;;------------------------------------------------------------------------------
;; Constants
;;------------------------------------------------------------------------------

(defconst int<mis>:comment:type/defaults
  '((prog-mode     . inline)
    (org-mode      . block)
    (markdown-mode . block))
  "Alist of major mode symbols to `block'/`inline' comment types.

Will check for the major mode or modes derived from it.

Try to put the most common modes at the beginning of the list.")


;;------------------------------------------------------------------------------
;; Helper Functions
;;------------------------------------------------------------------------------

(defun int<mis>:comment:type/auto (buffer)
  "Figure out what comment type (block/inline) to use for the current BUFFER.

BUFFER should be a buffer object or a buffer name string."
  (with-current-buffer (get-buffer buffer)
    ;; Can't do a simple `alist-get', since we also need to know about derived
    ;; modes. So do a linear search.
    (let ((modes int<mis>:comment:type/defaults)
          mode-assoc
          mode
          type
          type/found)
      (while (and modes
                  (null type/found))
        (setq mode-assoc (pop modes)
              mode (car mode-assoc)
              type (cdr mode-assoc))
        (when (derived-mode-p mode)
          (setq type/found type)))
      ;; Default to inline if no type known?
      (or type/found
          'inline))))
;; (int<mis>:comment:type/auto (current-buffer))


(defun int<mis>:comment:start (type &optional language)
  "Figure out what to use as start of comment.

Must be run in context of desired buffer.

TYPE should be one of:
  - `:block', `block':   Multi-line comments.
  - `:inline', `inline': Single line comments.

LANGUAGE should be nil, string, or symbol:
  - a language name for e.g. markdown source blocks?
  - an Emacs mode name for e.g. org source blocks?"
  ;;------------------------------
  ;; Error checks.
  ;;------------------------------
  (int<mis>:valid:string-symbol-nil? 'int<mis>:comment:start
                                     'language
                                     language)

  (when (int<mis>:valid:member? 'int<mis>:comment:start
                                'type
                                type
                                int<mis>:valid:comment/types)
    (setq type (int<mis>:valid:normalize->symbol 'type type)))


  ;;------------------------------
  ;; Return comment start string.
  ;;------------------------------
  (comment-normalize-vars)

  ;; Emacs already knows: If `comment-start' variable is set, that is the
  ;; correct thing to use.
  (cond (comment-start ;; TODO: block vs inline?
         (int<mis>:format:repeat comment-start (1+ (comment-add nil))))

        ;; Figure it out ourselves?
        ((eq major-mode 'org-mode)
         (if (eq type :block)
             (apply #'concat
                    "#+begin_src"
                    (if language
                        (list " " language)
                      nil))
           "~"))

        ((eq major-mode 'markdown-mode)
         (if (eq type :block)
             (apply #'concat
                    "```"
                    (if language
                        (list " " language)
                      nil))
           "`"))

        ;; Fallthrough... error?
        (t
         (int<mis>:error 'int<mis>:comment:start
                         "Don't know what to use for start of comments in `%S' mode."
                         major-mode))))


(defun int<mis>:comment:end (type &optional language)
  "Figure out what to use as end of comment.

Must be run in context of desired buffer.

TYPE should be one of:
  - `:block', `block':   Multi-line comments.
  - `:inline', `inline': Single line comments.

LANGUAGE should be nil or string:
  - a language name for e.g. markdown source blocks?
  - an Emacs mode name for e.g. org source blocks?"
  ;;------------------------------
  ;; Error checks.
  ;;------------------------------
  (int<mis>:valid:string-symbol-nil? 'int<mis>:comment:start
                                     'language
                                     language)

  (when (int<mis>:valid:member? 'int<mis>:comment:start
                                'type
                                type
                                int<mis>:valid:comment/types)
    (setq type (int<mis>:valid:normalize->symbol 'type type)))

  ;;------------------------------
  ;; Return comment end string.
  ;;------------------------------
  (comment-normalize-vars)

  ;; Emacs already knows: If `comment-end' variable is set, that is the
  ;; correct thing to use.
  (cond (comment-end) ;; TODO: block vs inline?

        ;; Figure it out ourselves?
        ((eq major-mode 'org-mode)
         (if (eq type 'block)
             "#+end_src"
           "~"))

        ((eq major-mode 'markdown-mode)
         (if (eq type 'block)
             "```"
           "`"))

        ;; Fallthrough... error?
        (t
         (int<mis>:error 'int<mis>:comment:end
                         "Don't know what to use for end of comments in `%S' mode."
                         major-mode))))


;; TODO: a func or something for "if type==block, insert-or-don't a newline
;; before-or-after-depending to separate from surrounding stuff"


;;------------------------------------------------------------------------------
;; Output API
;;------------------------------------------------------------------------------

(defun mis:comment (&rest args)
  "Parse ARGS into a Mis comment list.

ARGS should start off with styling key/values before supplying
the format string and format args. Example:
  Valid:
    (mis:comment :type 'inline \"hello world\")
    (mis:comment :type 'inline :language 'emacs-lisp \"hello %s\" (get-greeted))
  Invalid:
    (mis:comment \"hello %s\" :type 'inline (get-greeted))
    (mis:comment \"hello %s\" (get-greeted) :type 'inline :language 'emacs-lisp)

NOTE: The \"invalids\" will just be interpreted as having no comment args and
extra message args.

NOTE: Comment keyword args must always have both a keyword and a value."
  (let* ((parsed   (apply 'int<mis>:parse
                          'mis:comment
                          '(:comment :style) ; Also allow styling in our comments.
                          args))
         (comment  (plist-get parsed :comment))
         (type     (plist-get comment :type))
         (language (plist-get comment :language)))

    ;; Set `type'/`language' to a default if they are nil?
    ;; Not yet... right now just record that we'll be doing that eventually.
    (unless type
      (setq type 'default)
      (push type comment)
      (push :type comment))

    ;; Update `:comment' value in `parsed' with start/end comment strings, etc.
    (push (int<mis>:comment:end type language) comment)
    (push :postfix comment)
    (push (int<mis>:comment:start type language) comment)
    (push :prefix comment)

    (setq parsed (plist-put parsed :comment comment))))
;; (mis:comment :align 'center "hello")
;; (mis:comment :type 'inline "hello")
;; (mis:comment :type 'block :align 'center "hello %s" "world")
;; (mis:comment :type 'block :language 'csharp :align 'center :width 11 "hello %s" "world")
;; (mis:comment :width 80 "foobar")
;; (mis:comment :width 80 :align 'center "foobar")
;; (mis:comment "foobar")



;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'mis-comment)
;;; mis-comment.el ends here

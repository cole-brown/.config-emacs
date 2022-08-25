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
(require 'mis-parse)
(require 'mis-buffer)
(require 'mis-string)
(require 'mis-style)


;;------------------------------------------------------------------------------
;; Constants
;;------------------------------------------------------------------------------

(defconst int<mis>:comment:type/defaults
  '((prog-mode     . inline)
    (org-mode      . quote)
    (markdown-mode . quote))
  "Alist of major mode symbols to `block'/`inline' comment types.

Will check for the major mode or modes derived from it.

Try to put the most common modes at the beginning of the list.")


;;------------------------------------------------------------------------------
;; Helper Functions
;;------------------------------------------------------------------------------

(defun int<mis>:comment:type/get (type)
  "Finalize TYPE into one of: `inline', `block', `quote'.

Must be run in context of output buffer."
  (if (not (eq type 'default))
      ;; The specific types are already finalized.
      type
    ;; Need to figure out what "default" means...
    (let (final)
      (dolist (mode/assoc int<mis>:comment:type/defaults)
        (let ((mode (car mode/assoc))
              (default (cdr mode/assoc)))
          (when (and (null final)
                     (derived-mode-p mode))
            (setq final default))))
      ;; Final guess: inline
      (or final
          'inline))))
;; (int<mis>:comment:type/get 'default)


;; TODO: Use this somewhere?
(defun int<mis>:comment:type/auto (buffer)
  "Figure out what comment type to use for the current BUFFER.

BUFFER should be something `int<mis>:buffer:get' understands."
  (with-current-buffer (int<mis>:buffer:get buffer)
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
  - `:quote', `quote':   Multi-line comments for e.g. org, markdown.

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
         (concat (int<mis>:format:repeat comment-start (1+ (comment-add nil)))
                 ;; TODO: avoid space padding via `mis:comment' param?
                 " "))

        ;; Figure it out ourselves?
        ((eq major-mode 'org-mode)
         (if (eq type 'inline)
             "~"
           (apply #'concat
                  "#+begin_src"
                  (if language
                      (list " " language)
                    nil))))

        ((eq major-mode 'markdown-mode)
         (if (eq type 'inline)
             "`"
           (apply #'concat
                  "```"
                  (if language
                      (list " " language)
                    nil))))

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
  - `:quote', `quote':   Multi-line comments for e.g. org, markdown.

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
  (cond (comment-end ;; TODO: block vs inline?
         (if (string-empty-p comment-end)
             comment-end
           ;; TODO: avoid space padding via `mis:comment' param?
           (concat " " comment-end)))

        ;; Figure it out ourselves?
        ((eq major-mode 'org-mode)
         (if (eq type 'inline)
             "~"
           "#+end_src"))

        ((eq major-mode 'markdown-mode)
         (if (eq type 'inline)
             "`"
           "```"))

        ;; Fallthrough... error?
        (t
         (int<mis>:error 'int<mis>:comment:end
                         "Don't know what to use for end of comments in `%S' mode."
                         major-mode))))


;; TODO: a func or something for "if type==block, insert-or-don't a newline
;; before-or-after-depending to separate from surrounding stuff"


;;------------------------------------------------------------------------------
;; Output Builders
;;------------------------------------------------------------------------------

;; TODO: Handle style!
(defun int<mis>:compile:comment (caller syntax style)
  "Format Mis SYNTAX Tree using STYLE; return string.

SYNTAX should be `:mis:comment' syntax tree.

STYLE should be nil or a `:mis:style' syntax tree.
Example: (mis:style :width 80) -> '((:mis:style (:width . 80)))

Only STYLE will be checked for styling; style in SYNTAX is ignored.

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)"
  (let* ((caller  (list 'int<mis>:compile:comment caller))
         (type    (int<mis>:comment:type/get
                   (int<mis>:syntax:find caller syntax :mis:comment :type)))
         (prefix  (or (int<mis>:syntax:find caller syntax :mis:comment :prefix) ""))
         (postfix (or (int<mis>:syntax:find caller syntax :mis:comment :postfix) ""))
         (comment (int<mis>:format:syntax caller syntax)))

    ;;------------------------------
    ;; Format into a comment.
    ;;------------------------------
    ;; TODO: Indent? Trim? Etc?
    (cond ((eq type 'quote)
           ;; Simpler: Just prefix line, the string, and postfix line.
           (int<mis>:string:join/lines prefix comment postfix))

          ((eq type 'inline)
           ;; Each line gets treated the same.
           (apply #'int<mis>:string:combine/lines
                  prefix
                  postfix
                  (int<mis>:string:split/lines comment)))

          ((eq type 'block)
           ;; Prefix is just for first line; postfix is just for last line.
           ;; TODO: a prefix for the rest of the lines?
           ;; TODO: e.g. C-style:
           ;; TODO:   /* line 1
           ;; TODO:    * line 2 */
           ;; TODO: The 'rest of lines' prefix is " *".
           (concat prefix comment postfix))

          ((eq type 'default)
           ;; Special error cuz it shouldn't be `default' after `int<mis>:comment:type/get'.
           (int<mis>:error caller
                           "Comment type (%S) should not still be `default'! %S"
                           type
                           syntax))

          (t
           (int<mis>:error caller
                           "Unhandled comment type `%S' in: %S"
                           type
                           syntax)))))
;; (int<mis>:compile:comment 'test (mis:comment "hi") '((:mis:style (:width . 80))))
;; (mis:comment "hi")
;;   -> ((:mis:comment (:prefix . ";;") (:postfix . "") (:type . default)) (:mis:string . "hi"))


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
  (let* ((syntax   (apply 'int<mis>:parse
                          'mis:comment
                          '(:comment :style) ; Also allow styling in our comments.
                          args))
         ;; Block or inline comment?
         (type     (or (int<mis>:syntax:find 'mis:comment
                                             syntax
                                             :mis:comment :type)
                       'default))
         ;; Explicit comment language (e.g. for org-mode source blocks)?
         (language (int<mis>:syntax:find 'mis:comment
                                         syntax
                                         :mis:comment :language)))

    ;; Update `:comment' value in `syntax' with start/end comment strings, etc.
    (int<mis>:syntax:update 'mis:comment
                            :mis:comment
                            syntax
                            (when language
                              (cons :language language))
                            (cons :type    type)
                            (cons :postfix (int<mis>:comment:end type language))
                            (cons :prefix  (int<mis>:comment:start type language)))))
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

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
  ;; Set-up.
  ;;------------------------------
  (comment-normalize-vars)

  (int<mis>:debug 'int<mis>:comment:end
                  "language: %S"
                  language)
  (int<mis>:debug 'int<mis>:comment:end
                  "type: %S"
                  type)
  (int<mis>:debug 'int<mis>:comment:start
                  "major-mode: %S"
                  major-mode)
  (int<mis>:debug 'int<mis>:comment:start
                  "comment-start: %S"
                  comment-start)

  ;;------------------------------
  ;; Embedded Languages
  ;;------------------------------
  (cond ((eq type 'quote) ;; Are we embedding one language in another?
         (cond ((eq major-mode 'org-mode)
                (if (eq type 'inline)
                    "~"
                  (apply #'concat
                         "#+begin_src"
                         (if language
                             (list " " language)
                           nil))))

               ((and (eq major-mode 'markdown-mode)
                     (not (eq language major-mode)))
                (if (eq type 'inline)
                    "`"
                  (apply #'concat
                         "```"
                         (if language
                             (list " " language)
                           nil))))

               (t
                (int<mis>:error 'int<mis>:comment:start
                                "Don't know how to do an embedded/quoted language comment for language `%S' in mode `%S'."
                                major-mode
                                (or language
                                    'default)))))

        ;;------------------------------
        ;; Normal Case: Plain Ole Regular Normal Comments
        ;;------------------------------
        ;; Emacs already knows: If `comment-start' variable is set, that is the
        ;; correct thing to use.
        (comment-start ;; TODO: block vs inline?
         (concat (int<mis>:format:repeat comment-start (1+ (comment-add nil)))
                 ;; TODO: avoid space padding via `mis:comment' param?
                 " "))

        ;;------------------------------
        ;; Fallthrough: Error
        ;;------------------------------
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
  (int<mis>:valid:string-symbol-nil? 'int<mis>:comment:end
                                     'language
                                     language)

  (when (int<mis>:valid:member? 'int<mis>:comment:end
                                'type
                                type
                                int<mis>:valid:comment/types)
    (setq type (int<mis>:valid:normalize->symbol 'type type)))

  ;;------------------------------
  ;; Set-up.
  ;;------------------------------
  (comment-normalize-vars)

  (int<mis>:debug 'int<mis>:comment:end
                  "language: %S"
                  language)
  (int<mis>:debug 'int<mis>:comment:end
                  "type: %S"
                  type)
  (int<mis>:debug 'int<mis>:comment:end
                  "major-mode: %S"
                  major-mode)
  (int<mis>:debug 'int<mis>:comment:end
                  "comment-end: %S"
                  comment-end)

  ;;------------------------------
  ;; Embedded Languages
  ;;------------------------------
  (cond ((eq type 'quote) ;; Are we embedding one language in another?
         (cond ((eq major-mode 'org-mode)
                (if (eq type 'inline)
                    "~"
                  "#+end_src"))

               ((and (eq major-mode 'markdown-mode)
                     (not (eq language major-mode)))
                (if (eq type 'inline)
                    "`"
                  "```"))

               (t
                (int<mis>:error 'int<mis>:comment:end
                                "Don't know how to do an embedded/quoted language comment for language `%S' in mode `%S'."
                                major-mode
                                (or language
                                    'default)))))

        ;;------------------------------
        ;; Normal Case: Plain Ole Regular Normal Comments
        ;;------------------------------
        ;; Emacs already knows: If `comment-end' variable is set, that is the
        ;; correct thing to use.
        (comment-end ;; TODO: block vs inline?
         (if (string-empty-p comment-end)
             comment-end
           ;; TODO: avoid space padding via `mis:comment' param?
           (concat " " comment-end)))

        ;;------------------------------
        ;; Fallthrough: Error
        ;;------------------------------
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
  "Compile `:comment' SYNTAX using STYLE; return string.

SYNTAX should be `:comment' syntax tree.

STYLE should be nil or a `:style' syntax tree.
Example: (mis:style :width 80) -> '((:style (:width . 80)))

Only STYLE will be checked for styling; style in SYNTAX is ignored.

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)"
  (let* ((caller  (list 'int<mis>:compile:comment caller))
         (type    (int<mis>:comment:type/get
                   (int<mis>:syntax:find caller syntax :comment :type)))
         (prefix  (or (int<mis>:syntax:find caller syntax :comment :prefix) ""))
         (postfix (or (int<mis>:syntax:find caller syntax :comment :postfix) ""))
         ;; Compile our children to get the string that should be commented.
         (comment (or (int<mis>:compile:children caller :comment syntax style) "")))

    ;; TODO: Styling needs to be smarter for these..?
    ;; TODO: E.g. centering: Need to center string to width, then carve out space for prefix/postfix, then concat.
    ;;------------------------------
    ;; Style the comment.
    ;;------------------------------
    (int<mis>:style caller
                    ;;------------------------------
                    ;; Format syntax into a comment.
                    ;;------------------------------
                    ;; Need to send `int<mis>:style' a list of strings.
                    (cond ((eq type 'quote)
                           ;; Simpler: Just prefix line, the string, and postfix line.
                           (list (int<mis>:string:lines/join prefix comment postfix)))

                          ((eq type 'inline)
                           (int<mis>:string:lines/split
                            ;; Each line gets treated the same.
                            (apply #'int<mis>:string:lines/affix
                                   prefix
                                   postfix
                                   (int<mis>:string:lines/split comment))))

                          ((eq type 'block)
                           ;; Prefix is just for first line; postfix is just for last line.
                           ;; TODO: a prefix for the rest of the lines?
                           ;; TODO: e.g. C-style:
                           ;; TODO:   /* line 1
                           ;; TODO:    * line 2 */
                           ;; TODO: The 'rest of lines' prefix is " *".
                           (list (concat prefix comment postfix)))

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
                                           syntax)))
                    :comment
                    syntax
                    style)))
;; (int<mis>:compile:comment 'test (mis:comment "hi") '((:style (:width . 80))))


(int<mis>:compiler:register :comment #'int<mis>:compile:comment)


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
                          :comment
                          '(:comment :style) ; Also allow styling in our comments.
                          args))
         ;; Block or inline comment? Need comment `type' decided (i.e. not `default')
         ;; before prefix/postfix are made.
         (type     (int<mis>:comment:type/get (or (int<mis>:syntax:find 'mis:comment
                                                                        syntax
                                                                        :comment :type)
                                                  'default)))
         ;; Explicit comment language (e.g. for org-mode source blocks)?
         (language (int<mis>:syntax:find 'mis:comment
                                         syntax
                                         :comment :language)))

    ;; Update `:comment' value in `syntax' with start/end comment strings, etc.
    (int<mis>:syntax:update 'mis:comment
                            :comment
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
;; (mis:comment (mis:line "-"))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'mis-comment)
;;; mis-comment.el ends here

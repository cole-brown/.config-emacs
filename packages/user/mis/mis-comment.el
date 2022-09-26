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
;; Compiler
;;------------------------------------------------------------------------------

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
                   (int<mis>:syntax:find    caller syntax :comment :type)))
         (prefix/major  (or (int<mis>:syntax:find caller syntax :comment :prefix:major) ""))
         (postfix/major (or (int<mis>:syntax:find caller syntax :comment :postfix:major) ""))
         ;; Compile our children to get the Mis Output Tree that should be
         ;; commented.
         (mot/comment (int<mis>:compile:children caller :comment syntax style))
         output)

    (int<mis>:debug caller "syntax:        %S" syntax)
    (int<mis>:debug caller "style:         %S" style)
    (int<mis>:debug caller "type:          %S" type)
    (int<mis>:debug caller "prefix/major:  %S" prefix/major)
    (int<mis>:debug caller "postfix/major: %S" postfix/major)
    (int<mis>:debug caller "comment:       %S" mot/comment)

    ;; TODO-mot: Styling needs to be smarter for these..?
    ;; TODO-mot: E.g. centering: Need to center string to width, then carve out space for prefix/postfix, then concat.

    ;;------------------------------
    ;; Format SYNTAX into a comment.
    ;;------------------------------
    (setq output
          (cond ((eq type 'quote)
                 ;; We want to block quote this comment for e.g. `org-mode', so
                 ;; we need final full text string.
                 (let* ((mot/final      (int<mis>:output:finalize/block caller mot/comment))
                        (entries/final  (int<mis>:output:get/entries    caller mot/final))
                        (string/final   (int<mis>:output:get/string     caller (nth 0 entries/final)))
                        (metadata/final (int<mis>:output:get/metadata   caller (nth 0 entries/final))))

                   ;; Now it's simple: prefix line, the comment text, and postfix line.
                   ;; And wrap it up in a MOT for outputting.
                   (int<mis>:output:create caller
                                           (int<mis>:string:affix prefix/major
                                                                  postfix/major
                                                                  string/final)
                                           metadata/final)))

                ((eq type 'inline)
                 ;; Each line gets treated the same, so reduced the comment MOT
                 ;; down as much as possible before commenting. This will give
                 ;; us a MOT with string/metadata per newline.
                 (let ((mot/lines (int<mis>:output:finalize/lines caller mot/comment))
                       mot/return)
                   (dolist (entry/line     (int<mis>:output:get/entries  caller mot/lines))
                     (let* ((string/line   (int<mis>:output:get/string   caller entry/line))
                            (metadata/line (int<mis>:output:get/metadata caller entry/line)))
                       ;; Convert text into a comment.
                       (setq mot/return (int<mis>:output:append
                                         caller
                                         mot/return
                                         (apply #'int<mis>:output:create
                                                caller
                                                (int<mis>:string:lines/affix prefix/major
                                                                             postfix/major
                                                                             string/line)
                                                metadata/line)))))
                   ;; Done.
                   mot/return))

                ((eq type 'block)
                 ;; `prefix/major' is just for first line; `postfix/major' is
                 ;; just for last line. We must get `prefix/minor' and
                 ;; `prefix/minor' for all the other lines in order to make this
                 ;; block comment correctly.
                 ;; E.g. C-style comments are often:
                 ;;     /* line 1
                 ;;      * line 2 */
                 ;; So in that case would be:
                 ;;   - `prefix/minor'  == " *"
                 ;;   - `postfix/minor' == "" or nil
                 (let* ((prefix/minor   (or (int<mis>:syntax:find caller syntax :comment :prefix:minor) ""))
                        (postfix/minor  (or (int<mis>:syntax:find caller syntax :comment :postfix:minor) ""))
                        (mot/lines      (int<mis>:output:finalize/lines caller mot/comment))
                        (outputs/line   (int<mis>:output:get/entries    caller mot/lines))
                        (outputs/length (length outputs/line))
                        mot/return)
                   (dotimes (i outputs/length)
                     (let* ((entry/line    (nth i outputs/line))
                            (string/line   (int<mis>:output:get/string   caller entry/line))
                            (metadata/line (int<mis>:output:get/metadata caller entry/line))
                            (line/first? (= i 0))
                            (line/last?  (= i (1- outputs/length))))
                       ;; `int<mis>:output:finalize/lines' has combined/split
                       ;; the output strings so that each entry is a separate
                       ;; line. We just need to decide on the correct
                       ;; combinatior of prefix and postfix (major vs minor).
                       (setq mot/return (int<mis>:output:append
                                         caller
                                         mot/return
                                         (apply #'int<mis>:output:create
                                                caller
                                                (int<mis>:string:lines/affix (if line/first?
                                                                                 prefix/major
                                                                               prefix/minor)
                                                                             (if line/last?
                                                                                 postfix/major
                                                                               postfix/minor)
                                                                             string/line)
                                                metadata/line)))))
                   ;; Done.
                   mot/return))

                ;;------------------------------
                ;; Errors
                ;;------------------------------
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
                                 syntax))))

    (int<mis>:debug caller "output:        %S" mot/comment)

    ;;------------------------------
    ;; Style the comment.
    ;;------------------------------
    (let ((styled (int<mis>:style caller
                                  output
                                  :comment
                                  syntax
                                  style)))
      (int<mis>:debug caller "styled:        %S" styled)
      styled)))
;; (int<mis>:compile:comment 'test (mis:comment "hi") '((:style (:width . 80))))
;; (int<mis>:compile:comment 'test (mis:comment :type 'block "hi") '((:style (:width . 80))))


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
                              (cons :language      language))
                            (cons   :type          type)
                            ;; TODO: `when' for `:postfix:minor'
                            ;; TODO: `when' for `:prefix:minor'
                            (cons   :postfix:major (int<mis>:comment:end type language))
                            (cons   :prefix:major  (int<mis>:comment:start type language)))))
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

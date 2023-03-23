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
(require 'mis-int-string)
(require 'mis-tree-syntax)
(require 'mis-tree-output)
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


(defcustom mis:comment:overrides
  '((org-mode . ((inline . ((:prefix:major  . "~")
                            (:postfix:major . "~")))
                 (block .  ((:prefix:major  . "#+begin_src%s")
                            (:postfix:major . "#+end_src")))
                 (quote .  ((:prefix:major  . "#+begin_src%s")
                            (:postfix:major . "#+end_src")))))
    (markdown-mode . ((inline . ((:prefix:major  . "`")
                                 (:postfix:major . "`")))
                      (block .  ((:prefix:major  . "```%s")
                                 (:postfix:major . "```")))
                      (quote .  ((:prefix:major  . "```%s")
                                 (:postfix:major . "```"))))))
  "Override start/end strings for comments by mode.

Override `comment-start' & `comment-end' by setting `:prefix:major' &
`:postfix:major'.

Alist of major-mode to:
  Alist of comment type (`inline', `block', `quote') to:
    Alist of comment prefix/postfix (major & minor) strings.

'%s' will be replaced by `:language' value or empty string."
  :group 'mis:group
  :type  '(alist :key-type   (symbol :tag "major-mode symbol")
                 :value-type (alist :key-type   (choice (const inline)
                                                        (const block)
                                                        (const quote))
                                    :value-type (alist :key-type (choice (const :prefix:major)
                                                                         (const :postfix:major)
                                                                         (const :prefix:minor)
                                                                         (const :postfix:minor))
                                                       :value-type string))))
;; (alist-get :prefix:major (alist-get 'quote (alist-get 'org-mode mis:comment:overrides)))


;;------------------------------------------------------------------------------
;; Helper Functions
;;------------------------------------------------------------------------------

(defun int<mis>:comment:type/get (type)
  "Finalize TYPE into one of: `inline', `block', `quote'.

Must be run in context of output buffer."
  (if (and (not (eq type 'default))
           (not (memq type int<mis>:valid:clear/types)))
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
;; (int<mis>:comment:type/get 'clear)


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

  (when (int<mis>:valid:member/normalize? 'int<mis>:comment:start
                                          'type
                                          type
                                          int<mis>:valid:comment/types)
    (setq type (int<mis>:valid:normalize->symbol 'int<mis>:comment:start
                                                 'type
                                                 type)))

  ;;------------------------------
  ;; Set-up.
  ;;------------------------------
  (comment-normalize-vars)

  (int<mis>:debug 'int<mis>:comment:start
                  "language: %S"
                  language)
  (int<mis>:debug 'int<mis>:comment:start
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
        (comment-start ;; TODO-mis: block vs inline?
         (concat comment-start
                 ;; Add more `comment-start' characters if needed...
                 ;; E.g. in Emacs Lisp `comment-start' is ";" but we want comments to be ";;".
                 (if (> (comment-add nil) 0)
                     (int<mis>:format:repeat comment-start (comment-add nil))
                   "")
                 ;; TODO-mis: A way to have a padding/margin/whatever here.
                 ;;           So far, we don't want a space here more often than we do want one, so skip for now.
                 ;; " "
                 ))

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

  (when (int<mis>:valid:member/normalize? 'int<mis>:comment:end
                                          'type
                                          type
                                          int<mis>:valid:comment/types)
    (setq type (int<mis>:valid:normalize->symbol "int<mis>:comment:end"
                                                 'type
                                                 type)))

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


(defun int<mis>:comment:get (caller syntax language type position)
  "Get comment characters string to prepend/postpend to the message string.

SYNTAX should be a Mis Syntax Tree. It will be checked for prefix/postfix
comment strings first, then the overrides in `mis:comment:overrides' will be
checked, then Emacs will be checked (e.g. `comment-start').

TYPE should be a return vaule from `int<mis>:comment:type/get'.

POSITION should be one of:
  - `:prefix:major'  - start of comment
  - `:postfix:major' - end of comment
  - `:prefix:minor'  - start of non-first line of multi-line comments
  - `:postfix:minor' - end of non-last line of multi-line comments

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)"
  (let ((caller (list 'int<mis>:comment:get caller)))
    (int<mis>:debug caller
                    "syntax:   %S"
                    syntax)
    (int<mis>:debug caller
                    "language: %S"
                    language)
    (int<mis>:debug caller
                    "type:     %S"
                    type)
    (int<mis>:debug caller
                    "position: %S"
                    position)
    ;;------------------------------
    ;; Error Checking
    ;;------------------------------
    (int<mis>:valid:comment/kvp? caller :type type)
    (int<mis>:valid:member/exact? caller
                                  'position
                                  position
                                  '(:prefix:major :prefix:minor :postfix:major :postfix:minor))
    (int<mis>:valid:string-symbol-nil? 'int<mis>:comment:end
                                       'language
                                       language)

    ;;------------------------------
    ;; Figure out the comment strings.
    ;;------------------------------
    (cond
     ;;---
     ;; Contained in Mis Syntax Tree?
     ;;---
     ((int<mis>:syntax:find caller
                            syntax
                            :comment position))

     ;;---
     ;; Contained in Override?
     ;;---
     ((alist-get position
                   (alist-get type
                              (alist-get major-mode mis:comment:overrides))))

     ;;---
     ;; Major Comment Delimiters:
     ;;---
     ;; Need to avoid `int<mis>:comment:start'/`int<mis>:comment:end' if
     ;; `comment-normalize-vars' would cause a user prompt. It prompts when
     ;; there's no `comment-start', so:
     ((and (eq position :prefix:major)
           (not (null comment-start)))
      (int<mis>:comment:start type language))

     ;; Same for the postfix. And still just check `comment-start' to avoid prompt.
     ((and (eq position :postfix:major)
           (not (null comment-start)))
      (int<mis>:comment:end type language))

     ;;---
     ;; Minor Comment Delimiters:
     ;;---
     ((memq position '(:prefix:minor :postfix:minor))
      ;; TODO: only available via overrides or... can we get from somewhere else?
      ;; Dunno, but we're calling this for when it starts working here so... just return nil.
      nil)

     ;;---
     ;; Fallthrough: Error
     ;;---
     (t
      (int<mis>:error caller
                      '("Don't know what to use to create a comment for: %S %S %S. "
                        "No overrides in `mis:comment:overrides' and nothing from Emacs.")
                      language
                      type
                      position)))))
;; (int<mis>:comment:get 'test 'lisp :inline :prefix:major)
;; (int<mis>:comment:get 'test 'lisp :inline :prefix:major)


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
         (postfix/major (or (int<mis>:syntax:find caller syntax :comment :postfix:major) "")))

    (int<mis>:debug caller "syntax:        %S" syntax)
    (int<mis>:debug caller "style:         %S" style)
    (int<mis>:debug caller "type:          %S" type)
    (int<mis>:debug caller "prefix/major:  %S" prefix/major)
    (int<mis>:debug caller "postfix/major: %S" postfix/major)

    ;; Compile our children to get the Mis Output Tree that should be commented.
    (let ((mot/comment (int<mis>:compile:children caller
                                                  :comment
                                                  syntax
                                                  style))
          output)
      (int<mis>:debug caller "comment:       %S" mot/comment)

      ;;------------------------------
      ;; Format SYNTAX into a comment.
      ;;------------------------------
      (setq output
            ;;------------------------------
            ;; Single-Line Comments
            ;;------------------------------
            (cond ((eq type 'inline)
                   ;; Need to add comment prefix/postfix to each line, so finalize based on lines.
                   (let* ((mot/lines      (int<mis>:output:finalize/lines caller mot/comment))
                          (outputs/line   (int<mis>:output:get/entries    caller mot/lines))
                          (outputs/length (length outputs/line))
                          mot/return)
                     (dotimes (i outputs/length)
                       (let* ((entry/line    (nth i outputs/line))
                              (string/line   (int<mis>:output:get/string   caller entry/line))
                              (metadata/line (int<mis>:output:get/metadata caller entry/line)))
                         ;; `int<mis>:output:finalize/lines' has combined/split
                         ;; the output strings so that each entry is a separate
                         ;; line. We just need to decide on the correct
                         ;; combinatior of prefix and postfix (major vs minor).
                         (setq mot/return
                               (int<mis>:output:append
                                caller
                                mot/return
                                (int<mis>:output/string:affix caller
                                                              prefix/major
                                                              postfix/major
                                                              entry/line)))))
                     ;; Done.
                     mot/return))

                  ;;------------------------------
                  ;; Multi-Line Comments
                  ;;------------------------------
                  ((eq type 'quote)
                   (let* ((prefix/minor   (or (int<mis>:syntax:find caller syntax :comment :prefix:minor) ""))
                          (postfix/minor  (or (int<mis>:syntax:find caller syntax :comment :postfix:minor) ""))
                          (mot/block      (int<mis>:output:finalize/block caller mot/comment))
                          (outputs/quote  (int<mis>:output:get/entries    caller mot/block)))
                     (if (> (length outputs/quote) 1)
                         (int<mis>:error caller
                                         "Comment type (%S) should only have one string to work with after finalizing! %S -> %S"
                                         type
                                         syntax
                                         outputs/quote)
                       ;; Now it's simple: prefix line, the finalized comment, and postfix line.
                       (int<mis>:output/string:affix caller
                                                     prefix/major
                                                     postfix/major
                                                     (car mot/entries/final)))))

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
                         (setq mot/return
                               (int<mis>:output:append
                                caller
                                mot/return
                                (int<mis>:output/string:affix caller
                                                              (if line/first?
                                                                  prefix/major
                                                                prefix/minor)
                                                              (if line/last?
                                                                  postfix/major
                                                                postfix/minor)
                                                              entry/line)))))
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

      (int<mis>:debug caller "output:        %S" output)

      ;;------------------------------
      ;; Style the comment.
      ;;------------------------------
      (let ((styled (int<mis>:style caller
                                    output
                                    :comment
                                    syntax
                                    style)))
        (int<mis>:debug caller "styled:        %S" styled)
        styled))))
;; (int<mis>:compile:comment 'test (mis:comment "hi") '((:style (:width . 80))))
;; (int<mis>:compile:comment 'test (mis:comment :type 'block "hi") '((:style (:width . 80))))
;;
;; alignment-aware commenting:
;;   (int<mis>:compile:comment 'test (mis:comment :align 'center :width 80 "hello there") '((:style (:width . 80))))
;; vs normal centered string:
;;   (int<mis>:compile 'test (mis:string :align 'center :width 80 "hello there") nil)


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
                            ;; The comment start/end strings.
                            (cons   :prefix:major  (int<mis>:comment:get 'test syntax language type :prefix:major))
                            (cons   :postfix:major (int<mis>:comment:get 'test syntax language type :postfix:major))
                            ;; Multiline comment middle lines start/end strings?
                            (when-let ((prefix/minor  (int<mis>:comment:get 'test syntax language type :prefix:minor)))
                              (cons :prefix:minor  prefix/minor))
                            (when-let ((postfix/minor (int<mis>:comment:get 'test syntax language type :postfix:minor)))
                              (cons :postfix:minor postfix/minor)))))
;; (mis:comment :align 'center "hello")
;; (mis:comment :type 'inline "hello")
;; (mis:comment :type 'block :align 'center "hello %s" "world")
;; (mis:comment :type 'block :language 'csharp :align 'center :width 11 "hello %s" "world")
;; (mis:comment :width 80 "foobar")
;; (mis:comment :width 80 :align 'center "foobar")
;; (mis:comment "foobar")
;; (mis:comment (mis:line "-"))
;; (mis:comment :prefix:major "/*" :postfix:major "*/" :prefix:minor " *" :postfix:minor "" "foobar")


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'mis-comment)
;;; mis-comment.el ends here

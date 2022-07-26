;;; tools/autogit/output.el --- Output Functions  -*- lexical-binding: t; -*-
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
;; Functions for manipulating strings and displaying output to a buffer.
;;
;;; Code:


(require 'subr-x)

(imp:require :autogit 'variables)
(imp:require :autogit 'buffer)
(imp:require :autogit 'repo)


;;------------------------------------------------------------------------------
;; Strings
;;------------------------------------------------------------------------------

(defun int<autogit>:string:indent (indent level)
  "Get indent string for indent LEVEL from INDENT.

LEVEL must be a wholenum - 0 is 'not indented'.

INDENT can be:
  - wholenum: a string of spaces of length (INDENT * LEVEL).
  - string:   returns the string repeated LEVEL times.
  - list:     returns the string at element (LEVEL - 1) or \"\" if LEVEL is 0."
  (cond ((not (wholenump level))
         (error "int<autogit>:string:indent: LEVEL must be a wholenum, got: %S"
                level))

        ((listp indent)
         (if (= level 0)
             ""
           ;; Pretend level 1 indent so no enlarging of the list's indent.
           (int<autogit>:string:indent (seq-elt indent (1- level)) 1)))

        ((wholenump indent)
         (make-string (* indent level) ?\s))

        ((stringp indent)
         (let (indents)
           (dotimes (_ level)
             (setq indents (cons indent indents)))
           (apply 'concat indents)))

        (t
         (error "int<autogit>:string:indent: Unknown INDENT type: %S %S"
                (type-of indent) indent))))
;; (int<autogit>:string:indent 4 1)
;; (int<autogit>:string:indent 4 2)
;; (int<autogit>:string:indent "  " 1)
;; (int<autogit>:string:indent "  " 2)
;; (int<autogit>:string:indent '("hello" "there") 1)
;; (int<autogit>:string:indent '("hello" "there") 2)


(defun int<autogit>:string:propertize (&rest args)
  "Parse ARGS to created a propertized string.

ARGS is a plist. Keys/Values are:
  - :prop / :property
    + Value must be a list of properties,
      or a single keyword from `autogit:text:properties'.
  - :text
    + Value must be either a single string,
      or a list of (format-string arg0 ...) pass through `format'.

PROPS should be either a keyword from `int<autogit>:properties' or actual string
properties."
  (let (properties
        text)
    ;; Build the string.
    (let* ((args/text (plist-get args :text))
           ;; string is either only thing, or first in list.
           (fmt/str  (if (stringp args/text)
                         args/text
                       (nth 0 args/text)))
           ;; args either don't exist, or are the rest of the list.
           (args/str (if (stringp args/text)
                         nil
                       (cdr args/text))))
      (if (not args/str)
          (if fmt/str
              ;; Only had a string, set it as text and we're done "building".
              (setq text fmt/str)
            ;; No args and no text; error.
            (error (concat "int<autogit>:string:propertize: "
                           "Must have text to propertize. Found nothing "
                           "for key `:text' in args: %S")
                   args))

        ;; Have args.
        (if (not fmt/str)
            ;; Args but no string... Don't know what to do with this.
            (error (concat "int<autogit>:string:propertize: "
                           "Must have a string to propertize as "
                           "`:text' key's value, or as first element in "
                           "`:text' key's value's list. Got `:text' value: %S")
                   args/text)

          ;; Build text.
          (setq text (apply #'format fmt/str args/str)))))

    ;; Check for a property.
    (when-let ((prop (or (plist-get args :prop)
                         (plist-get args :property))))
      (cond ((keywordp prop)
             ;; Just a single keyword - try to process it.
             (if-let ((autogit/props (plist-get autogit:text:properties prop)))
                 (dolist (ag/prop autogit/props)
                   (push ag/prop properties))
               (push prop properties)))

            ((listp prop)
             ;; List of keywords - try to process them.
             (dolist (each props)
               (cond ((keywordp each)
                      (if-let ((autogit/props (plist-get autogit:text:properties each)))
                          (dolist (ag/prop autogit/props)
                            (push ag/prop properties))
                        (push each properties)))

                     ;; TODO: More conditions as we improve this.
                     ;; Or switch to if-else.
                     (t
                      (push each properties)))))

            (t
             ;; Unknown so... error.
             (error (concat "int<autogit>:string:propertize: "
                            "Found `:prop' keyword, but don't know how to "
                            "process its value. Expected keyword or list; "
                            "got: %S (type: %S)")
                    prop (type-of prop)))))

    ;; Apply the props if we have any and return string.
    (if (null properties)
        ;; Just the text (no properties for it).
        text
      ;; Properties are backwards, so fix that.
      (apply #'propertize text (nreverse properties)))))
;; (int<autogit>:string:propertize :text "hello there" :prop :autogit)
;; (int<autogit>:display:message :messages (int<autogit>:string:propertize "hello there" :autogit))
;; (propertize "hello there" 'face 'package-name)


(defun int<autogit>:string:finalize (args)
  "Create a (propertized) string ready to be output to a buffer.

ARGS should be a list and each item should be one of:
  - A string.
  - A plist for propertizing a string.
    + With requried key `:text', value of a string or list of
      format-string and format-args.
    + With optional key `:prop' or `:property', value of a keyword from
      `int<autogit>:string:propertize'.
  - A list of: a format-string and format-args.

No padding between args is created."
  (let (text/list)
    (dolist (arg args)
      (cond ((null arg)
             ;; Allow nil/empty list for conditional things. Just ignore it.
             nil)

            ((stringp arg)
             (push arg text/list))

            ;; Plist
            ((and (listp arg)
                  (keywordp (nth 0 arg)))
             (push (apply #'int<autogit>:string:propertize arg) text/list))

            ;; Regular list
            ((listp arg)
             (push (apply #'format arg) text/list))

            ;; Unknown - Error
            (t
             (error "`int<autogit>:string:finalize': Cannot process input: %S"
                    arg))))

    (when text/list
      ;; Combine the list and we're done.
      (apply #'concat (nreverse text/list)))))


;;------------------------------------------------------------------------------
;; Output/Display to Buffer
;;------------------------------------------------------------------------------

(defun int<autogit>:display:insert (buffer message)
  "Insert finalized MESSAGE into BUFFER.

BUFFER should be a keyword, string or buffer object.
  - If it is the keyword `:messages', output to the *Messages* buffer using
    `message'.
  - Else inserts into BUFFER using `int<autogit>:macro:with-buffer'.

MESSAGE should be output from `int<autogit>:string:finalize'."
  (cond ((and (keywordp buffer)
              (eq buffer :messages))
         ;; Using *Messages* buffer, so just use the `message' function to put the
         ;; message there.
         (message message))

        ;; Some other keyword: error.
        ((keywordp buffer)
         (error "`int<autogit>:display:insert': unknown buffer keyword: %S" buffer))

        ;; Buffer obj or str:
        ((or (stringp buffer)
             (bufferp buffer))
         (int<autogit>:macro:with-buffer buffer
                                         ;; We are now in BUFFER, so just insert the formatted string on a new line at the end.
                                         (goto-char (point-max))
                                         (insert (concat "\n" message))))

        ;; Fallthrough error.
        (t
         (error "`int<autogit>:display:insert': unhandled buffer type %S %S"
                (type-of buffer) buffer))))


(defun int<autogit>:display:message (buffer &rest args)
  "Create a (propertized) string and outputs it to BUFFER.

BUFFER should be a keyword, string or buffer object.
  - If it is the keyword `:messages', output to the *Messages* buffer using
    `message'.
  - Else inserts into BUFFER using `int<autogit>:macro:with-buffer'.

ARGS should each be one of:
  - A string.
  - A plist for propertizing a string.
    + With requried key `:text', value of a string or list of
      format-string and format-args.
    + With optional key `:prop' or `:property', value of a keyword from
      `int<autogit>:string:propertize'.
  - A list of: a format-string and format-args.

No padding between args is created.

NOTE: If BUFFER is not `:message', it will print to the current buffer,
so it must be used inside the `int<autogit>:macro:with-buffer' macro body!"
  (int<autogit>:display:insert buffer (int<autogit>:string:finalize args)))
;; (let ((buffer autogit:buffer:name/status))
;;     (int<autogit>:display:message buffer
;;                         (list :prop :face:self :text autogit:text:name)
;;                         ": "
;;                         "Status of "
;;                         (list :prop :face:highlight
;;                               :text (list "%d" (length autogit:repos:path/watch)))
;;                         " watch locations...")
;;     (int<autogit>:buffer:display buffer)))


(defun int<autogit>:display:message/indented (buffer indent &rest args)
  "Create an indented, propertized message and outputs it to BUFFER.

BUFFER should be a keyword, string or buffer object.
  - If it is the keyword `:messages', output to the *Messages* buffer using
    `message'.
  - Else inserts into BUFFER using `int<autogit>:macro:with-buffer'.

INDENT should be a wholenum or a string.
  - wholenum: indent by that number of spaces
  - string: use that string to indent

Each line of the final message will be indented.

ARGS should each be one of:
  - A string.
  - A plist for propertizing a string.
    + With requried key `:text', value of a string or list of
      format-string and format-args.
    + With optional key `:prop' or `:property', value of a keyword from
      `int<autogit>:string:propertize'.
  - A list of: a format-string and format-args.

No padding between args is created.

NOTE: If BUFFER is not `:message', it will print to the current buffer,
so it must be used inside the `int<autogit>:macro:with-buffer' macro body!"
  (let ((msg (int<autogit>:string:finalize args))
        (indent-str (cond ((wholenump indent)
                           (make-string indent ?\s))
                          ((stringp indent)
                           indent)
                          (t
                           ""))))
    ;; Apply indent to all lines in message.
    (when indent-str
      (let ((lines (split-string msg "\n"))
            indented)
        (dolist (line (nreverse lines))
          (push (concat indent-str line) indented))
        (setq msg (mapconcat 'identity indented "\n"))))

    (int<autogit>:display:insert buffer msg)))


(defun int<autogit>:display:newline (buffer)
  "Insert a newline into autogit output BUFFER."
  (int<autogit>:display:message buffer ""))


(defun int<autogit>:display:break (buffer &optional force)
  "Insert a section break into BUFFER.

BUFFER should be a keyword, string or buffer object.
  - If it is the keyword `:messages', output to the *Messages* buffer using
    `message'.
  - Else inserts into BUFFER using `int<autogit>:macro:with-buffer'.

If FORCE is non-nil, always output a section break. Else only output one if
insert point in BUFFER is not the very beginning of the buffer."
  ;; Add section break if forced or we think it's needed.
  (when (or force
            (> (point) 1))
    ;; TODO: Make these from defcustoms?: num newlines, padding cons, padding char, width-or-use-fill-column.
    (let* ((newlines (make-string 2 ?\n))
           (padding '("┌" . "┐")))
      (int<autogit>:display:message buffer
                                    (list :prop :face:section
                                          :text (concat newlines
                                                        (car padding)
                                                        (make-string (- fill-column
                                                                        (length (car padding))
                                                                        (length (cdr padding)))
                                                                     (string-to-char "─"))
                                                        (cdr padding)))))))


;;------------------------------------------------------------------------------
;; Higher-Level Output Functions:
;;------------------------------------------------------------------------------

(defun int<autogit>:display:status (buffer alist/changes)
  "Output a status message to BUFFER about a git repo sub-dir's ALIST/CHANGES."
  ;; Error checking.
  (unless (memq autogit:changes:display int<autogit>:changes:display/valid)
    (error "`autogit:changes:display' (%S) is not a valid setting. Set it to one of: %S"
           autogit:changes:display
           int<autogit>:changes:display/valid))

  ;; Title or whatever is not our responsibility.

  (let (indent)
    ;; Should we display summary?
    (when (memq autogit:changes:display '(:summary :full))
      (int<autogit>:display:message buffer
                                   (list :prop :face:title
                                         :text "  Status: ")
                                   (list :prop :face:highlight
                                         :text (int<autogit>:changes:summary alist/changes)))
      (setq indent t))

    ;; List, if desired.
    (when (memq autogit:changes:display '(:paths :full))
      ;; Extra indentation if we also printed summary.
      (setq indent (if indent
                       "    "
                     "  "))
      (dolist (entry alist/changes)
        ;; TODO: Keyword (:staged, :unstaged, etc) to display string?
        (if (null (cdr entry))
            ;; No changes for this dir.
            (int<autogit>:display:message buffer
                                         (list "%s" indent)
                                         (list :prop :face:title :text (list "%s" (car entry)))
                                         ": "
                                         (list :prop :face:highlight :text "None."))
          ;; Show type (staged, unstaged...) and list of change.
          (int<autogit>:display:message buffer
                                       (list "%s" indent)
                                       (list :prop :face:title :text (list "%s" (car entry)))
                                       ":")
          (dolist (path (cdr entry))
            (int<autogit>:display:message buffer
                                         (list "%s  - " indent)
                                         (list :prop :face:path :text path))))))))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :autogit 'output)

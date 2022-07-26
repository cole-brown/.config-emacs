;;; tools/autogit/status.el -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2020-08-28
;; Modified:   2022-07-25
;; URL:        https://github.com/cole-brown/.config-emacs
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Commands for:
;;   - Getting general status for certain other git repos.
;;     + E.g. get status for your notes repo(s) and your code repo(s) in one go.
;;
;;; Code:


(require 'subr-x)

;; TODO: How to get this lazy loaded?
;;   - Take the require out of the funcs when figured out.
;; (require 'magit)

(imp:require :autogit 'variables)
(imp:require :path)


;;------------------------------------------------------------------------------
;; Paths & Files
;;------------------------------------------------------------------------------

(defun int<autogit>:path:join (root &rest path)
  "Return absolute (file) path to ROOT + PATH.

Given a git ROOT, and a PATH of e.g. ('path/to' 'dir' 'with-file' 'file.txt'),
will return full /file/ path in platform-agnostic manner."
  (if (imp:feature? :path)
      (apply #'path:absolute:file root path)
    (concat (file-name-as-directory (expand-file-name "" root))
            (directory-file-name (mapconcat #'file-name-as-directory path "")))))
;; (int<autogit>:path:join (car autogit:repos:path/commit) "foo")


(defun int<autogit>:path:changes/rel->abs (path-abs alist/changes)
  "Convert relative paths to absolute.

Converts all of ALIST/CHANGES' relative paths to absolute paths given
a PATH-ABS inside of the git repository."
  (let* ((default-directory path-abs)
         (git-root (magit-toplevel))
         results)
    ;; Have alist of staged, unstaged, etc files.
    (dolist (entry alist/changes results)
      ;; Convert to absolute paths.
      (push (cons (car entry)
                  (mapcar (lambda (x) (int<autogit>:path:join git-root x))
                          (cdr entry)))
            results))))
;; (int<autogit>:path:changes/rel->abs default-directory (int<autogit>:changes:in-repo default-directory))


;;------------------------------------------------------------------------------
;; Repo: Changes
;;------------------------------------------------------------------------------

(defun int<autogit>:changes:in-repo (subdir-abs)
  "Find changed files in a repository.

Determine if magit knows of any changes (staged, unstaged, untracked, unmerged)
in the repo that SUBDIR-ABS is a sub-directory of.

SUBDIR-ABS should be an absolute path string.

Return an alist of changes:
  '((:staged    . <list of filenames or nil>)
    (:unstaged  . <list of filenames or nil>)
    (:untracked . <list of filenames or nil>)
    (:unmerged  . <list of filenames or nil>))"
  ;; Magit works on `default-directory', so make sure to set that. Also magit
  ;; returns lowercase paths, so make sure to downcase for Windows.
  (let* ((subdir-abs (downcase subdir-abs))
         (default-directory subdir-abs))
    ;; These are all changes in repo, not subdir
    (list (cons :staged    (magit-staged-files))
          (cons :unstaged  (magit-unstaged-files))
          (cons :untracked (magit-untracked-files))
          (cons :unmerged  (magit-unmerged-files)))))
;; (int<autogit>:changes:in-repo default-directory)


(defun int<autogit>:changes:in-subdir (subdir-abs)
  "Find changed files under a repository's sub-directory.

Determine if magit knows of any changes (staged, unstaged, untracked, unmerged)
in the repo that SUBDIR-ABS is a sub-directory of. Then filter them down to just
changes that are in/under SUBDIR-ABS.

SUBDIR-ABS should be an absolute path string.

Return an alist of changes:
  '((:staged    . <list of filenames or nil)
    (:unstaged  . <list of filenames or nil)
    (:untracked . <list of filenames or nil)
    (:unmerged  . <list of filenames or nil))"
  (let ((alist/changes (int<autogit>:path:changes/rel->abs subdir-abs
                                                           (int<autogit>:changes:in-repo subdir-abs)))
        results)
    ;; Need to filter each category of changes down to just the subdir requested.
    (dolist (entry alist/changes results)
      (push (cons (car entry)
                  (seq-filter (lambda (x) (string-prefix-p subdir-abs x)) (cdr entry)))
            results))))
;; (int<autogit>:changes:in-subdir default-directory)


;; TODO: Ahead/behind upstream.
;;   - getter
;;   - summarizer
;; (cons "↑" commits-ahead)
;; (cons "↓" commits-behind)


(defun int<autogit>:changes:summary (alist/changes)
  "Convert ALIST/CHANGES into a summary string.

ALIST/CHANGES should be output of `int<autogit>:changes:in-subdir' or
`int<autogit>:changes:in-repo':
  '((:staged    . <list of filenames or nil)
    (:unstaged  . <list of filenames or nil)
    (:untracked . <list of filenames or nil)
    (:unmerged  . <list of filenames or nil))

Will convert using `autogit:changes:symbols' custom var."
  (mapconcat
   (lambda (entry)
     "Convert alist ENTRY into a string of symbol (according to ENTRY's key) and integer (length of ENTRY's value)."
     (when-let ((symbol (alist-get (car entry) autogit:changes:symbols))
                (entries (length (cdr entry))))
       ;; TODO: if zero, display?
       ;;   - 0            (always)
       ;;   - blank space  (blank)
       ;;   - empty string (collapse)
       (concat symbol (number-to-string entries))))
   alist/changes
   " ")
  ;; TODO: also want ahead/behind upstream info?
  )
;; (int<autogit>:changes:summary (int<autogit>:changes:in-subdir default-directory))


;; TODO: int<autogit>:changes:summary/multiple-repos-fmt
;;   - Take in alist of repo-path to alist/changes.
;;   - Check entire alist for how to format:
;;     - numbers
;;       - e.g. 123 changed files in foo repo, so use 3 digits in all fields
;;     - column widths
;;       - e.g. longest repo-path is 88 chars, so path column is 88 chars wide
;;   - Create output alist:
;;     '(("<repo-path-0> formatted to width: " "formatted status string")
;;       (etc ...)
;;       ...)


(defun int<autogit>:changes:commit-filter (alist/changes)
  "Return either list of files to commit or a keyword.

Returns a keyword if you shouldn't commit changes.
  - :unmerged - unmerged changes exist - do not commit.
  - :no-op    - No staged/unstaged/untracked to commit.

ALIST/CHANGES should be output of `int<autogit>:changes:in-subdir' or
`int<autogit>:changes:in-repo':
  '((:staged    . <list of filenames or nil)
    (:unstaged  . <list of filenames or nil)
    (:untracked . <list of filenames or nil)
    (:unmerged  . <list of filenames or nil))

Returns list of files to commit otherwise - collapses alist down into a
single list."
  ;; Disallow because unmerged?
  (cond ((> (length (alist-get :unmerged alist/changes))
            0)
         :unmerged)

        ;; Nothing to do.
        ((and (= (length (alist-get :staged alist/changes)) 0)
              (= (length (alist-get :unstaged alist/changes)) 0)
              (= (length (alist-get :untracked alist/changes)) 0))
         :no-op)

        ;; Ok; it is allowed.
        (t
         ;; Collapse `alist/changes' down into just paths.
         (append (alist-get :staged alist/changes)
                 (alist-get :unstaged alist/changes)
                 (alist-get :untracked alist/changes)
                 ;; :unmerged must be empty since we got here, so ignore.
                 ))))


;;------------------------------------------------------------------------------
;; Magit
;;------------------------------------------------------------------------------

(defmacro int<autogit>:magit/with-errors (&rest body)
  "Set magit error flag and run BODY."
  `(let ((magit-process-raise-error t))
     ,@body))


(defun int<autogit>:magit:fetch (dry-run buffer indent)
  "Fetch from remotes (w/ prune) and then pull current branch.

DRY-RUN should be nil/non-nil.

BUFFER should be a keyword, string or buffer object.
  - If it is the keyword `:messages', output to the *Messages* buffer using
    `message'.
  - Else inserts into BUFFER using `int<autogit>:macro:with-buffer'.

INDENT must be a wholenum - 0 is 'not indented'."
  (int<autogit>:magit/with-errors
   ;;------------------------------
   ;; Fetch from remotes.
   ;;------------------------------
   (int<autogit>:output:message/indented buffer
                                         (int<autogit>:output:indent indent 1)
                                         ;; Dry-Run prefix?
                                         (list :prop :face:failure
                                               :text (concat (if dry-run
                                                                 autogit:text:dry-run
                                                               "GIT")
                                                             ":")))
   (int<autogit>:output:message/indented buffer
                                         (int<autogit>:output:indent indent 2)

                                         ;; Command or message.
                                         (list :prop :face:git
                                               :text "`magit-fetch-all-prune'"))
   (magit-fetch-all-prune)

   ;;------------------------------
   ;; Pull current branch.
   ;;------------------------------
   (int<autogit>:output:message/indented buffer
                                         (int<autogit>:output:indent indent 1)
                                         ;; Dry-Run prefix?
                                         (list :prop :face:failure
                                               :text (concat (if dry-run
                                                                 autogit:text:dry-run
                                                               "GIT")
                                                             ":")))
   (int<autogit>:output:message/indented buffer
                                         (int<autogit>:output:indent indent 2)

                                         ;; Command or message.
                                         (list :prop :face:git
                                               :text "`magit-pull-from-upstream'"))
   (magit-pull-from-upstream nil)))


;; TODO: use in status
(defun int<autogit>:magit:head-name (subdir-abs)
  "Return a string about what the repo's HEAD is.

SUBDIR-ABS should be an absolute path string.

Returned string will be either:
  - Name of branch.
  - First seven of hash."
  ;; TODO: Does this work in a brand new git repo?
  (let* ((subdir-abs (downcase subdir-abs))
         (default-directory subdir-abs)
         (headish (magit-headish))
         (branch (magit-name-branch headish)))
    (or branch headish)))
;; (int<autogit>:magit:head-name default-directory)


(defun int<autogit>:magit:git (dry-run buffer indent message &rest args)
  "Call `magit-call-git' with ARGS (unless DRY-RUN is nil).

BUFFER should be a keyword, string or buffer object.
  - If it is the keyword `:messages', output to the *Messages* buffer using
    `message'.
  - Else inserts into BUFFER using `int<autogit>:macro:with-buffer'.

INDENT can be a string or a wholenum (0 is 'not indented').

If MESSAGE is a string, prepend with INDENT (if str) or INDENT number of spaces
\(if `wholenump').

If MESSAGE is `:args-as-msg', message string will be INDENT (as
above) plus 'git' plus the ARGS provided.

Example:
  (int<autogit>:magit:git 2 :args-as-msg  \"add\" \"-A\" \".\")
     -message-> \"  git add -A .\"
  (int<autogit>:magit:git \"calling git: \"
                      \"adding all changes...\"
                      \"add\" \"-A\" \".\")
     -message-> \"calling git: adding all changes...\""
  (int<autogit>:magit/with-errors
   ;; Output messages first.
   (int<autogit>:output:message/indented buffer
                                         (int<autogit>:output:indent indent 1)

                                         ;; Dry-Run prefix?
                                         (list :prop :face:failure
                                               :text (concat (if dry-run
                                                                 autogit:text:dry-run
                                                               "GIT")
                                                             ":")))
   (int<autogit>:output:message/indented buffer
                                         (int<autogit>:output:indent indent 2)

                                         ;; Command or message.
                                         (list :prop :face:git
                                               :text (if (eq message :args-as-msg)
                                                         (concat "git " (string-join args " "))
                                                       message)))

   ;; Do not actually run the command in dry-run mode.
   (unless dry-run
     (apply #'magit-call-git args))))


;;------------------------------------------------------------------------------
;; Buffer
;;------------------------------------------------------------------------------

;; TODO: Validate `buffer' function?
;; I know there's one somewhere that checks for a valid keyword, whereas these
;; don't. Maybe pull that out into a validation func so everyone can use it.


(defun int<autogit>:macro:with-buffer//tail (buffer)
  "Make sure all windows viewing the BUFFER are viewing the tail of it.

BUFFER should be a string or buffer object."
  (let ((windows (get-buffer-window-list buffer nil t)))
    (while windows
      (set-window-point (car windows) (point-max))
      (setq windows (cdr windows)))))


(defun int<autogit>:macro:with-buffer//call (buffer body)
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
         (int<autogit>:macro:with-buffer//tail ,buffer))
    ;; Create/get buffer to use while executing body.
    `(with-current-buffer (get-buffer-create ,buffer)
       ,@body
       (int<autogit>:macro:with-buffer//tail ,buffer))))


(defmacro int<autogit>:macro:with-buffer (buffer &rest body)
  "Run BODY then tail BUFFER.

BUFFER should be a keyword, string or buffer object.
  - If it is the keyword `:messages', use the *Messages* buffer.
  - If it is a string, insert into that named buffer.
  - Else insert into the buffer object."
  (declare (indent 1))
  (int<autogit>:macro:with-buffer//call buffer
                                        body))
;; (pp-macroexpand-expression
;;  (int<autogit>:macro:with-buffer autogit:buffer:name/push
;;                              (message "hello there")))
;; (pp-macroexpand-expression
;;  (int<autogit>:macro:with-buffer :messages
;;                              (message "hello there")))


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
;; Messages
;;------------------------------------------------------------------------------

(defun int<autogit>:output:indent (indent level)
  "Get indent string for indent LEVEL from INDENT.

LEVEL must be a wholenum - 0 is 'not indented'.

INDENT can be:
  - wholenum: a string of spaces of length (INDENT * LEVEL).
  - string:   returns the string repeated LEVEL times.
  - list:     returns the string at element (LEVEL - 1) or \"\" if LEVEL is 0."
  (cond ((not (wholenump level))
         (error "int<autogit>:output:indent: LEVEL must be a wholenum, got: %S"
                level))

        ((listp indent)
         (if (= level 0)
             ""
           ;; Pretend level 1 indent so no enlarging of the list's indent.
           (int<autogit>:output:indent (seq-elt indent (1- level)) 1)))

        ((wholenump indent)
         (make-string (* indent level) ?\s))

        ((stringp indent)
         (let (indents)
           (dotimes (_ level)
             (setq indents (cons indent indents)))
           (apply 'concat indents)))

        (t
         (error "int<autogit>:output:indent: Unknown INDENT type: %S %S"
                (type-of indent) indent))))
;; (int<autogit>:output:indent 4 1)
;; (int<autogit>:output:indent 4 2)
;; (int<autogit>:output:indent "  " 1)
;; (int<autogit>:output:indent "  " 2)
;; (int<autogit>:output:indent '("hello" "there") 1)
;; (int<autogit>:output:indent '("hello" "there") 2)


(defun int<autogit>:output:propertize (&rest args)
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
            (error (concat "int<autogit>:output:propertize: "
                           "Must have text to propertize. Found nothing "
                           "for key `:text' in args: %S")
                   args))

        ;; Have args.
        (if (not fmt/str)
            ;; Args but no string... Don't know what to do with this.
            (error (concat "int<autogit>:output:propertize: "
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
             (error (concat "int<autogit>:output:propertize: "
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
;; (int<autogit>:output:propertize :text "hello there" :prop :autogit)
;; (int<autogit>:output:message :messages (int<autogit>:output:propertize "hello there" :autogit))
;; (propertize "hello there" 'face 'package-name)


(defun int<autogit>:output:finalize (args)
  "Create a (propertized) string ready to be output to a buffer.

ARGS should be a list and each item should be one of:
  - A string.
  - A plist for propertizing a string.
    + With requried key `:text', value of a string or list of
      format-string and format-args.
    + With optional key `:prop' or `:property', value of a keyword from
      `int<autogit>:output:propertize'.
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
             (push (apply #'int<autogit>:output:propertize arg) text/list))

            ;; Regular list
            ((listp arg)
             (push (apply #'format arg) text/list))

            ;; Unknown - Error
            (t
             (error "`int<autogit>:output:finalize': Cannot process input: %S"
                    arg))))

    (when text/list
      ;; Combine the list and we're done.
      (apply #'concat (nreverse text/list)))))


(defun int<autogit>:output:insert (buffer message)
  "Insert finalized MESSAGE into BUFFER.

BUFFER should be a keyword, string or buffer object.
  - If it is the keyword `:messages', output to the *Messages* buffer using
    `message'.
  - Else inserts into BUFFER using `int<autogit>:macro:with-buffer'.

MESSAGE should be output from `int<autogit>:output:finalize'."
  (cond ((and (keywordp buffer)
              (eq buffer :messages))
         ;; Using *Messages* buffer, so just use the `message' function to put the
         ;; message there.
         (message message))

        ;; Some other keyword: error.
        ((keywordp buffer)
         (error "`int<autogit>:output:insert': unknown buffer keyword: %S" buffer))

        ;; Buffer obj or str:
        ((or (stringp buffer)
             (bufferp buffer))
         (int<autogit>:macro:with-buffer buffer
                                         ;; We are now in BUFFER, so just insert the formatted string on a new line at the end.
                                         (goto-char (point-max))
                                         (insert (concat "\n" message))))

        ;; Fallthrough error.
        (t
         (error "`int<autogit>:output:insert': unhandled buffer type %S %S"
                (type-of buffer) buffer))))


(defun int<autogit>:output:message (buffer &rest args)
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
      `int<autogit>:output:propertize'.
  - A list of: a format-string and format-args.

No padding between args is created.

NOTE: If BUFFER is not `:message', it will print to the current buffer,
so it must be used inside the `int<autogit>:macro:with-buffer' macro body!"
  (int<autogit>:output:insert buffer (int<autogit>:output:finalize args)))
;; (let ((buffer autogit:buffer:name/status))
;;     (int<autogit>:output:message buffer
;;                         (list :prop :face:self :text autogit:text:name)
;;                         ": "
;;                         "Status of "
;;                         (list :prop :face:highlight
;;                               :text (list "%d" (length autogit:repos:path/watch)))
;;                         " watch locations...")
;;     (int<autogit>:buffer:display buffer)))


(defun int<autogit>:output:message/indented (buffer indent &rest args)
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
      `int<autogit>:output:propertize'.
  - A list of: a format-string and format-args.

No padding between args is created.

NOTE: If BUFFER is not `:message', it will print to the current buffer,
so it must be used inside the `int<autogit>:macro:with-buffer' macro body!"
  (let ((msg (int<autogit>:output:finalize args))
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

    (int<autogit>:output:insert buffer msg)))


;; TODO: namespace for 'message', 'section', 'buffer' type things.
;;   - 'output'?
;;   - 'display'?
(defun int<autogit>:output:newline (buffer)
  "Insert a newline into autogit output BUFFER."
  (int<autogit>:output:message buffer ""))


(defun int<autogit>:output:section-break/display (buffer)
  "Insert a section break into BUFFER.

BUFFER should be a keyword, string or buffer object.
  - If it is the keyword `:messages', output to the *Messages* buffer using
    `message'.
  - Else inserts into BUFFER using `int<autogit>:macro:with-buffer'."
  ;; TODO: Make these from defcustoms. num newlines, padding cons, padding char, width-or-use-fill-column.
  (let* ((newlines (make-string 2 ?\n))
         (padding '("┌" . "┐")))
    (int<autogit>:output:message buffer
                                 (list :prop :face:section
                                       :text (concat newlines
                                                     (car padding)
                                                     (make-string (- fill-column
                                                                     (length (car padding))
                                                                     (length (cdr padding)))
                                                                  (string-to-char "─"))
                                                     (cdr padding))))))


(defun int<autogit>:output:section-break/auto (buffer)
  "Inserts a section break into BUFFER if needed.

BUFFER should be a keyword, string or buffer object.
  - If it is the keyword `:messages', output to the *Messages* buffer using
    `message'.
  - Else inserts into BUFFER using `int<autogit>:macro:with-buffer'."
  (when (> (point) 1)
    (int<autogit>:output:section-break/display buffer)))


;;------------------------------------------------------------------------------
;; Output/Repo: Status
;;------------------------------------------------------------------------------

(defun int<autogit>:output:status (buffer alist/changes)
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
      (int<autogit>:output:message buffer
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
            (int<autogit>:output:message buffer
                                         (list "%s" indent)
                                         (list :prop :face:title :text (list "%s" (car entry)))
                                         ": "
                                         (list :prop :face:highlight :text "None."))
          ;; Show type (staged, unstaged...) and list of change.
          (int<autogit>:output:message buffer
                                       (list "%s" indent)
                                       (list :prop :face:title :text (list "%s" (car entry)))
                                       ":")
          (dolist (path (cdr entry))
            (int<autogit>:output:message buffer
                                         (list "%s  - " indent)
                                         (list :prop :face:path :text path))))))))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :autogit 'internal)

;; -*- mode: emacs-lisp; lexical-binding: t -*-

;;-------------------------------------spy--------------------------------------
;;--                             Path Functions                               --
;;---------------------------------/mnt/hello-----------------------------------

(imp:require :str)

;; TODO: defaliases for emacs's path functions?
;;   - file-name-as-directory == path->dir ?
;;     - and some inverse: path->file ?

;; TODO: truenames?
;;   - E.g. Funcs to follow symlinks.
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Truenames.html
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/File-Name-Expansion.html
;; And I guess TODO symlinks in general?

;; TODO: `ignore-case' non-nil on windows et al?


;;------------------------------------------------------------------------------
;; Constants
;;------------------------------------------------------------------------------

(defconst path:types
  ;; Directory
  '(:dir

    ;; File
    :file

    ;; Symbolic Link
    :symlink)
  "Types of files that Emacs can reason about.

https://www.gnu.org/software/emacs/manual/html_node/elisp/Kinds-of-Files.html")


(defconst path:rx:names:ignore
  (list
   ;; Ignore "." and ".." entries.
   (rx-to-string '(sequence
                   string-start
                   (repeat 1 2 ".")
                   string-end)
                 :no-group))
  "What to ignore when traversing paths, getting children, etc..

NOTE: These should be compiled regex strings.")


(defconst path:rx:dirs:not-parent-or-current-dot
  directory-files-no-dot-files-regexp
  "Regexp matching any (non-empty) file name except \".\" and \"..\".

Useful for `path:children' and `path:children:types'.")


;;------------------------------------------------------------------------------
;; Predicates
;;------------------------------------------------------------------------------

(defun int<path>:type:valid? (caller type &optional nil-invalid?)
  "Error if TYPE is invalid. Return TYPE if valid (can be nil).

CALLER should be the name of the calling function, for use in the error messages."
  ;;---
  ;; Error/OK: nil may or may not be valid...
  ;;---
  (cond ((null type)
         ;; Should nil be allowed as a type?
         (if nil-invalid?
             (error "%s: Invalid TYPE `%S'. TYPE must be one of: %S"
                caller
                type
                path:types)

           ;; nil is allowed.
           type))

         ;;---
         ;; Error: Not a keyword?
         ;;---
         ((not (keywordp type))
          (error "%s: Invalid TYPE `%S'. TYPE must be a keyword! Valid TYPEs: %S"
                caller
                type
                path:types))

         ;;---
         ;; Error: Not a valid keyword.
         ;;---
         ((not (memq type path:types))
          (error "%s: Unknown TYPE `%S'. TYPE must be one of: %S"
                 caller
                 type
                 path:types))

         ;;---
         ;; OK: Anything else? Think we've checked everything.
         ;;---
         (t
          type)))


(defun path:directory? (path)
  "Return non-nil if PATH is a directory name path."
  (directory-name-p path))


(defun path:file? (path)
  "Return non-nil if PATH is a file name path."
  (not (path:directory? path)))


(defun path:absolute? (path)
  "Return non-nil if PATH is an absolute path."
  (file-name-absolute-p path))


(defun path:exists? (path &optional type)
  "Return non-nil if PATH exists.

TYPE can be: :dir, :file, :symlink, or nil.
If TYPE is provided, PATH must exist \and\ be the correct type."
  ;;------------------------------
  ;; Error Cases
  ;;------------------------------
  ;; Errors on invaild type.
  (int<path>:type:valid? "path:exists?" type)

  ;; Sanity check path...
  (cond ((not (stringp path))
         (error "path:exists?: PATH must be a string: %S"
                path))

        ;;------------------------------
        ;; Check Existance
        ;;------------------------------
        ((null type)
         (file-exists-p path))

        ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Kinds-of-Files.html
        ((eq :dir type)
         (file-directory-p path))

        ((eq :file type)
         (file-regular-p path))

        ((eq :symlink type)
         (file-symlink-p path))

        ;; Why & how are you here?
        (t
         (error "path:exists?: Unhandled TYPE `%S'!!!"
                type))))
;; (path:exists? (path:current:file))
;; (path:exists? (path:current:file) :file)
;; (path:exists? (path:current:file) :dir)
;; (path:exists? (path:current:dir) :dir)
;; (path:exists? (path:current:dir))


(defun path:readable? (path)
  "Return non-nil if PATH exists and is readable."
  (file-readable-p path))


(defun path:descendant? (descendant parent)
  "Return non-nil if DESCENDANT path is a descendant of PARENT path.

DESCENDANT and PARENT cannot be the same path.

Will convert the paths to absolute/canonical values before comparing."
  ;; Convert both to dir or file paths so that a dir and a file path work correctly.
  ;; e.g.:
  ;;   (path:descendant? "/foo/" "/foo")
  ;;     -> nil
  (let ((descendant/abs (path:canonicalize:dir descendant))
        (parent/abs (path:canonicalize:dir parent)))
    (unless (string= descendant/abs parent/abs)
      (string-prefix-p parent/abs
                       descendant/abs))))


(defun path:child? (child parent)
  "Return non-nil if CHILD path is a direct child of PARENT path.

CHILD and PARENT cannot be the same path.

Will convert the paths to absolute/canonical values before comparing."
  ;; Convert both to file paths so that a dir and a file path work correctly.
  ;; (File paths because that's what `path:parent' returns).
  ;; e.g.:
  ;;   (path:child? "/foo/" "/foo")
  ;;     -> nil
  (let ((child/abs (path:canonicalize:file child))
        (parent/abs (path:canonicalize:file parent)))
    (message (mapconcat #'identity
                        '("child:          %s"
                          "parent:         %s"
                          "child's parent: %s")
                        "\n")
             child/abs
             parent/abs
             (path:parent child/abs))
    (unless (string= child/abs parent/abs)
      (string= (path:parent child/abs)
               parent/abs))))
;; (path:child? "/" "/")
;; (path:child? "/foo" "/")
;; (path:child? "/foo/bar" "/foo")


(defun path:equal? (a b)
  "Return non-nil if paths A and B are the same.

Will convert the paths to absolute/canonical values before comparing."
  ;; Convert both to dir or file paths so that comparing a dir and a file path work correctly.
  (string= (path:canonicalize:file a)
           (path:canonicalize:file b)))



;;------------------------------------------------------------------------------
;; Traversal
;;------------------------------------------------------------------------------

(defun path:parent (path)
  "Return the parent directory of PATH."
  (directory-file-name (file-name-directory path)))
;; (path:parent "relative/path/to/foo.test")


;; TODO: Move to regex.el?
(defun path:ignore? (path regexes)
  "Return non-nil if PATH matches any of the REGEXES."
  (let ((func/name "path:ignore?")
        (regex:list regexes) ;; Shallow copy so we can pop without possibly changing caller's list.
        ignore?)
    (unless (stringp path)
      (error "%s: PATH must be a string! Got: %S"
             func/name
             path))
    (unless (listp regexes)
      (error "%s: REGEXES must be a list of regex strings! Got: %S"
             func/name
             regexes))

    (while (and (not ignore?) ;; Found a reason to ignore already?
                regex:list)   ;; Finished the regexes?
      (let ((regex (pop regex:list)))
        (unless (stringp regex)
          (error "%s: Regex must be a string! Got: %S"
                 func/name
                 regex))

        (when (string-match regex path)
          ;; Set our return & stop-looping-early value.
          (setq ignore? t))))

    ignore?))
;; (path:ignore? "x.y" path:rx:names:ignore)
;; (path:ignore? "." path:rx:names:ignore)
;; (path:ignore? ".." path:rx:names:ignore)
;; (path:ignore? "..." path:rx:names:ignore)


(defun path:children:types (path:dir &optional absolute-paths? regex &rest types)
  "Return immediate children of PATH:DIR directory.

Return an alist of children by type:
  '((:file . (\"child.ext\" ...))
    ...)

If ABSOLUTE-PATHS? is non-nil, return absolute paths to the children.
Else return names of children.

If REGEX is non-nil, return only children whose filename matches the REGEX.

TYPES should be nil or a list of keywords from `path:types'.
If TYPES is non-nil, return only children of those types."
  (let ((types (if types
                   ;; Remove nils & flatten '(nil) to just nil.
                   (seq-filter (lambda (t) (not (null t)))
                               types)
                 ;; nil is any/all types.
                 nil)))
    ;;------------------------------
    ;; Error Checking
    ;;------------------------------
    ;; Errors on invaild type.
    (dolist (type types)
      (int<path>:type:valid? "path:children:types" type))

    (let ((func/name "path:children:types"))
      (unless (stringp path:dir)
        (error "%s: PATH:DIR must be a string! Got: %S"
               func/name
               path:dir))

      (let ((path:root     (path:canonicalize:dir path:dir))
            (save:dirs     (if types (memq :dir     types) t))
            (save:files    (if types (memq :file    types) t))
            (save:symlinks (if types (memq :symlink types) t))
            children)
        (unless (path:exists? path:root :dir)
          (error "%s: PATH:DIR does not exist: %s"
                 func/name
                 path:root))

        ;;------------------------------
        ;; Find Children
        ;;------------------------------
        ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Contents-of-Directories.html
        ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/File-Attributes.html#Definition-of-file_002dattributes
        (dolist (child (directory-files-and-attributes path:root nil regex))
          (let* ((child:name  (car child))
                 (child:path  (path:join path:root child:name))
                 (child:attrs (cdr child))
                 type)

            ;;------------------------------
            ;; Ignore / Determine Type
            ;;------------------------------
            ;; Explicitly ignore?
            (cond ((path:ignore? child:name path:rx:names:ignore)
                   (setq type :ignore))

                  ;;---
                  ;; Save / Ignore by Type
                  ;;---
                  ((eq (file-attribute-type child:attrs) t) ;; t is the attr type for directories.
                   (setq type (if save:dirs :dir :ignore)))

                  ((eq (file-attribute-type child:attrs) nil) ;; nil is the attr type for files.
                   (setq type (if save:files :file :ignore)))

                  ((stringp (file-attribute-type child:attrs)) ;; The attr type for symlinks is a string of the path they point to.(
                   (setq type (if save:symlinks :symlink :ignore)))

                  ;;---
                  ;; Error: How did you get here?
                  ;;---
                  (t
                   (error "%s: '%s': Unhandled file-attribute-type: %S"
                          func/name
                          child:path
                          (file-attribute-type child:attrs))))

            ;;------------------------------
            ;; Save Child?
            ;;------------------------------
            (when (and type
                       (not (eq type :ignore)))
              (let ((children:type (alist-get type children)))
                ;; Update type's list of children with this child's path.
                (push (if absolute-paths? child:path child:name) children:type)
                (setf (alist-get type children) children:type)))))

        ;;------------------------------
        ;; Return list we've built.
        ;;------------------------------
        children))))
;; (path:children:types user-emacs-directory)
;; (path:children:types user-emacs-directory nil)
;; (path:children:types user-emacs-directory t)
;; (path:children:types user-emacs-directory nil :file)
;; (path:children:types user-emacs-directory nil :dir)
;; (path:children:types (path:current:dir) nil nil)


;; TODO: Follow symlinks or no?
(defun path:children (path:dir &optional absolute-paths? type regex)
  "Return immediate children of PATH:DIR directory.

If ABSOLUTE-PATHS? is non-nil, return a list of absolute paths to the children.
Else return a list of names of children.

If TYPE is supplied, return only children of that type.

If REGEX is non-nil, return only children whose filename matches the REGEX."
  ;; Get by TYPE, then flatten to a single list of all children.
  (let ((by-types (path:children:types path:dir absolute-paths? regex type))
        children)
    (dolist (type:assoc by-types)
      (setq children (if children
                         ;; Append to children.
                         (cons children (cdr type:assoc))
                       ;; Create children.
                       (cdr type:assoc))))
    children))
;; (path:children (path:current:dir))
;; (path:children (path:current:dir) t)
;; (path:children (path:current:dir) nil :file)
;; (path:children (path:current:dir) nil :dir)


(defun int<path>:walk (root dir callback)
  "Helper for walking a directory tree.

Gets children from ROOT subdirectory DIR, calls CALLBACk for each child.

CALLBACK should accept params:
  1) string - root (absolute)
  2) string - directory (relative path from root)
  3) string - child (a file/dir/etc in directory)

CALLBACK should be a predicate for \"Continue walking?\"; it should return
non-nil to continue and nil to halt the walk."
  (let ((children:type (path:children:types (path:canonicalize:dir root dir)))
        (continue t)
        (child:dirs nil)) ;; Walk down into these dirs.

    ;;------------------------------
    ;; Deal with children based on how likely to be a directory.
    ;;------------------------------
    (when-let ((continue continue) ;; Already done?
               (files (alist-get :file children:type)))
      (while (and continue
                  files)
        (when-let ((child (pop files)))
          (setq continue (funcall callback
                                  root
                                  dir
                                  child)))))

    (when-let ((continue continue) ;; Already done?
               (symlinks (alist-get :symlink children:type)))
      (while (and continue
                  symlinks)
        (when-let ((child (pop symlinks)))
          ;; TODO: Push dir symlinks (not file symlinks) into `child:dirs'?
          (setq continue (funcall callback
                                  root
                                  dir
                                  child)))))

    (when-let ((continue continue) ;; Already done?
               (dirs (alist-get :dir children:type)))
      (while (and continue
                  dirs)
        (when-let ((child (pop dirs)))
          (push child child:dirs)
          (setq continue (funcall callback
                                  root
                                  dir
                                  child)))))

    ;;------------------------------
    ;; Return all our children dirs to walk.
    ;;------------------------------
    (cons continue
          (if child:dirs
              (cons dir child:dirs)
            nil))))
;; (int<path>:walk (path:current:dir) "." (lambda (x) (message "hi %S" x)))
;; (int<path>:walk (path:current:dir) nil (lambda (x) (message "hi %S" x)))
;; (int<path>:walk (path:current:dir) ".." (lambda (x) (message "hi %S" x)) t)


(defun path:walk (root callback)
  "Walk the directory tree starting at ROOT, call CALLBACK for each child.

CALLBACK should accept params:
  1) string - path of child, absolute
  2) string - path of child, relative to ROOT

CALLBACK should be a predicate for \"Continue walking?\"; it should return
non-nil to continue and nil to halt the walk."
  ;;------------------------------
  ;; Validate input.
  ;;------------------------------
  (cond ((null callback)
         (error "int<path>:walk: Must have a CALLBACK function to walk directory tree! Got: %S"
                callback))
        ((not (functionp callback))
         (error "int<path>:walk: CALLBACK must be a `functionp' to walk directory tree! Got: %S --functionp?--> %S"
                callback
                (functionp callback)))
        (t
         ;; ok
         nil))

  ;;------------------------------
  ;; Walk dirs.
  ;;------------------------------
  ;; Start our walk.
  (let* ((path-root (path:canonicalize:dir root))
         ;; walked is (continue . (dir . children))
         (walked   (int<path>:walk root
                                   nil
                                   callback))
         (continue (car walked))
         (next     (list (cdr walked))))

    ;; Walk while we still have entries in `next'.
    (while (and continue
                next)
      (let* ((current  (pop next))
             (dir      (car current))
             (children (cdr current)))
        ;; Walk a child directory.
        (while (and continue
                    children)
          ;; Walk a child directory.
          (setq walked (int<path>:walk root
                                       (path:join dir (pop children))
                                       callback)
                continue (car walked))

          ;; _Append_ to next; don't set/overwrite it.
          (when (cddr walked) ;; Only append if we have more children directories to walk.
            ;; Don't push `continue' flag.
            (push (cdr walked) next)))))))
;; (path:walk (path:current:dir) (lambda (x) (message "hi %S" x)))
;; (path:walk (path:parent (path:current:dir)) (lambda (x) (message "hi %S" x)) t)


;;------------------------------------------------------------------------------
;; Join
;;------------------------------------------------------------------------------

;; TODO: `path:validate' function?

(defun int<path>:append (parent next)
  "Append NEXT element to PARENT, adding dir separator between them.

PARENT & NEXT are normalized via `str:normalize:name', so keywords or symbol
names can be used as well as strings."
  (cond ((and (null parent)
              (null next))
         ;; Change this to just return nil so `path:join' can be a little more robust?
         nil
         ;; (error "int<path>:append: Cannot append nulls! parent: %S, next: %S"
         ;;        parent
         ;;        next)
         )
        ((null next)
         (str:normalize:name parent))
        ((null parent)
         (str:normalize:name next))
        (t
         (concat (file-name-as-directory (str:normalize:name parent))
                 (str:normalize:name next)))))
;; (int<path>:append nil "jeff")
;; (str:normalize:name->list "jill")
;; (int<path>:append "jeff" "jill")
;; (int<path>:append "jeff/" "jill")
;; (int<path>:append 'jeff :jill)
;; (int<path>:append 'jeff nil)
;; (int<path>:append nil nil)


(defun path:join (&rest path)
  "Combines PATH elements together into a path platform-agnostically.

(path:join \"jeff\" \"jill.el\")
  ->\"jeff/jill.el\""
  (seq-reduce #'int<path>:append
              (apply #'str:normalize:name->list path)
              nil))
;; (path:join "jeff" "jill")
;; (path:join "jeff" "jill/")
;; (path:join "jeff" nil)
;; (path:join "" nil)
;; (path:join nil)


;;------------------------------------------------------------------------------
;; Split
;;------------------------------------------------------------------------------

(defvar int<path>:separators:rx (rx-to-string '(one-or-more (or ?/ ?\\)) ;; '(or ?/ ?\\)
                                              :no-group)
  "Separators for Windows and Linux paths.")


(defun int<path>:filter:strings:keep (item)
  "`-keep' needs the ITEM itself returned, so you can't just use `stringp'."
  (and (stringp item) item))


;;------------------------------
;; Path Segments
;;------------------------------

(defun path:segments (&rest path)
  "Split all PATH strings by directory separators, return a plist.

Returned PLIST will have these keys (if their values are non-nil).
  :drive   - Drive letter / name
           - \"C:/foo/bar\" -> \"C:\"
  :root    - Root of the path (\"/\", \"C:\\\", etc.)
           - \"C:/foo/bar\" -> \"/\"
           - \"/foo/bar\" -> \"/\"
  :parents - Parent directory ancestors of PATH.
           - \"/foo/bar/baz\" -> '(\"foo\" \"bar\")
  :name    - Name of the final path element.
           - \"/foo/bar/baz\" -> \"baz\"
           - \"/foo/bar/\" -> \"bar\"
           - \"/foo/bar.tar.gz\" -> \"bar.tar.gz\""
  (let (drive
        root
        ;; `segments' will get split into `parents' and `name'.
        segments
        parents
        name
        output
        ;; Get rid of any nulls or invalid segments.
        (paths (-keep #'int<path>:filter:strings:keep path)))
    (if (null paths)
        (error "path:segments: PATH has no strings to split: %S"
               path)

      ;;------------------------------
      ;; Parse input.
      ;;------------------------------
      ;; Path is absolute if first/only segment is absolute.
      (when (path:absolute? (car paths))

        ;; If absolute, set the root directory.
        (setq root "/")

        ;; If on windows, set the root drive.
        (when (eq system-type 'windows-nt)
          (save-match-data
            (when (string-match (rx-to-string '(and string-start letter ":")
                                              :no-group)
                                (car paths))
              (setq drive (match-string 0 (car paths)))))))

      ;; Split each input into segments.
      (dolist (path paths)
        (dolist (segment (split-string path
                                       int<path>:separators:rx
                                       t
                                       split-string-default-separators))
          (push segment segments)))

      ;; `segments' is backwards, so first item is file/dir `name',
      ;; rest need to be reversed into the `parents'.
      (setq name    (car segments)
            parents (nreverse (cdr segments)))

      ;;------------------------------
      ;; Build output plist (in reverse).
      ;;------------------------------
      (when name
        (push name output)
        (push :name output))

      (when parents
        (push parents output)
        (push :parents output))

      (when root
        (push root output)
        (push :root output))

      (when drive
        (push drive output)
        (push :drive output))

      ;; Return segments plist.
      output)))
;; (path:segments "/foo/bar" "/baz")


;;------------------------------
;; Split on Dir Separators
;;------------------------------

(defun path:split (&rest segment)
  "Split all SEGMENT strings by directory separators, return one list."
  ;; `path:segments' will do the heavy lifting.
  (let ((segments (apply #'path:segments segment)))
    ;; Now just convert the plist into a list, and drop any nulls.
    (-keep #'int<path>:filter:strings:keep
           (-flatten (list
                      (plist-get segments :drive)
                      (plist-get segments :root)
                      (plist-get segments :parents)
                      (plist-get segments :name))))))
;; (path:split "/foo/bar" "/baz")


;;------------------------------------------------------------------------------
;; Names
;;------------------------------------------------------------------------------

(defun path:name:base (path &rest segment)
  "Join PATH & any SEGMENTs, then remove any file extensions/suffixes.

Example:
  (path:name:base \"/foo\" \"bar.baz\")
    -> \"/foo/bar\""
  (file-name-sans-extension (path:join path segment)))


(defun path:name:dir (path &rest segment)
  "Join PATH & any SEGMENTs, then ensure it is a directory path.

Example:
  (path:name:dir \"/foo\" \"bar.baz\")
    -> \"/foo/bar.baz/\""
  (file-name-sans-extension (path:join path segment)))



;;------------------------------------------------------------------------------
;; Normalize / Canonicalize
;;------------------------------------------------------------------------------

(defun path:canonicalize:file (path &rest segment)
  "Canonicalize/normalize a file PATH and path SEGMENTS.

Return an absolute path.

Does not fix or validate PATH or SEGMENT components; they are expected to be
valid."
  ;; Force the return value to be a file path by ensuring trailing "/" and then
  ;; removing it.
  (directory-file-name
   (file-name-as-directory
    ;; Get a normalized path.
    (expand-file-name (apply #'path:join
                             path
                             segment)))))
;; (path:canonicalize:file "~" "personal" "something.exe" "zort.txt")
;; (path:canonicalize:file "~" "personal" "something.exe" ".")
;; (path:canonicalize:file "~" "personal" "something.exe" "..")


(defalias 'path:absolute:file 'path:canonicalize:file)
(defalias 'path:abs:file      'path:canonicalize:file)


(defun path:canonicalize:dir (path &rest segment)
  "Canonicalize/normalize a directory PATH and path SEGMENTS.

Return an absolute path.

Does not fix or validate PATH or SEGMENT components; they are expected to be
valid."
  ;; Fully qualify base as start of return value.
  (file-name-as-directory (apply #'path:canonicalize:file path segment)))
;; (path:canonicalize:dir "~" "personal" "something" "zort")


(defalias 'path:absolute:dir 'path:canonicalize:dir)
(defalias 'path:abs:dir      'path:canonicalize:dir)


(defun path:canonicalize:absolute (path &rest segment)
  "Canonicalize/normalize a file PATH and path SEGMENTS.

Will attempt to preserve file/directory-ness of PATH - that is, try to preserve
the final slash if it exists.

Return an absolute path.

Does not fix or validate PATH or SEGMENT components; they are expected to be
valid."
  (let ((path/joined (apply #'path:join path segment)))
    (funcall (if (path:directory? path/joined)
                 #'path:canonicalize:dir
               #'path:canonicalize:file)
             path/joined)))
;; (path:canonicalize:absolute "/foo" "bar")
;; (path:canonicalize:absolute "/foo" "bar/")


(defalias 'path:canonicalize       'path:canonicalize:absolute)
(defalias 'path:absolute           'path:canonicalize:absolute)
(defalias 'path:abs                'path:canonicalize:absolute)


(defun path:abbreviate:file (path &rest segment)
  "Create file path from PATH & SEGMENTs, shortened using `directory-abbrev-alist'.

Return an absolute path.

Does not fix or validate PATH or SEGMENT components; they are expected to be
valid."
  (abbreviate-file-name
   (apply #'path:canonicalize:file path segment)))


(defalias 'path:abbrev:file        'path:abbreviate:file)


(defun path:abbreviate:dir (path &rest segment)
  "Create dir path from PATH & SEGMENTs, shortened using `directory-abbrev-alist'.

Return an absolute path.

Does not fix or validate PATH or SEGMENT components; they are expected to be
valid."
  (abbreviate-file-name
   (apply #'path:canonicalize:dir path segment)))


(defalias 'path:abbrev:dir         'path:abbreviate:dir)


(defun path:abbreviate:absolute (path &rest segment)
  "Create path from PATH & SEGMENTs, shortened using `directory-abbrev-alist'.

Will attempt to preserve file/directory-ness of PATH - that is, try to preserve
the final slash if it exists.

Return an absolute path.

Does not fix or validate PATH or SEGMENT components; they are expected to be
valid."
  (let ((path/joined (apply #'path:join path segment)))
    (funcall (if (path:directory? path/joined)
                 #'path:abbreviate:dir
               #'path:abbreviate:file)
             path/joined)))
;; (path:abbreviate:absolute "/foo" "bar")
;; (path:abbreviate:absolute "/foo" "bar/")


(defalias 'path:abbreviate         'path:abbreviate:absolute)
(defalias 'path:abbrev             'path:abbreviate:absolute)


;;------------------------------------------------------------------------------
;; Relative Paths
;;------------------------------------------------------------------------------

(defun path:canonicalize:relative (path root)
  "Return a file path to PATH relative to ROOT.

Could just return PATH if it has no relation to ROOT.

Raises an error if PATH is not a string.
Raises an error if ROOT is not nil and not a string."
  (unless (stringp path)
    (error "path:canonicalize:relative: PATH must be a string! Got: path: %S, root: %S"
           path root))
  (unless (or (null root)
              (stringp root))
    (error "path:canonicalize:relative: ROOT must be nil or a string! Got: path: %S, root: %S"
           path root))

  ;; Translate nil ROOT to empty string if needed.
  ;; And canonicalize our paths.
  (let ((root (or (path:canonicalize:dir root) ""))
        (path (path:canonicalize:file path)))
    ;; Don't like `file-relative-name' as it can return wierd things when it
    ;; goes off looking for actual directories and files...
    (replace-regexp-in-string
     root ;; Look for ROOT directory path...
     ""   ;; Replace with nothing to get a relative path.
     path ;; Ensure
     :fixedcase
     :literal)))
;; (path:canonicalize:relative "/path/to/a/file/location.txt" "/path/to/a/")
;; (path:canonicalize:relative "/path/to/a/dir/location/" "/path/to/a/")
;; (path:canonicalize:relative "/path/to/a/dir/location/" "/path/to/a")


(defalias 'path:relative           'path:canonicalize:relative)
(defalias 'path:rel                'path:canonicalize:relative)


;;------------------------------------------------------------------------------
;; Current Paths
;;------------------------------------------------------------------------------

(defun path:current:file ()
  "Return the Emacs Lisp file this function is called from.

Works when:
  - byte compiling
    - `byte-compile-current-file'
  - loading
    - `load-file-name'
    - `current-load-list'
  - visiting/evaluating
    - function `buffer-file-name'

Raises an error signal if it cannot find a file path."
  (cond
   ;;------------------------------
   ;; Look for a valid "current file" variable.
   ;;------------------------------
   ((bound-and-true-p byte-compile-current-file))

   (load-file-name)

   ((stringp (car-safe current-load-list))
    (car current-load-list))

   (buffer-file-name)

   ;;------------------------------
   ;; Error: Didn't find anything valid.
   ;;------------------------------
   ((error "path:current:file: Cannot get the current file's path"))))
;; (path:current:file)


(defun path:current:dir ()
  "Return the directory of the Emacs Lisp file this function is called from.

Uses `path:current:file' and just chops off the filename."
  (when-let (path (path:current:file))
    (path:parent path)))
;; (path:current:dir)


;;------------------------------------------------------------------------------
;; Translations Between OSes
;;------------------------------------------------------------------------------

;; There are some existing packages for dealing with windows->unix or unix->windows paths...
;;   Windows emacs, Cygwin paths: https://www.emacswiki.org/emacs/cygwin-mount.el
;;   Cygwin/WSL emacs, win paths: https://github.com/victorhge/windows-path
;; but..: aren't on melpa, haven't been updated in years, etc.
(defun path:translate (from to dir)
  "Translates a path style, e.g. from Windows to WSL.

FROM and TO should be one of: (:windows :wsl :linux)
DIR should be a string.

For `:windows' -> `:wsl':
  - Translates '<drive>:' to '/mnt/<drive>'.
  - Translates '\\' to '/'.
"
  (let ((trans dir))
    ;; Translate: :windows -> :wsl
    (cond ((and (eq from :windows)
                (eq to   :wsl))
           (let ((drive nil)
                 (path nil))
             ;; Regex to:
             ;;   1) Find <drive> letter.
             ;;   2) Ignore ':'.
             ;;   3) Find <path>.
             (if (not (string-match (rx string-start
                                        ;; Get drive letter as a capture group.
                                        (group (= 1 (any "a-z" "A-Z")))
                                        ":"
                                        (group (zero-or-more anything))
                                        string-end)
                                    dir))
                 ;; No match - no translation.
                 (setq trans "")
               (setq drive (downcase (match-string 1 dir))
                     path (match-string 2 dir))
               (setq trans (concat "/mnt/"
                                   drive
                                   (replace-regexp-in-string (rx "\\")
                                                             "/"
                                                             path))))))

          ;; Translate: :windows -> :wsl
          ((and (eq from :wsl)
                (eq to   :windows))
           ;; Replace '/mnt/<drive>' with '<drive>:'.
           (let ((drive nil)
                 (path nil))
             ;; Regex to:
             ;;   1) Trim off leading "/mnt/".
             ;;   2) Find <drive> letter.
             ;;   3) Find <path>.
             (if (not (string-match (rx string-start
                                        "/mnt/"
                                        ;; Get drive letter as a capture group.
                                        (group (= 1 (any "a-z" "A-Z")))
                                        (group (or (and (not (any "a-z" "A-Z"))
                                                        (zero-or-more anything))
                                                   string-end)))
                                    dir))
                 ;; No match - no translation.
                 (setq trans "")

               ;; Get drive and path from regex matching, then build
               ;; translation.
               (setq drive (upcase (match-string 1 dir))
                     path (match-string 2 dir)
                     trans (concat drive
                                   ":"
                                   (if (and (not (null path))
                                            (not (string= "" path)))
                                       path
                                     "/"))))))

          ;; Fallthrough -> error out.
          (t
           (error "path:translate currently does not support %s -> %s: %s"
                  from to path)))

    ;; Return the translation.
    trans))
;; (path:translate :windows :wsl "D:/path/to/somewhere.txt")
;; (path:translate :windows :wsl "D:/path/to/somewhere.txt")
;; (path:translate :wsl :windows "/mnt/d/path/to/somewhere.txt")
;; Should not be able to translate so should return "".
;; (path:translate :windows :wsl "~/path/to/somewhere.txt")


(defun int<path>:type (path)
  "Try to guess a PATH type.

Return:
   :wsl     - Linux path to a windows drive?
   :windows - Windows path?
   :linix   - Linux path?"
  ;; Start guessing...
  ;; If it has a backslash, it's probably windows.
  (cond ((str:contains? "\\" path)
         :windows)

        ;; Does it start with a drive-letter and colon?
        ;; Probably windows.
        ((string-match (rx string-start
                           letter
                           ":")
                       path)
         :windows)

        ((string-match (rx string-start
                           "/mnt/"
                           letter
                           "/")
                       path)
         :wsl)

        (t
         :linux)))


;; TODO: "desired type" param?
(defun path:cmd:translate (path)
  "Try to auto-guess PATH type and then translate the path."
  (interactive "sPath: ")
  (let* ((source (int<path>:type path))
         (dest (if (eq source :windows)
                   ;; WSL should work for translating to Linux too?
                   :wsl
                 :windows))
         (translated (path:translate source
                            dest
                            path)))
    ;; Copy to kill-ring...
    (kill-new translated)
    ;; Return it.
    translated))
;; (path:cmd:translate "D:/")


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :path 'path)

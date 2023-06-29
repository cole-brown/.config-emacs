;;; autogit-path.el --- Path Functions -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2020-08-28
;; Timestamp:  2023-06-29
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Path Functions
;;
;;; Code:


(require 'seq)
(require 'magit)


;;------------------------------------------------------------------------------
;; Helpers
;;------------------------------------------------------------------------------

(defconst int<autogit>:path:rx:names:ignore
  (list
   ;; Ignore "." and ".." entries.
   (rx-to-string '(sequence
                   string-start
                   (repeat 1 2 ".")
                   string-end)
                 :no-group))
  "What to ignore when traversing paths, getting children, etc..

NOTE: These should be compiled regex strings.")


(defun int<autogit>:path:ignore? (path regexes)
  "Return non-nil if PATH matches any of the REGEXES."
  (let ((func/name "int<autogit>:path:ignore?")
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
;; (int<autogit>:path:ignore? "x.y" int<autogit>:path:rx:names:ignore)
;; (int<autogit>:path:ignore? "." int<autogit>:path:rx:names:ignore)
;; (int<autogit>:path:ignore? ".." int<autogit>:path:rx:names:ignore)
;; (int<autogit>:path:ignore? "..." int<autogit>:path:rx:names:ignore)


(defun int<autogit>:path:type:valid? (caller type &optional nil-invalid?)
  "Error if TYPE is invalid. Return TYPE if valid (can be nil).

CALLER should be the name of the calling function, for use in the error
messages."
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



;;------------------------------------------------------------------------------
;; Predicates
;;------------------------------------------------------------------------------

(defun int<autogit>:path:exists? (path &optional type)
  "Return non-nil if PATH exists.

TYPE can be: :dir, :file, :symlink, or nil.
If TYPE is provided, PATH must exist \and\ be the correct type."
  ;;------------------------------
  ;; Error Cases
  ;;------------------------------
  ;; Errors on invaild type.
  (int<path>:type:valid? "int<autogit>:path:exists?" type)

  ;; Sanity check path...
  (cond ((not (stringp path))
         (error "int<autogit>:path:exists?: PATH must be a string: %S"
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
         (error "int<autogit>:path:exists?: Unhandled TYPE `%S'!!!"
                type))))
;; (int<autogit>:path:exists? (int<autogit>:path:join default-directory "autogit-path.el"))


;;------------------------------------------------------------------------------
;; Paths & Files
;;------------------------------------------------------------------------------

(defun int<autogit>:path:join (root &rest path)
  "Return absolute (file) path to ROOT + PATH.

Given a git ROOT, and a PATH of e.g. ('path/to' 'dir' 'with-file' 'file.txt'),
will return full /file/ path in platform-agnostic manner."
  (concat (file-name-as-directory (expand-file-name "" root))
          (directory-file-name (mapconcat #'file-name-as-directory path ""))))
;; (int<autogit>:path:join (car autogit:repos:path/commit) "foo")


(defun int<autogit>:path:changes:rel->abs (path-abs alist/changes)
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
;; (int<autogit>:path:changes:rel->abs default-directory (int<autogit>:changes:in-repo default-directory))


;;------------------------------------------------------------------------------
;; Canonicalize/Normalize Paths
;;------------------------------------------------------------------------------

(defun int<autogit>:path:canonicalize:file (path &rest segment)
  "Canonicalize/normalize a file PATH and path SEGMENTS.

Return an absolute path.

Does not fix or validate PATH or SEGMENT components; they are expected to be
valid."
  (expand-file-name (apply #'int<autogit>:path:join
                           path
                           segment)))
;; (int<autogit>:path:canonicalize:file "~" "personal" "something.exe" "zort.txt")
;; (int<autogit>:path:canonicalize:file "~" "personal" "something.exe" ".")
;; (int<autogit>:path:canonicalize:file "~" "personal" "something.exe" "..")


(defun int<autogit>:path:canonicalize:dir (path &rest segment)
  "Canonicalize/normalize a directory PATH and path SEGMENTS.

Return an absolute path.

Does not fix or validate PATH or SEGMENT components; they are expected to be
valid."
  ;; Fully qualify base as start of return value.
  (file-name-as-directory (apply #'int<autogit>:path:canonicalize:file path segment)))
;; (int<autogit>:path:canonicalize:dir "~" "personal" "something" "zort")


;;------------------------------------------------------------------------------
;; Directory Files (Children)
;;------------------------------------------------------------------------------

(defun int<autogit>:path:children:types (path/dir &optional absolute-paths? regex &rest types)
  "Return immediate children of PATH/DIR directory.

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
                   (seq-filter (lambda (x) (not (null x)))
                               types)
                 ;; nil is any/all types.
                 nil)))

    ;;------------------------------
    ;; Error Checking
    ;;------------------------------
    ;; Errors on invaild type.
    (dolist (type types)
      (int<autogit>:path:type:valid? "int<autogit>:path:children:types" type))

    (let ((func/name "int<autogit>:path:children:types"))
      (unless (stringp path/dir)
        (error "%s: PATH/DIR must be a string! Got: %S"
               func/name
               path/dir))

      (let ((path:root     (int<autogit>:path:canonicalize:dir path/dir))
            (save:dirs     (if types (memq :dir     types) t))
            (save:files    (if types (memq :file    types) t))
            (save:symlinks (if types (memq :symlink types) t))
            children)
        (unless (int<autogit>:path:exists? path:root :dir)
          (error "%s: PATH/DIR does not exist: %s"
                 func/name
                 path:root))

        ;;------------------------------
        ;; Find Children
        ;;------------------------------
        ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Contents-of-Directories.html
        ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/File-Attributes.html#Definition-of-file_002dattributes
        (dolist (child (directory-files-and-attributes path:root nil regex))
          (let* ((child:name  (car child))
                 (child:path  (int<autogit>:path:join path:root child:name))
                 (child:attrs (cdr child))
                 type)

            ;;------------------------------
            ;; Ignore / Determine Type
            ;;------------------------------
            ;; Explicitly ignore?
            (cond ((int<autogit>:path:ignore? child:name int<autogit>:path:rx:names:ignore)
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
;; (int<autogit>:path:children:types user-emacs-directory)
;; (int<autogit>:path:children:types user-emacs-directory nil)
;; (int<autogit>:path:children:types user-emacs-directory t)
;; (int<autogit>:path:children:types user-emacs-directory nil :file)
;; (int<autogit>:path:children:types user-emacs-directory nil :dir)
;; (int<autogit>:path:children:types default-directory nil nil)


(defun int<autogit>:path:children (path/dir &optional absolute-paths? type regex)
  "Return immediate children of PATH/DIR directory.

If ABSOLUTE-PATHS? is non-nil, return a list of absolute paths to the children.
Else return a list of names of children.

If TYPE is supplied, return only children of that type.

If REGEX is non-nil, return only children whose filename matches the REGEX."
  ;; Get by TYPE, then flatten to a single list of all children.
  (let ((by-types (int<autogit>:path:children:types path/dir absolute-paths? regex type))
        children)
    (dolist (type:assoc by-types)
      (setq children (if children
                         ;; Append to children.
                         (cons children (cdr type:assoc))
                       ;; Create children.
                       (cdr type:assoc))))
    children))
;; (int<autogit>:path:children default-directory)
;; (int<autogit>:path:children default-directory t)
;; (int<autogit>:path:children default-directory nil :file)
;; (int<autogit>:path:children default-directory nil :dir)


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'autogit-path)
;;; autogit-path.el ends here

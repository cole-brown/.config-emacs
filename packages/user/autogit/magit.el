;;; tools/autogit/magit.el -*- lexical-binding: t; -*-
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


(require 'magit)

(imp:require :autogit 'variables)
(imp:require :autogit 'path)
(imp:require :autogit 'output)


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
  (let ((alist/changes (int<autogit>:path:changes:rel->abs subdir-abs
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

(defmacro int<autogit>:magit:with-errors (&rest body)
  "Set magit error flag and run BODY."
  `(let ((magit-process-raise-error t))
     ,@body))


(defun int<autogit>:magit:fetch (dry-run buffer indent)
  "Fetch from remotes (w/ prune) and then pull current branch.

DRY-RUN should be nil/non-nil.

BUFFER should be a keyword, string or buffer object.
  - If it is the keyword `:messages', output to the *Messages* buffer using
    `message'.
  - Else inserts into BUFFER using `int<autogit>:buffer:with'.

INDENT must be a wholenum - 0 is 'not indented'."
  (int<autogit>:magit:with-errors
   ;;------------------------------
   ;; Fetch from remotes.
   ;;------------------------------
   (int<autogit>:display:message/indented buffer
                                         (int<autogit>:string:indent indent 1)
                                         ;; Dry-Run prefix?
                                         (list :prop :face:failure
                                               :text (concat (if dry-run
                                                                 autogit:text:dry-run
                                                               "GIT")
                                                             ":")))
   (int<autogit>:display:message/indented buffer
                                         (int<autogit>:string:indent indent 2)

                                         ;; Command or message.
                                         (list :prop :face:git
                                               :text "`magit-fetch-all-prune'"))
   (magit-fetch-all-prune)

   ;;------------------------------
   ;; Pull current branch.
   ;;------------------------------
   (int<autogit>:display:message/indented buffer
                                         (int<autogit>:string:indent indent 1)
                                         ;; Dry-Run prefix?
                                         (list :prop :face:failure
                                               :text (concat (if dry-run
                                                                 autogit:text:dry-run
                                                               "GIT")
                                                             ":")))
   (int<autogit>:display:message/indented buffer
                                         (int<autogit>:string:indent indent 2)

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
  - Else inserts into BUFFER using `int<autogit>:buffer:with'.

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
  (int<autogit>:magit:with-errors
   ;; Output messages first.
   (int<autogit>:display:message/indented buffer
                                         (int<autogit>:string:indent indent 1)

                                         ;; Dry-Run prefix?
                                         (list :prop :face:failure
                                               :text (concat (if dry-run
                                                                 autogit:text:dry-run
                                                               "GIT")
                                                             ":")))
   (int<autogit>:display:message/indented buffer
                                         (int<autogit>:string:indent indent 2)

                                         ;; Command or message.
                                         (list :prop :face:git
                                               :text (if (eq message :args-as-msg)
                                                         (concat "git " (string-join args " "))
                                                       message)))

   ;; Do not actually run the command in dry-run mode.
   (unless dry-run
     (apply #'magit-call-git args))))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :autogit 'magit)

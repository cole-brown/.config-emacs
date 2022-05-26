;;; load.el --- Loading files is fun, innit? -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2022-04-26
;; Modified:   2022-04-26
;; URL:        https://github.com/cole-brown/.config-emacs
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;  Loading files is fun, innit?
;;
;;; Code:


(imp:require :innit 'debug)

;;------------------------------------------------------------------------------
;; Ordered Loading (by file/dir name)
;;------------------------------------------------------------------------------

(defun innit:load:ordered:files (root relative)
  "Load files in ROOT + RELATIVE directory in alphanumeric order."
  ;; Ensure absolute path...
  (let* ((func/name "innit:load:ordered:files")
         (path/root/abs (path:canonicalize:absolute root))
         (path/dir/abs  (path:canonicalize:absolute root relative))
         (path/dir/rel  (path:canonicalize:relative path/dir/abs path/root/abs))
         (overall-result :init) ;; Start as something non-nil.
         file-result)
    (nub:out :innit
             :debug
             (imp:file:current)
             "innit:load:ordered:files: Load files at '%s/'..."
             path/dir/rel)

    (if (not (path:exists? path/dir/abs :dir))
        ;;------------------------------
        ;; Error: Invalid Input
        ;;------------------------------
        (error "[ERROR] innit:load:ordered:files: PATH is not a directory?: %S" path/dir/abs)

      ;;------------------------------
      ;; Load All Children
      ;;------------------------------
      ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Contents-of-Directories.html
      ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/File-Attributes.html#Definition-of-file_002dattributes

      ;; Get all the ".el" files, sorted.
      (dolist (path/file/abs (directory-files path/dir/abs :absolute-paths innit:rx:filename)) ;; TODO-PATH: A `path' function here?
        ;; Skip additional loads once something fails.
        (when overall-result
          ;; Drop the extension so `load' can choose ".elc" if appropriate.
          (let* ((path/load/abs (path:name:base path/file/abs))
                 (path/load/rel (path:relative path/load/abs path/root/abs))
                 (unwind/interrupted? t))
            ;; If there was an error, we want to know which file 'innit' was
            ;; loading, but also want the stack trace to be unaffected so don't use `condition-case'.
            ;; Instead, use `unwind-protect' with a guard variable (`success?').
            (unwind-protect
                ;;---
                ;; Do This:
                ;;---
                (prog1
                    (let ((feature/name (list :innit path/load/rel)) ;; Fake out some feature name.
                          ;; Just `file:name', not `file:name:base', as we've already stripped the extension.
                          (load/name   (file:name path/load/rel)))
                      (nub:out :innit
                               :debug
                               (imp:file:current)
                               "innit:load:ordered:files: Load file '%s'..."
                               load/name)
                      (imp:timing
                          feature/name
                          load/name
                          path/dir/rel
                        (setq file-result (load path/load/abs)))
                      (setq overall-result (and overall-result file-result)))
                  ;; Got throught the `unwind-protect' without being pre-empted by an error or other signal.
                  (setq unwind/interrupted? nil))
              ;;---
              ;; Finally/Regardless, Do This:
              ;;---
              ;; Say what failed in case the failure didn't say.
              (when unwind/interrupted?
                ;; _Print_ an error-level message out, but don't signal an error.
                (nub:error:sink
                    :innit
                    func/name
                    :debug ;; Output to `:debug' sink(s) instead of `:error'.
                  "Encountered error while loading '%s': %S %S"
                  path/load/abs
                  ;; error symbol: `error', `user-error', `arith-error', etc.
                  (car err)
                  ;; Always a nice string, even for errors without messages.
                  ;;   e.g. `arith-error' -> "Arithmetic error"
                  (error-message-string err)))))))

      ;;------------------------------
      ;; Post-Loading
      ;;------------------------------
      ;; Could put a message here if we want to be chatty?

      ;; Make sure we return the summarized result.
      overall-result)))


(defun innit:load:ordered:dirs (root relative)
  "Load files of ROOT + RELATIVE and then load files of its subdirectories.

Loads files of ROOT + RELATIVE first, then loads each immediate subdir's (does
not recurse).

NOTE: Uses `innit:load:ordered:files' for loading the files."
  ;; Ensure absolute path...
  (let* ((path/root/abs (path:canonicalize:absolute root))
         (path/dir/abs  (path:canonicalize:absolute root relative))
         (path/dir/rel  (path:canonicalize:relative path/dir/abs path/root/abs))
         (feature/name   (list :innit path/dir/rel)) ;; Fake out some feature name...
         (overall-result :init)) ;; Start as something non-nil.
    (nub:out :innit
             :debug
             (imp:file:current)
             "innit:load:ordered:dirs: Load '%s'..."
             path/dir/abs)
    ;; Time everything...
    (imp:timing
        ;; Fake out some feature name...
        (list :innit path/dir/rel)
        path/dir/rel
        path/root/abs

      ;; NOTE: We're trusting in `innit:load:ordered:files' to do nice error messages for us.

      ;;------------------------------
      ;; 1) Load our own files.
      ;;------------------------------
      (nub:out :innit
               :debug
               (imp:file:current)
               "innit:load:ordered:dirs: Load files in '%s/'..."
               path/dir/rel)
      (setq overall-result (and overall-result
                                (innit:load:ordered:files path/root/abs path/dir/rel)))

      ;;------------------------------
      ;; 2) Load subdirs.
      ;;------------------------------
      (unless overall-result ;; Unless loading files failed...
        ;; TODO-PATH: Do I want this as a path function?..
        (dolist (child (directory-files-and-attributes path/dir/abs :absolute-paths))
          (let* ((child/path/abs (car child))
                 (child/path/rel (path:canonicalize:relative child/path/abs path/root/abs))
                 (child/attrs    (cdr child)))
            (nub:out :innit
                     :debug
                     (imp:file:current)
                     "innit:load:ordered:dirs: Load files in '%s/'..."
                     child/path/rel)
            ;; `t' is the attr type for directories. Ignore all the other file types.
            (when (and overall-result
                       ;; TODO-PATH: ...would also need this then.
                       (eq (file-attribute-type child/attrs) t))
              (setq overall-result (and overall-result
                                        (innit:load:ordered:files path/root/abs
                                                                  child/path/rel)))))))

      ;; Return summarized result.
      overall-result)))


(defun innit:load (caller step)
  "Load `innit:path:core/boot' + STEP files & dirs, save result to `innit:status'.

If loading isn't successful, signal an error using CALLER (e.g. \"init.el\") in
error string."
  (nub:out :innit
           :debug
           (imp:file:current)
           "innit:load (@%s): %s"
           caller
           step)
  (let ((result (innit:load:ordered:dirs innit:path:core/boot step)))
    (innit:status:set step result)
    (unless result
      (error "[ERROR] innit:load:with-error: '%s' failed loading '%s' files. `innit:status': %S"
             caller
             step
             result))
    result))


;;------------------------------------------------------------------------------
;; Optimization
;;------------------------------------------------------------------------------

(defun innit:optimize? ()
  "Guard predicate for optimizing interactive start-up.

Return false (i.e. do not optimize) if:
  - Emacs is in:
    - daemon (service) mode
    - noninteractive (batch/script) mode
  - Emacs or `innit' is in:
    - some sort of debug mode"
  (not (or (daemonp)
           noninteractive
           (innit:debug? :any))))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :innit 'load)

;;; early-init.el --- Early Init. -*- lexical-binding: t; -*-
;;
;; Author: Cole Brown <code@brown.dev>
;; URL:    https://github.com/cole-brown/.config-emacs
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Emacs 27.1 introduced early-init.el, which is run before init.el, before
;; package and UI initialization happens, and before site files are loaded.
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Modules Required for Init
;;------------------------------------------------------------------------------

(let ((path-core-modules (expand-file-name "core/modules/" user-emacs-directory)))
  ;; Order matters here. These build on each other.
  (load (expand-file-name "emacs/imp/init" path-core-modules))

  ;; TODO: Make timing optional. Don't enable here, and let it be set or not in "settings.el"?
  (customize-set-variable 'imp:timing:enabled? t)

  ;; Group all the required-for-innit modules together in timing info.
  (imp:timing
      '(:innit early-init modules)
      "early-init.el"
      (imp:path:current:dir)
    (load (expand-file-name "emacs/str/init"   path-core-modules))
    (load (expand-file-name "emacs/alist/init" path-core-modules))
    (load (expand-file-name "emacs/path/init"  path-core-modules))
    (load (expand-file-name "output/nub/init"  path-core-modules))
    (load (expand-file-name "emacs/innit/init" path-core-modules))))


(let ((file/this (imp:file:current)))
  ;;------------------------------------------------------------------------------
  ;; Settings & Overrides
  ;;------------------------------------------------------------------------------

  ;; Load these settings first so they can override any `defvar' and be prepared
  ;; for wherever they're needed in the init sequence.
  (imp:path:root :settings user-emacs-directory)
  ;; Don't want to error if file isn't there, but do want to error if loading the
  ;; file causes an error, so `:optional t` should be perfect.
  (imp:load :feature  '(:settings)
            :optional t
            :filename "settings")


  ;;------------------------------------------------------------------------------
  ;; Output
  ;;------------------------------------------------------------------------------

  ;; Set up `nub' for use by `innit'.
  (let* (;; TODO: Make this a defcustom so it can be changed in settings.
         (nub-innit-pop-to-buffer t)
         ;; TODO: Make this a defcustom too?
         (nub-innit-buffer-name "ⓘ-innit-ⓘ")
         ;; TODO: More defcustoms?
         nub-innit-interactive-debug-tags
         nub-innit-levels-prefixes
         nub-innit-levels-enabled
         (nub-innit-sink-fn (nub:output:sink :innit
                                             nub-innit-buffer-name
                                             nub-innit-pop-to-buffer)))
    (nub:vars:init :innit
                   nub-innit-interactive-debug-tags ; common debug tags (for interactive toggling auto-complete help)
                   nub-innit-levels-prefixes ; output message prefixes
                   nub-innit-levels-enabled  ; default enabled/disabled per output levels
                   ;; Sinks by Level: Add our sink to all so that they get
                   ;; collected there as well as output by default funcs.
                   (list (cons :error (list nub-innit-sink-fn :default))
                         (cons :warn  (list nub-innit-sink-fn :default))
                         (cons :info  (list nub-innit-sink-fn :default))
                         (cons :debug (list nub-innit-sink-fn :default)))))

  (nub:out :innit
           :debug
           file/this
           ;; Is "Power On Self Test" a good term to steal?
           "POST")


  ;;------------------------------------------------------------------------------
  ;; Init Constants & Variables
  ;;------------------------------------------------------------------------------
  ;; We already called the core "core", so... The next layer is "mantle", I guess?
  ;; And a third layer would be called "crust"?

  (defconst innit:path:core/boot (path:join user-emacs-directory "core/boot/")
    "Absolute path to the \"core/boot\" subdirectory.")


  (defconst innit:path:core/modules (path:join user-emacs-directory "core/modules/")
    "Absolute path to the \"core/modules\" subdirectory.")


  (defconst innit:filenames:mantle
    '(:init   "init.el"
      :config "config.el")
    "Names of files to look for in `innit:paths:mantle' for loading.")


  (defvar innit:features:mantle nil
    "List of `imp' feature lists to load after their core counterpart.

An element in the list would be either 1) just the keyword,
or 2) a specific sub-feature.
  1) '(:path)
  2) '(:path regex)

Each path in the list will optionally load a file (if it is present) during a
specific part of init:
  1) \"init.el\"
     - Just after core's init is finished, and before config.
  2) \"config.el\"
     - Just after core's config is finished, before completing start-up.

Paths should be absolute directory paths. \"init.el\" and \"config.el\" will be
appended to them for looking for the proper file to load.")


  (defconst innit:rx:filename
    (rx string-start
        (one-or-more printing) ".el"
        string-end)
    "Base filename must match to be loaded by `innit:load:files:ordered'.")


  (defvar innit:status nil
    "Alist of innit sequence keyword to status.")


  ;;------------------------------------------------------------------------------
  ;; Innit Loading Helpers
  ;;------------------------------------------------------------------------------
  ;; Some "load from 'core/boot' helper functions and related variables/constants.


  ;;------------------------------
  ;; `innit:status' getter/setter
  ;;------------------------------

  (defun innit:status:set (step status)
    "Set STATUS of innit STEP.

STATUS should be, at minimum, boolean.

STEP is the \"core/boot\" subdirectory path string:
  - \"[...]/core/boot/20-init/99-finalize\" -> \"20-init/99-finalize\""
    (if (null innit:status)
        (setq innit:status (list (cons step status)))
      (setf (alist-get step innit:status nil nil #'string=) status)))
  ;; (setq innit:status nil)
  ;; innit:status
  ;; (innit:status:set "01-test/11-foo" t)
  ;; (innit:status:set "01-test/11-foo" :success)
  ;; (innit:status:set "01-test/12-bar" :partial-success)
  ;; innit:status
  ;; (innit:status:set "01-test/13-qux" nil)


  (defun innit:status:get (step)
    "Get status of innit STEP string.

nil is considered a failure state as it is the return when the sequence
keyword doesn't appear in `innit:status'.

Innit steps are \"core/boot\" subdirectory path strings:
  - \"[...]/core/boot/20-init/99-finalize\" -> \"20-init/99-finalize\""
    (alist-get step innit:status nil nil #'string=))
  ;; innit:status
  ;; (innit:status:get "01-test/11-foo")
  ;; (innit:status:get "01-test/12-bar")
  ;; (innit:status:get "01-test/13-qux")
  ;; (innit:status:get "01-test/14-dne")
  ;; (innit:status:get "98-dne/99-done")


  ;;------------------------------
  ;; Ordered Loading (by file/dir name)
  ;;------------------------------

  (defun innit:load:ordered:files (root relative)
    "Load files in ROOT + RELATIVE directory in alphanumeric order."
    ;; Ensure absolute path...
    (let* ((path/root/abs (path:canonicalize:absolute root))
           (path/dir/abs  (path:canonicalize:absolute root relative))
           (path/dir/rel  (path:canonicalize:relative path/dir/abs path/root/abs))
           (overall-result :init) ;; Start as something non-nil.
           file-result)
      (nub:out :innit
               :debug
               file/this
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
                   (path/load/rel (path:relative path/load/abs path/root/abs)))
              (condition-case err
                  (let ((feature/name (list :innit path/load/rel)) ;; Fake out some feature name.
                        ;; Just `file:name', not `file:name:base', as we've already stripped the extension.
                        (load/name   (file:name path/load/rel)))
                    (nub:out :innit
                             :debug
                             file/this
                             "innit:load:ordered:files: Load file '%s'..."
                             load/name)
                    (imp:timing
                        feature/name
                        load/name
                        path/dir/rel
                      (setq file-result (load path/load/abs)))
                    (setq overall-result (and overall-result file-result)))
                ;; Say what failed in case the failure didn't say.
                (error
                 (error "[ERROR] innit:load:ordered:files: Encountered error while loading '%s': %S %S"
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
               file/this
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
                 file/this
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
                       file/this
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
             file/this
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


  (defun innit:feature:mantle:add (caller &rest feature)
    "Add `imp' FEATURE to mantle init/config load sequence.

FEATURE should already have an `imp' root.

CALLER should be string of function or file name which called this."
    ;;------------------------------
    ;; Error Checks
    ;;------------------------------
    ;; TODO: is there an `imp' function to use for validation instead?
    (cond ((null feature)
           (error "[ERROR] innit:feature:mantle:add: %s: FEATURE cannot be nil, got: %S"
                  caller
                  feature))

          ((not (keywordp (nth 0 feature)))
           (error "[ERROR] innit:feature:mantle:add: %s: FEATURE must start with a keyword! Got: %S"
                  caller
                  (nth 0 feature)))

          ((not (seq-every-p #'symbolp feature))
           (error "[ERROR] innit:feature:mantle:add: %s: FEATURE must only be keywords/symbols! Got: %S"
                  caller
                  feature))

          ;;------------------------------
          ;; Valid; add to list.
          ;;------------------------------
          (t
           (push feature innit:features:mantle))))


  ;;------------------------------------------------------------------------------
  ;; Load Early-Init Files
  ;;------------------------------------------------------------------------------

  (nub:out :innit
           :debug
           file/this
           "Boot Loader: 00 Early")

  (imp:timing
      '(:innit early-init load)
      file/this
      (imp:path:current:dir)
    (innit:load file/this "00-early")))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
;; TODO: Provide anything? Push to some list or whatever that can be added to imp after the fact?

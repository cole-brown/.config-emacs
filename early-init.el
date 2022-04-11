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


;; TODO: an output debug thing that isn't an hack.
(defun innit:debug:hack (msg &rest args)
  "MSG ARGS."
  (let ((buffer (get-buffer-create "innit")))
    (with-current-buffer buffer
      (let ((buffer:end (point-max)))
        (goto-char buffer:end)
        (unless (= (point-min) buffer:end)
          (insert "\n"))
        (insert (format msg args))))
    ;; Show buffer?
    (pop-to-buffer buffer)))

(innit:debug:hack "[innit] early-init.el: hello?")


;;------------------------------------------------------------------------------
;; Settings & Overrides
;;------------------------------------------------------------------------------

;; Load these settings first so they can override any `defvar' and be prepared
;; for wherever they're needed in the init sequence.
(let ((settings:path (expand-file-name "settings.el" user-emacs-directory)))
  ;; Don't want to error if file isn't there, but do want to error if loading the
  ;; file causes an error, so check if it exists first.
  (when (file-exists-p settings:path)
    ;; Now let `load' error if it wants - can't error on non-existant optional file anymore.
    (load (expand-file-name "settings.el" user-emacs-directory))))


;;------------------------------------------------------------------------------
;; Init Constants & Variables
;;------------------------------------------------------------------------------
;; We already called the core "core", so... The next layer is "mantle", I guess?
;; And a third layer would be called "crust"?


(defconst innit:path:core/boot (expand-file-name "core/boot/" user-emacs-directory)
  "Absolute path to the \"core/boot\" subdirectory.")


(defconst innit:path:core/modules (expand-file-name "core/modules/" user-emacs-directory)
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

(defun innit:load:ordered:files (path)
  "Load files in PATH directory in alphanumeric order."
  ;; Ensure absolute path...
  (let ((path (expand-file-name path))
        (overall-result :init) ;; Start as something non-nil.
        file-result)
    (if (not (file-directory-p path))
        ;;------------------------------
        ;; Error: Invalid Input
        ;;------------------------------
        (error "[ERROR] innit:load:ordered:files: PATH is not a directory?: %S" path)

      ;;------------------------------
      ;; Load All Children
      ;;------------------------------
      ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Contents-of-Directories.html
      ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/File-Attributes.html#Definition-of-file_002dattributes

      ;; Get all the ".el" files, sorted.
      (dolist (filename (directory-files path :absolute-paths innit:rx:filename))
        ;; Skip additional loads once something fails.
        (when overall-result
          (let ((loadname (expand-file-name (file-name-sans-extension filename) path)))
            (condition-case err
                ;; Load, dropping the extension so it can choose ".elc" if appropriate.
                (progn
                  (setq file-result (load loadname))
                  (setq overall-result (and overall-result file-result)))
            ;; Say what failed in case the failure didn't say.
            (error
             (error "[ERROR] innit:load:ordered:files: Error loading '%s': %S %S"
                    loadname
                    ;; error symbol: `error', `user-error', etc.
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


(defun innit:load:ordered:dirs (path dir)
  "Call `innit:load:ordered:files' for each directory of PATH + DIR in order.

NOTE: Loads files of PATH + DIR first, then loads each immediate subdir's."
  ;; Ensure absolute path...
  (let ((root (expand-file-name dir path))
        (overall-result :init)) ;; Start as something non-nil.

    ;; NOTE: We're trusting in `innit:load:ordered:files' to do nice error messages for us.

    ;;------------------------------
    ;; 1) Load our own files.
    ;;------------------------------
    (setq overall-result (and overall-result
                              (innit:load:ordered:files root)))

    ;;------------------------------
    ;; 2) Load subdirs.
    ;;------------------------------
    (unless overall-result ;; Unless loading files failed...
      (dolist (child (directory-files-and-attributes root :absolute-paths))
        (let* ((child:path  (car child))
               (child:attrs (cdr child)))
          ;; `t' is the attr type for directories. Ignore all the other file types.
          (when (and overall-result
                     (eq (file-attribute-type child:attrs) t))
            (setq overall-result (and overall-result
                                      (innit:load:ordered:files child:path)))))))

    ;; Return summarized result.
    overall-result))


(defun innit:load (caller step)
  "Load `innit:path:core/boot' + STEP files & dirs, save result to `innit:status'.

If loading isn't successful, signal an error using CALLER (e.g. \"init.el\") in error string."
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

(innit:load "early-init.el" "00-early")


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
;; TODO: Provide anything? Push to some list or whatever that can be added to imp after the fact?

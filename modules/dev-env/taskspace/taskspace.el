;;; taskspace/taskspace.el --- Extremely Simple Taskspace/Workspace Management  -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2019-04-24
;; Modified:   2022-07-06
;; Version:    2.3
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; taskspace.el is a KISS taskspace (workspace) generator/manager.
;;
;; FAQ:
;;   1) Can it do X?
;;      - No... I really meant simple.
;;   2) Can it do multiple taskspace roots?
;;      - Yes... I had to make it less simple.
;;
;; It can make a folder based on a simple dating and numbering scheme, with a
;; simple description tacked on for human usability.
;;
;; It can copy files into the new taskspace. It can generate files based on a
;; static string or a function supplied in taskspace vars.
;;
;; It can copy the taskspace's full path or name to the kill ring/clipboard.
;;
;; It can open the taskspace dir itself (or the taskspace parent dir)
;; in a buffer.
;;
;; It can 'deal' with these kind of taskspaces:
;;   - Self-Contained
;;;    - Taskspace dir contains:
;;       - notes,
;;       - data,
;;       - etc.
;;    - No other directories or files.
;;   - Split (taskspace notes are separate; taskspace dir contains data, etc)
;;;    - Taskspace dir contains:
;;       - data,
;;       - etc.
;;    - Notes file exists separately.
;;
;;
;;------------------------------
;; Commands:
;;------------------------------
;;   The Main Command:
;;     `taskspace:create'
;;       - Create a new taskspace. Will prompt for the short description.
;;         - e.g. description of "2019-01-01_2_some-task-name" is
;;           "some-task-name".
;;
;;   DWIM Commands:
;;     - Accepts numeric prefix arg.
;;       - 0 or no prefix: Today's date
;;       - positive: Future date; N days from now.
;;       - negative: Past date; N days back.
;;     - DWIM means:
;;       - If none for date: Create.
;;       - If just one existing: Return it.
;;       - If multiple: Choose from prompt of options.
;;     `taskspace:dwim:dir'
;;       - Returns fully qualified path to taskspace.
;;         - e.g. "c:/home/user/taskspace/2019-01-01_2_some-task-name"
;;     `taskspace:dwim:name'
;;       - Returns taskspace (directory) name only.
;;         - e.g. "2019-01-01_2_some-task-name"
;;
;;   Other Commands:
;;     `taskspace:dired:task'
;;       - Opens the directory of a task in emacs
;;         (uses find-file, so defaults to dired-mode buffer).
;;     `taskspace:dired:root'
;;       - Opens the directory of all tasks in emacs (aka `(int<taskspace>:config group :dir/tasks)')
;;         (uses find-file, so defaults to dired-mode buffer).
;;     `taskspace:shell'
;;       - Opens a shell.
;;       - Use `taskspace:dwim:dir' to determine which taskspace is
;;         intended from the context.
;;
;;
;;------------------------------
;; Settings:
;;------------------------------
;; See 'General Settings' header to find out what all can be customized per
;; group right now.
;;
;;
;;------------------------------
;; Use-Package Config, Simple:
;;------------------------------
;;
;;  (use-package taskspace)
;;
;;
;;------------------------------
;; Use-Package Config, Multi-Group:
;;------------------------------
;;
;; (use-package taskspace
;;   ;; My own personal package - do not package manager it.
;;   :ensure nil
;;
;;   ;;------------------------------
;;   :init
;;   ;;------------------------------
;;
;;   ;;---
;;   ;; General (Non-Per-Domain) Init...
;;   ;;---
;;   (defun my/taskspace/generate (taskname taskpath)
;;     "NOTE: Could be redefined later for more work-specific details, so check
;; e.g. 'finalize-domain-secret.el' for a redef. Or 'C-h f
;; my/taskspace/generate' and see what file it's defined
;; in.
;; "
;;     ;; Format:
;;     ;; header snippet key
;;     ;;
;;     ;; taskname
;;     ;; taskpath
;;     ;;
;;     ;; 'mkdir cmd'
;;     ;;
;;     ;; fancy box to separate this stuff from start of normal notes
;;     (format (concat "%s\n" ;; header
;;                     "\n"
;;                     "#+TASKSPACE: %s\n" ;; taskpath
;;                     "%s\n" ;; taskname
;;                     "\n"
;;                     "%s\n" ;; mkdir cmd for remote servers
;;                     "\n"
;;                     "%s\n" ;; fancy box top
;;                     "%s\n" ;; fancy box middle
;;                     "%s\n" ;; fancy box bottom
;;                     "\n\n")
;;             "my-header-snippet"
;;             taskpath
;;             taskname
;;             (format "mkdir ~/temp/%s" taskname)
;;             "     ┌┬┬┬──────────────────────────────────────────────────────────────┬┬┬┐"
;;             "     ├┼┼┤                             ...                              ├┼┼┤"
;;             "     └┴┴┴──────────────────────────────────────────────────────────────┴┴┴┘"
;;             ))
;;
;;   ;;---
;;   ;; "Home" Domain
;;   ;;---
;;
;;   ;; I can redef later if I want different ones..
;;   (defalias 'my/taskspace/generate/home 'my/taskspace/generate)
;;
;;   (defvar my/taskspace/group/home
;;     '((:type/notes        :self-contained)
;;       (:format/datetime   my/datetime/format/yyyy-mm-dd)
;;       (:dir/tasks         my/taskspace/path/tasks/home)
;;       (:dir/notes         my/taskspace/path/notes/home)
;;       (:file/new/generate ((".projectile" "") ;; projectile: empty file
;;                            ;; notes.org: setup with org header snippet
;;                            ;; ready to go
;;                            ((int<taskspace>:config :home :file/notes)
;;                             my/taskspace/generate/home))))
;;     "Custom settings for my `:home' taskspace group.")
;;
;;   ;;---
;;   ;; "Work" Domain
;;   ;;---
;;
;;   ;; I can redef later if I want different ones..
;;   (defalias 'my/taskspace/generate/work 'my/taskspace/generate)
;;
;;   (defvar my/taskspace/group/work
;;     '((:type/notes        :noteless)
;;       (:format/datetime   my/datetime/format/yyyy-mm-dd)
;;       (:dir/tasks         my/taskspace/path/tasks/work)
;;       (:dir/notes         my/taskspace/path/notes/work)
;;       (:file/new/generate ((".projectile" "") ;; projectile: empty file
;;                            ;; notes.org: setup with org header snippet
;;                            ;; ready to go
;;                            ((int<taskspace>:config :home :file/notes)
;;                             my/taskspace/generate/home))))
;;     "Custom settings for my `:work' taskspace group.")
;;
;;   ;;------------------------------
;;   :custom
;;   ;;------------------------------
;;
;;   (taskspace:groups
;;    '((:work    "Work Taskspace" my/taskspace/group/work)
;;      (:home    "Home Taskspace" my/taskspace/group/home)
;;      (:default "Defaults"       taskspace:group:default))))
;;
;;
;;; Code:


(require 'cl-lib) ;; for `some'
(require 'seq) ;; for `seq-contains'
;; TODO: switch over to `:path' functions
(require 'f) ;; for nicer file api
(require 'dash)
(require 'org-element)

(imp:require :dlv)


;;---------------------------------taskspace------------------------------------
;;--                   Simple Taskspace / Task Management                     --
;;------------------------------------------------------------------------------

;; §-TODO-§ [2020-02-25]: cleanup pass?
;; §-TODO-§ [2020-02-25]: find/do the todos here?


(defgroup taskspace nil
  "Extremely Simple Taskspace/Workspace Management.

Tool for creating/using simple taskspaces (workspaces) for short tasks,
bug investigation, log munging, or whatever."
  :group 'convenience)


;;------------------------------------------------------------------------------
;; Multiple Separate Taskspaces
;;------------------------------------------------------------------------------

(defcustom taskspace:groups
  '((:default "Taskspace" taskspace:group:default))
  "Definitions for multiple task spaces. Each will have its own settings.
Each entry in the alist is a list of: (keyword string settings-variable)

'keyword' is used internally to identify the taskspace groups.

'string' is a name used for display purposes.

'settings-variable' should be the big settings alist (see
`taskspace:group:default' for the default's settings)."
  :group 'taskspace
  :type '(alist :key-type symbol
                :value-type string))


;;------------------------------------------------------------------------------
;; Per-Taskspace Settings
;;------------------------------------------------------------------------------

(defcustom taskspace:group:default
  '((:type/notes :self-contained
    (concat
     "What kind of taskspace to create by default.\n"
     "\n"
     "It can 'deal' with these kind of taskspaces:\n"
     "  - `:self-contained' - Self-Contained (taskspace contains everything)\n"
     "    - Taskspace dir contains:\n"
     "      - notes,\n"
     "      - data,\n"
     "      - etc.\n"
     "   - No other directories or files.\n"
     "  - `:noteless' - Split (taskspace notes are separate from rest)\n"
     "    - Taskspace dir contains:\n"
     "      - data,\n"
     "      - etc.\n"
     "   - Notes file exists separately.\n"))

   (:format/datetime "%Y-%m-%d"
    "Date format for parsing/naming taskspace directory names.")

   (:function/shell  #'shell
    (concat
     "Function to call to open shell buffer. `shell' and `eshell' work. "
     "Opens the current taskspace's top dir in an emacs shell buffer."))

   (:dir/tasks (path:absolute:dir "taskspace" user-emacs-directory)
    (concat
     "User's root taskspace folder for small work tasks. "
     "All taskspaces for this group will be created here."))

   (:dir/notes (path:absolute:dir "taskspace" user-emacs-directory)
    (concat
     "User's folder for all notes that are in `:noteless' taskspaces. "
     "Unused in `:self-contained' taskspaces."))

   (:file/new/copy (path:absolute:dir "taskspace-new"
                                      (int<taskspace>:config :default :dir/tasks))
    "User's folder for files to copy into new taskspaces.")

   (:file/new/generate (((int<taskspace>:config :default :file/notes) "") ;; empty file
                        (".projectile" "")) ;; also empty
    (concat
     "Files to generate for new taskspaces. Expects an alist like:\n"
     "(('file1.name' 'contents') ('file2.name' #'your-gen-function))\n"
     "\n"
     "Note: `group', `taskname' and `taskpath' are supplied as the args to \n"
     "the generator functions. Taskpath is the fully expanded file path.\n"
     "Should return a string of the file's contents.\n"
     "e.g.: (defun my/taskspace/gen-org-notes (taskname taskpath)\n"
     "        (format ...))\n"))

   (:file/notes "_notes.org" "File for storing/recording notes about a task.")

   ;; TODO: REGEX
   ;; TODO: regex all these or somethinng?
   ;; TODO: REGEX
   (:dir/tasks/ignore
    ;; TODO: how do I even do string and/or regexes? Different alist entries
    ;; probably since regexes are just strings?
    ("." ".."
     "00_archive"
     (path:absolute:file (int<taskspace>:config :default :file/new/copy)))
    (concat
     "Always ignore these when getting/determining a taskspace directory. "
     "Can be strings or functions."))

   (:naming/separator "_"
    "Split directory name on this to extract date, number, and description.")

   ;; TODO: Turn these into regexes w/ capture groups, I think?..
   ;; Have make-name and split-name use the regexes to make/split.
   ;; http://ergoemacs.org/emacs/elisp_string_functions.html
   ;; TODO: use this nice regex builder (elisp sexprs)?:
   ;;   https://www.reddit.com/r/emacs/comments/cf8r83/easier_editing_of_elisp_regexps/eu84ob1/
   (:naming/parts-alists
    (
      ;; Three part. Code does this all the time?
      ((date . 0)
       (number . 1)
       (description . 2))
      ;; Two part. Human does this most of the time...
      ((date . 0)
       (description . 2))
      )
    (concat
     "Order of items in task's descriptive directory name. List of alists. "
     "First one of the correct length is used currently."))

   ;; §-TODO-§ [2020-08-18]: Turn this into an `(rx ...)'.
   (:naming/description/rx/valid "^[[:alnum:]_\\-]\\{3,\\}$"
    "Letters, numbers, underscore, and hypen are valid.")

   (:dir/tasks/org/keyword "TASKSPACE"
    (concat
     "Name of the Property/Keyword that might hold a pointer to the "
     "taskspace directory in a taskspace's org notes file.\n"
     "\n"
     "e.g.: if this line is in the '_notes.org':"
     "#+TASKSPACE: ~/path/to/taskspace/2020-03-17_0_example"
     "...then `int<taskspace>:org:keyword:get' can be used to get it for"
     "`taskspace:dwim:dir'.")))

  "Alist of custom settings (name/value/docstr) for default taskspace."
  :group 'taskspace
  ;; :type no idea so I'm leaving it out... sexp?
  )


;;------------------------------------------------------------------------------
;; Per-Group Config/Settings Helpers
;;------------------------------------------------------------------------------

(defun int<taskspace>:config (group key &optional field)
  "Get the FIELD of KEY from GROUP's settings alist in `taskspace:groups'.
If GROUP doesn't exist in `taskspace:groups', this will look for `:default'.
Errors if nothing is found in GROUP or `:default' settings.

If FIELD is nil or `:setting', gets the setting's value.

If FIELD is `:docstr', gets the setting's docstr.

If FIELD is `:key', gets the setting's key, which is KEY. Almost
useless, but does validate entry's exists."
  (let ((entry nil)
        (settings (int<taskspace>:config:group:get/settings group)))
    ;; First, try to get from group's settings (if we found group's settings).
    (when settings
      (setq entry (int<taskspace>:config:get key settings)))

    ;; Second, if needed, try to get from default/fallback settings.
    (when (null entry)
      ;; Didn't find the requested group... use defaults.
      (setq entry
            (int<taskspace>:config:get
             key
             (int<taskspace>:config:group:get/settings :default))))

    ;; Now we can finally either reduce down to the value or error out.
    (if (null entry)
        ;; Not found anywhere; error out.
        (nub:error
            :taskspace
            "int<taskspace>:config"
          "Taskspace: Could not find setting '%S' in group '%S' or default"
          key group)

      ;; Got group; return value of requested field.
      ;; Entry should be: (key value docstr)
      ;; Now figure out which is requested, what it actually is, and return it.
      (cond ((eq field :key)
             (int<taskspace>:config:entry:get/key entry))

            ((eq field :docstr)
             (int<taskspace>:config:entry:get/docstr entry))

            (t
             (int<taskspace>:config:entry:get/setting entry))))))
;; (int<taskspace>:config :home :format/datetime)
;; (int<taskspace>:config :home :format/datetime :setting)
;; (int<taskspace>:config :home :format/datetime :docstr)
;; (int<taskspace>:config :home :format/datetime :key)
;; (int<taskspace>:config :home :type/notes)
;; (int<taskspace>:config :home :dir/tasks)
;; (int<taskspace>:config :home :dir/tasks/ignore)
;; (int<taskspace>:config :home :naming/parts-alists)
;; (int<taskspace>:config :home :file/new/copy)


(defun int<taskspace>:config:entry:get/key (entry)
  "Helper for int<taskspace>:config.

Given an ENTRY from a group's settings alist, returns its key or nil."
  (nth 0 entry))


(defun int<taskspace>:config:entry:get/setting (entry)
  "Helper for int<taskspace>:config.

Given an ENTRY from a group's settings alist, turn it into an actual setting.

ENTRY is the alist tuple of (taskspace-keyword thing-to-figure-out docstr).

Thing-to-figure-out could be: a symbol that needs evaluated, a string, a
function that needs called, etc."
  (let ((setting (nth 1 entry)))
    (cond
     ;; Function: Check before listp. If `setting' is #'ignore then I guess it's
     ;; actually (function ignore), which is a list.
     ;; There's probably a better way to do this...
     ((and (listp setting)
           (eq (nth 0 setting) 'function)
           (functionp (nth 1 setting)))
      (funcall (nth 1 setting)))

     ;; Just a function: Call it.
     ((or (functionp setting)
          (and (listp setting)
               (eq (nth 0 setting) 'function)
               (functionp (nth 1 setting))))
      (funcall setting))

     ;; If setting is a list, figure out what to do with it before returning.
     ((listp setting)
      (cond ((functionp (nth 0 setting))
             ;; List starts with a function; eval list and return.
             (eval setting))

            ;; Any other things to do with a list?

            ;; Default to just returning the list.
            (t setting)))

     ;; Symbol: Return value of symbol; use lexical scope.
     ((symbolp setting)
      (condition-case-unless-debug err
          (eval setting t)
        ;; If it's void, just return symbol itself.
        (void-variable setting)
        ;; Let other errors through?
        ;; (error 'setting)
        ))

     ;; Else just return it.
     (t
      setting))))
;; (int<taskspace>:config:entry:get/setting '(jeff (+ 4 1) "list function"))
;; (int<taskspace>:config:entry:get/setting '(jeff (4 1)   "just a list"))
;; (int<taskspace>:config:entry:get/setting '(jeff #'ignore "a function"))
;; (int<taskspace>:config:entry:get/setting '(jeff jeff "symbol, no value"))
;; (let ((a 42)) (int<taskspace>:config:entry:get/setting '(jeff a "symbol w/ value")))


(defun int<taskspace>:config:entry:get/docstr (entry)
  "Helper for int<taskspace>:config.

Given an ENTRY from a group's settings alist, returns its docstr or nil."
  (nth 2 entry))


(defun int<taskspace>:config:group:get/settings (group)
  "Helper for int<taskspace>:config.

Get settings from `taskspace:groups' using GROUP as alist key.
Return just the settings - not the full assoc value.
Can return nil."
  ;; Group entry is: (keyword display-name settings)
  ;; Return only settings, or just nil if assoc/nth don't find anything.
  (let ((settings (int<taskspace>:group:settings group)))
    (cond ((listp settings)
           settings)

          ((symbolp settings)
           (eval settings))

          (t
           nil))))
;; (int<taskspace>:config:group:get/settings :default)


(defun int<taskspace>:config:get (key settings)
  "Helper for int<taskspace>:config.

Gets value for KEY from settings. Returns nil if not found.
Returns assoc value if found (key's full entry in SETTINGS alist)."
  (assoc key settings))


;;------------------------------------------------------------------------------
;; Prompt Helpers
;;------------------------------------------------------------------------------

;;------------------------------
;; Prompt: Group
;;------------------------------

(defun int<taskspace>:prompt:group:name (group-assoc)
  "Get displayable groups for prompting user.

GROUP-ASSOC shoud be an entry from `taskspace:groups'.

Returns a cons of:
  - A string containing both the display name and the keyword.
  - The keyword."
  (cons
   ;; Display string first, since that's what `completing-read' shows to user.
   (concat (format "%-10s" (int<taskspace>:group:keyword group-assoc))
           " - "
           (int<taskspace>:group:name/display group-assoc))
   ;; Group keyword second so we can get back to it from user's choice of
   ;; display strings.
   (int<taskspace>:group:keyword group-assoc)))
;; (int<taskspace>:prompt:group:name '(:default "Jeff!" taskspace:group:default))


(defun int<taskspace>:prompt:group:get (choices)
  "Interactive prompt for user to input/choose group name.
Provides both the keyword name and the display string for user to
complete against. Returns keyword name chosen.

CHOICES should be filtered down keyword names from `taskspace:groups'."
  (let ((display-choices (-map #'int<taskspace>:prompt:group:name choices)))
    (alist-get
     (completing-read "Taskspace Group: "
                      ;; Build the names for the options list...
                      display-choices
                      nil
                      ;; Make them confirm if not on the list...
                      'confirm)
     display-choices
     nil nil
     #'string=)))
;; (int<taskspace>:prompt:group:get '((:a "aaa" nil) (:b "b" nil)))


(defun int<taskspace>:prompt:group (&optional auto quiet)
  "Filter groups down to options available to user.

If only one, uses that. If only `:default' available, uses that. If multiple
user groups, prompts user via `int<taskspace>:prompt:group' for which to use.

AUTO can be a few things:
  - nil: No auto-guessing the group unless there is only one non-default.
  - dlv: `int<taskspace>:group:dlv'
  - current: `int<taskspace>:group:current'
  - auto: `int<taskspace>:group:auto'
    - Combines `dlv' and `current', preferring `dlv'.

If QUIET is non-nil, return nil on error instead of raising error signal.

Return group keyword (aka 0th element of entry in `taskspace:groups')."
  ;; `or' will give us either:
  ;;   1) the auto group,
  ;;   2) or the prompted group.
  (or
   ;; 1) Check for an auto-group func to try...
   (cond ((eq nil auto)
          nil)
         ((eq 'auto auto)
          (int<taskspace>:group:auto quiet))
         ((eq 'dlv auto)
          (int<taskspace>:group:dlv))
         ((eq 'current auto)
          (int<taskspace>:group:current quiet))
         ((not (null auto))
          (if quiet
              nil
            (nub:error
                :taskspace
                "int<taskspace>:prompt:group"
              "Unknown AUTO option `%S'"
              auto))))
   ;; 2) No luck on the auto-group... Check the groups and prompt as needed.
   (let ((groups-sans-default
          ;; Filter down to just non-defaults...
          (-filter (lambda (group) (not (eq (int<taskspace>:group:keyword group) :default)))
                   taskspace:groups)))
     ;; Just one group? Return it.
     (cond ((= (length groups-sans-default) 1)
            ;; Get the only group there...
            (nth 0 groups-sans-default))

           ;; Multiple groups? That's what we're actually here for - prompt user!
           ((> (length groups-sans-default) 1)
            (int<taskspace>:prompt:group:get groups-sans-default))

           ;; 0 or less groups.
           (t
            ;; Try to use the default, I guess...
            (int<taskspace>:group :default))))))
;; (int<taskspace>:prompt:group)


;;------------------------------
;; Prompt: Task Name (Description)
;;------------------------------
(defun int<taskspace>:prompt:name (group)
  "Convert minibuffer input into taskspace name.

GROUP should be return value from `int<taskspace>:group' (assoc from
`taskspace:groups').

Prompt in minibuffer for input, read and format to string, then return."
  ;; Replace all whitespace with hyphens.
  (s-replace-regexp (rx (one-or-more whitespace))
                    "-"
                    (read-from-minibuffer
                     (format "New `%s` Task Desc.: "
                             (int<taskspace>:group:name/display group)))))


;;------------------------------
;; Prompt: Choose Existing Task
;;------------------------------

;; Thank you to this thread:
;;   https://emacs.stackexchange.com/questions/32248/how-to-write-a-function-with-an-interactive-choice-of-the-value-of-the-argument
;; I was not finding any usable help/tutorials/documentation
;; for my knowledge/skill level until I found that.
(defun int<taskspace>:prompt:task/existing (group taskspaces &optional display)
  "Prompt user for existing taskspace.

Given a list of taskspaces from e.g. `int<taskspace>:dir:list:date', prompt user
with list of choices, take the user's input, and match back up with an entry in
the list of taskspaces.

GROUP should be return value from `int<taskspace>:group' (assoc from
`taskspace:groups').

DISPLAY can be:
- nil: Pass taskspaces as-is to completion. AKA display as-is.
- nondirectory: Strip each element to `file:name'

Choice is matched back to taskspaces via dumb string matching. First match in
TASKSPACES that substring matches user's choice from `completing-read' is
returned as choice.

Return nil or a string in TASKSPACES."
  ;; Figure out how to display to user first.
  (let (display-names)
    (cond
     ;; nil -> as-is
     ((null display)
      (setq display-names taskspaces))

     ;; nondirectory -> strip each to remove all parent dirs
     ((equal display 'nondirectory)
      (setq display-names (mapcar #'file:name taskspaces)))

     ;; unexpected -> error?
     (t
      (nub:error
          :taskspace
          "int<taskspace>:prompt:task/existing"
        "Unknown display option `%s'"
        display)))

    ;; Give user their choices...
    ;;
    ;; With helm at the wheel, this goes to helm--completing-read-default.
    ;; `confirm' to force completion to one complete choice.
    (let ((choice (completing-read (format "Choose %s: "
                                           (int<taskspace>:group:name/display group))
                                   display-names nil 'confirm)))

      ;; ...and match their choice back up with a taskname.
      (seq-contains taskspaces
                    choice
                    (lambda (input taskname)
                      "Check substring match of user's input against taskname."
                      (string-match-p (regexp-quote input) taskname))))))
;; (int<taskspace>:prompt:task/existing :home (int<taskspace>:dir:list:all :home) 'nondirectory)


;;------------------------------------------------------------------------------
;; Taskspace Internals
;;------------------------------------------------------------------------------

;;------------------------------
;; Groups
;;------------------------------

(defun taskspace:group:dlv (group directory)
  "Create a directory-local-variable for GROUP and DIRECTORY.
This sets the automatic group for that dir (and sub-dirs) to GROUP."
  (if (imp:provided? :dlv)
      (dlv:set directory
               nil ;; global mode
               (list 'int<taskspace>:dlv:group
                     group
                     :safe))
    (nub:error
        :taskspace
        "taskspace:group:dlv"
      "Requires `dlv' feature/package/module; didn't find them. Group: %s, Directory: %s"
      group directory)))


;; TODO: use this to validate group in code places.
(defun int<taskspace>:group:valid? (group)
  "Return non-nil if GROUP is a valid group keyword/name."
  (keywordp group))


(defun int<taskspace>:group:dlv ()
  "Try to get the directory-local-variable `taskspace/dlv/group'.

`taskspace/dlv/group' is not expected to exist, generally.
This will return its value or nil."
  ;; Cannot use `condition-case-unless-debug' here... it just doesn't catch
  ;; `void-variable' signal.
  (condition-case nil
      int<taskspace>:dlv:group
    ;; `int<taskspace>:dlv:group' does not exist; return nil.
    (void-variable nil)
    ;; Generic error signal...?
    ;; (error nil)
    ;; All signals?
    ;; (t nil)
    ))
;; (int<taskspace>:group:dlv)


(defun int<taskspace>:group:current (&optional quiet)
  "Try to figure out current group given currently visited buffer.

Set QUIET to non-nil for nil return on error, else will signal an error.
QUIET also suppresses the \"Current Taskspace Group: ...\" message."
  ;; `int<taskspace>:path:current' can return nil, so make sure to account for it.
  (let ((path (int<taskspace>:path:current))
        (current-root nil)
        (current-group nil))

    ;; Do we have enough to get started?
    (when path
        ;; Search through our groups for something to match to that.
        (setq current-root
              (car
               ;; Start by getting our group keywords...
               (->> (-map #'car taskspace:groups)
                    ;; and turning into task and notes dirs...
                    (-map (lambda (g) (list
                                       (int<taskspace>:config g :dir/tasks)
                                       (int<taskspace>:config g :dir/notes))))
                    ;; Have list of tuple lists now; want flat list.
                    (-flatten)
                    ;; Figure out which one is our current root.
                    (-map (lambda (root)
                            ;; If a child or the same dir as root, keep root.
                            ;; Otherwise return nil for a "nope, not this one".
                            (if (or
                                 (path:descendant? path root)
                                 (path:equal? path root))
                                root
                              nil)))
                    ;; Reduce down to non-nil answer.
                    ;; Assumes there is only one non-nil answer.
                    (-remove #'null)))))

    ;; How'd we do?
    (if (not (stringp current-root))
        ;; Complain if interactive; return nil if not.
        (if quiet
            nil
          (nub:error
              :taskspace
              "int<taskspace>:group:current"
            "Could not find a taskspace root for currently visited buffer dir: %s"
            path))

      ;; Normalize the path.
      (setq current-root (path:absolute:dir current-root))

      ;; Now, finally... We're not done.
      ;; Got to translate some taskspace or notes root dir into its group.
      (setq current-group
            (car ;; Not sure what to do if there's more than one choice left...
             ;; Reduce down to whatever is non-nil.
             (-remove
              #'null
              (-map (lambda (entry)
                      ;; If we match one of this group's roots, return the
                      ;; group keyword.
                      (if (or
                           (path:equal? (int<taskspace>:config (nth 0 entry)
                                                               :dir/tasks)
                                        current-root)
                           (path:equal? (int<taskspace>:config (nth 0 entry)
                                                               :dir/notes)
                                        current-root))
                          (nth 0 entry)
                        nil))
                    taskspace:groups))))
      ;; Inform it and return it.
      (unless quiet
          (message "Current Taskspace Group: %s" current-group))
      current-group)))
;; (int<taskspace>:group:current)


(defun int<taskspace>:group:auto (&optional quiet)
  "Try to get either the auto-group or the current-group.

Prefer the auto-group.

Set QUIET to non-nil for nil return on error, else will signal an error.
QUIET also suppresses the \"Current Taskspace Group: ...\" message."
  (or (int<taskspace>:group:dlv)
      (int<taskspace>:group:current quiet)))
;; (int<taskspace>:group:auto)


;;------------------------------
;; Group Getters
;;------------------------------

(defun int<taskspace>:group (group)
  "Return the `assoc' from `taskspace:groups' for GROUP.

If GROUP is a keyword, get the assoc and then return it.
If GROUP is a list, assume it is already the assoc list and return it.
Else signal an error."
  ;; Keyword? Get the assoc for that group.
  (cond ((keywordp group)
         (assoc group taskspace:groups))

        ;; List? Assume it's already the group assoc from `taskspace:groups',
        ;; and return it.
        ((listp group)
         group)

        (t
         (nub:error
             :taskspace
             "int<taskspace>:group"
           "Cannot understand type '%S'; need a keyword or list! Group: %S"
           (type-of group)
           group))))
;; (int<taskspace>:group :default)
;; (int<taskspace>:group '(:default "Defaults" taskspace:group:default))


(defun int<taskspace>:group:keyword (group)
  "Given GROUP keyword/list, return group's keyword.

GROUP should be return value from `int<taskspace>:group' (assoc from
`taskspace:groups')."
  (nth 0 (int<taskspace>:group group)))
;; (int<taskspace>:group:keyword '(:default "Taskspace of Default" this-dne))


(defun int<taskspace>:group:name/display (group)
  "Given GROUP keyword/list, return GROUP's display name.

GROUP should be either a keyword or the return value from `int<taskspace>:group'
\(assoc from `taskspace:groups')."
  (let ((group-list (int<taskspace>:group group)))
    (if (null (nth 1 group-list)) ;; Does the group even have a display name?
        (symbol-name (int<taskspace>:group:keyword group-list))
      (nth 1 group-list))))
;; (int<taskspace>:group:name/display '(:default "Taskspace of Default" this-dne))
;; (int<taskspace>:group:name/display '(:default nil this-dne))
;; (int<taskspace>:group:name/display :default)


(defun int<taskspace>:group:settings (group)
  "Given GROUP symbol/list, return group's settings.

GROUP should be return value from `int<taskspace>:group' (assoc from
`taskspace:groups')."
  (nth 2 (int<taskspace>:group group)))
;; (int<taskspace>:group:settings '(:default "Taskspace of Default" this-dne))
;; (int<taskspace>:group:settings :default)


;;------------------------------
;; Notes
;;------------------------------

(defun int<taskspace>:notes:open (group taskpath &optional no-error no-message)
  "Open the GROUP:TASKPATH's notes file.

If NO-ERROR, returns nil instead of raising an error signal.

If NO-MESSAGE, skips output message."

  ;; Ok - find and open the notes file. Don't want to just assume they are
  ;; where the settings now indicate new ones will be created; people can
  ;; change their settings. Old ones could be :self-contained and new
  ;; :notesless, for example.

  ;; Assume local/:self-contained first.
  (setq notespath (int<taskspace>:path:notes group taskpath :self-contained))

  ;; Does it exist?
  (unless (path:exists? notespath :file)
    ;; Nope; try remote/:noteless.
    (setq notespath (int<taskspace>:path:notes group taskpath :noteless)))

  ;; There is no third try.
  (if (not (path:exists? notespath :file))
      (if no-error
          nil
        (nub:error
            :taskspace
            "int<taskspace>:notes:open"
          "No notes file found! Look for it: %s, %s"
          (int<taskspace>:path:notes group taskpath :self-contained)
          (int<taskspace>:path:notes group taskpath :noteless)))

    ;; Exists; message and visit it.
    (unless no-message
      (message "Opening taskspace notes: %s" notespath))
    (find-file notespath)))


;;------------------------------
;; Paths
;;------------------------------

(defun int<taskspace>:path:current ()
  "Return correct 'current filepath' for both Dired mode and not."
  (cond ((equal major-mode 'dired-mode)
         default-directory)

        ((buffer-file-name)
         (path:parent (buffer-file-name)))

        (t
         nil)))


(defun int<taskspace>:path:notes (group taskpath &optional type/notes)
  "Given GROUP and TASKPATH, generate the notes file path.

If TYPE/NOTES is `:self-contained' or `:noteless', this will ignore the config
settings for the group and return based on TYPE/NOTES.

Otherwise, it will check GROUP's config settings for `:type/notes' and
build the notes file path based on that."
  ;; Get filename from config.
  (let ((filename (int<taskspace>:config group :file/notes))
        ;; Use type/notes if a valid value, else get from config.
        (type/notes (if (or (eq type/notes :self-contained)
                            (eq type/notes :noteless))
                        type/notes
                      (int<taskspace>:config group :type/notes))))

    ;; Build output based on what type we figured out.
    (cond ((eq type/notes :self-contained)
           ;; :self-contained notes filepath is just filename tacked
           ;; on to taskpath.
           (path:absolute:file taskpath filename))

          ((eq type/notes :noteless)
           ;; Remote file name is different - you want the task name in it so
           ;; the remote notes folder makes any sense on its own.
           (path:absolute:file
            ;; The remote notes dir from the group's config to get notespath:
            (int<taskspace>:config group :dir/notes)
            ;; And Remote File Name is:
            (concat
             ;; Task Name
             (file:name taskpath)
             ;; Plus a dot...
             "."
             ;; Plus filename, sans 'sort to top' stuff...
             (string-trim filename "_" "_")))))))
;; (int<taskspace>:path:notes :home "c:/2020-08-24_11_jeff/" :self-contained)
;; (int<taskspace>:path:notes :home "c:/2020-08-24_11_jeff/" :noteless)
;; (int<taskspace>:path:notes :home "c:/2020-08-24_11_jeff/" :jeff)
;; (int<taskspace>:path:notes :home "c:/2020-08-24_11_jeff/")


(defun int<taskspace>:path:generate (group taskpath filename)
  "Generate a file path for FILENAME and TASKPATH in GROUP.

This can be outside of the taskspace for e.g. :noteless taskspaces - the note
file will be elsewhere."
  (if (not (string= filename (int<taskspace>:config group :file/notes)))
      ;; Non-note files just go in taskspace...
      (path:absolute:file taskpath filename)

    ;; Notes files may or may not go in taskspace. Find out.
    (if (eq (int<taskspace>:config group :type/notes)
            :self-contained)
        ;; Local file name is just provided name.
        (path:absolute:file taskpath filename)

      ;; Remote file name could be different - may want task name in it.
      (path:absolute:file (int<taskspace>:config group :dir/notes)
                          (concat ;; remote file name:
                           ;; Task Name
                           (file:name taskpath)
                           ;; Plus a dot...
                           "."
                           ;; Plus filename, sans 'sort to top' stuff...
                           (string-trim filename "_" "_"))))))
;; (int<taskspace>:path:generate :default "c:/2020-20-20_20_jeff" "_notes.org")
;; (int<taskspace>:path:generate :default "c:/2020-20-20_20_jeff" "jeff.data")


;;------------------------------
;; Files
;;------------------------------

(defun int<taskspace>:file:generate (group taskpath file-alist)
  "Generate each file in FILE-ALIST into the new taskpath.

GROUP should be return value from `int<taskspace>:group' (assoc from
`taskspace:groups').

Expect FILE-ALIST entries to be:
  (filename . string-or-func)

Create 'filename' in TASKPATH and then insert string into it, or use func to
generate contents. Does not currently support directory structures/trees.

Return nil or an alist of errors.
Errors alist is all files not generated, where each assoc in errors alist is:
  (filename . 'reason')"

  ;; let it just do nothing when empty list
  (let (errors-alist ;; empty return value alist
        ;; Get taskname from path to supply to any file content gen funcs.
        (taskname (file:name taskpath)))
    (dolist (entry file-alist errors-alist)
      (let* ((file (file:name (eval (cl-first entry))))
             (filepath (int<taskspace>:path:generate group taskpath file))
             (str-or-func (cl-second entry)))

        (cond
         ;; ERROR: already exists...
         ((path:exists? filepath)
          (push `(,filepath . "file already exist") errors-alist))

;;         ;; ERROR: generator not bound
;;         ((not (boundp str-or-func))
;;          (push `(,filepath . "string/function not bound") errors-alist))

         ;; ERROR: unknown generator
         ((and (not (stringp str-or-func))
               (not (functionp str-or-func)))
          (push `(,filepath . ,(format "generator is not string or function: %s"
                                       str-or-func))
                errors-alist))

         ;; HAPPY!
         (t
          (with-temp-file filepath
            (if (stringp str-or-func)
                (insert str-or-func)
              ;; Call with group so users can have a function for multiple
              ;; groups if applicable...
              (insert (funcall str-or-func group taskname taskpath))))))

        ;; ;; If made a remote notes file, make a .taskspace config now.
        ;; (when (and (string= file (int<taskspace>:config group :file/notes))
        ;;            (not (path:child? filepath taskpath)))
        ;;   (taskspace/with/config taskpath
        ;;     (setq taskspace/config
        ;;           (taskspace/config/set :notes filepath taskspace/config))
        ;;     (taskspace/config/write taskspace/config taskpath)))

        ;; dolist returns the errors
        ))))


(defun int<taskspace>:file:copy (taskpath &rest filepaths)
  "Copy each of the files in FILEPATHS to TASKPATH.

Expect well-qualified filepaths (absolute, relative, or otherwise). Does not
currently support directory structures/trees.

Return nil or an errors alist.
  - Errors alist, where each assoc in errors alist is:
    (filepath . 'reason')"
  ;; let it just do nothing when empty list
  (let (errors-alist) ;; empty return value alist
    (dolist (path filepaths errors-alist)
      (cond
       ;; ERROR: can't find or...
       ((not (path:exists? path))
        (push `(,path . "file does not exist") errors-alist))
       ;; ERROR: can't read file or...
       ((not (path:readable? path))
        (push `(,path . "file is not readable") errors-alist))
       ;; ERROR: not a file (dir or symlink or something)
       ((not (path:exists? path :file))
        (push `(,path . "path is not a file") errors-alist))

       ;; HAPPY: copy it
       (t
        (copy-file path ;; from "the full path of where it is" to...
                   ;; taskpath + "the filename part of where it is"
                   (path:absolute:file taskpath (file:name path))))

       ;; dolist returns the errors
       ))))


;;------------------------------
;; Directories
;;------------------------------

(defun int<taskspace>:dir:create (group description date-arg)
  "Create a taskspace directory.

GROUP should be return value from `int<taskspace>:group' (assoc from
`taskspace:groups').

DATE-ARG must be nil, 'today (for today), or an number for a relative day.

Directory name is formatted with DESCRIPTION, date, and (monotonically
increasing) serial number."
  ;; Make sure basic folders exist.
  (unless (path:exists? (int<taskspace>:config group :dir/tasks) :dir)
    (message "Taskspace: Making root directory... %s"
             (int<taskspace>:config group :dir/tasks))
    (make-directory (int<taskspace>:config group :dir/tasks)))
  (unless (path:exists? (int<taskspace>:config group :dir/notes) :dir)
    (message "Taskspace: Making remote notes directory... %s"
             (int<taskspace>:config group :dir/notes))
    (make-directory (int<taskspace>:config group :dir/notes)))

  ;; Get today's date.
  (let* ((date (int<taskspace>:naming:get:date group date-arg))
         ;; Get today's dirs.
         (date-dirs (int<taskspace>:dir:list:date group date))
         ;;   - figure out index of this one
         (number (int<taskspace>:naming:get:number group date-dirs))

         ;; Build dir string from all that.
         (dir-name (int<taskspace>:naming:make group date number description))
         (dir-full-path (path:absolute:dir (int<taskspace>:config group :dir/tasks)
                                           dir-name)))

    ;; TODO: taskspace debugging func.
    (message "create-dir: %s %s %s %s" date date-dirs number dir-name)
    (message "create dir: %s" dir-full-path)

    ;; Only create if:
    ;;   - valid description input and
    ;;   - no dupes or accidental double creates
    ;;   - it doesn't exist (this is probably redundant if verify-description
    ;;     works right)
    (if (and (int<taskspace>:naming:verify group description)
               (not (cl-some (lambda (x) (int<taskspace>:dir= group
                                                description
                                                x
                                                'description))
                          date-dirs))
               (not (path:exists? dir-full-path)))
        ;; Make it.
        (progn

          ;; make-directory helpfully has no data on what it returns or why or when
          ;; or anything. But it returns nil on success so... super useful guys.
          (make-directory dir-full-path)

          ;; How about we report something actually useful maybe?
          ;; Full path of created dir on... success?
          ;; Nil on folder non-existance.
          (if (path:exists? dir-full-path)
              dir-full-path
            nil))

      ;; Failed check; return nil.
      nil)))
;; (int<taskspace>:dir:create :work "testcreate" nil)


(defun int<taskspace>:dir= (group name dir part)
  "Is NAME equal to a certain PART of DIR?

GROUP should be return value from `int<taskspace>:group' (assoc from
`taskspace:groups').

PART should be one of: 'date 'number 'description

Return nil/non-nil."
  ;; don't accept nulls
  (unless (or (null name) (null dir) (null part))
    ;; strip dir down to file name and
    ;; strip file name down to part (if non-nil part)
    (let* ((dir-name (file:name dir))
           (dir-part (int<taskspace>:naming:split group dir-name part)))
      (if (null dir-part)
          nil ;; don't accept nulls
        ;; else, usable data
        ;; check against input name
        (string= name dir-part)))))
;; (int<taskspace>:dir= :home "2000" "c:/zort/troz/2000_0_testcase" 'date)


(defun int<taskspace>:dir:list:all (group)
  "List all children directories in a taskspace.

GROUP should be return value from `int<taskspace>:group' (assoc from
`taskspace:groups').

Get children directories of taskspace/dir, ignoring:
  (int<taskspace>:config group :dir/tasks/ignore)."
  (let (task-dirs) ;; empty list for return value
    ;; loop on each file in the directory
    (dolist (file
             (path:children (int<taskspace>:config group :dir/tasks) :absolute-paths)
             task-dirs)
      (when (and (path:exists? file :dir) ;; Only want dirs and...
                 (not (member ;; ignore things in ignore list
                       (file:name file)
                       (int<taskspace>:config group :dir/tasks/ignore))))
        (push file task-dirs)))
    ;; dolist returns our constructed list since we put it as `result'
    ;; so we're done
  ))
;; (message "%s" (int<taskspace>:dir:list:all :home))


;; Get all, pare list down to date-str, return.
(defun int<taskspace>:dir:list:date (group date-str)
  "Get any/all taskspaces for today.

GROUP should be return value from `int<taskspace>:group' (assoc from
`taskspace:groups').

DATE-STR should be a string of the date."
  (unless (null date-str)
    (let ((task-dirs (int<taskspace>:dir:list:all group))
          date-dirs) ;; return val
      (dolist (dir task-dirs date-dirs)
        (when (int<taskspace>:dir= group date-str dir 'date)
          (push dir date-dirs))))))
;; (int<taskspace>:dir:list:date :home "2020-03-13")
;; (int<taskspace>:dir:list:date :work "2020-08-26")


;;------------------------------
;; Taskspace Naming
;;------------------------------

(defun int<taskspace>:naming:get:number (group dir-list)
  "Check dirs in DIR-LIST, returns highest number part + 1.

GROUP should be return value from `int<taskspace>:group' (assoc from
`taskspace:groups')."
  ;; Next number is one more than...
  (1+
   ;; The max of map/reduce shenanigans (or just -1 if given no dirs).
   (-max
       (or
        (->>
         ;; First, need to change from paths to just the name of the dirs.
         (-map #'file:name dir-list)
         ;; Now pare down to just numbers.
         (-map (lambda (dir) (int<taskspace>:naming:split group dir 'number)))
         ;; Filter out any nils; don't matter to us.
         (-remove #'null)
         ;; string -> int
         (-map #' string-to-number))

        ;; fallback list: negative 1 so we return zero.
        '(-1)))))
;; (int<taskspace>:naming:get:number :work (int<taskspace>:dir:list:date :work "2020-08-26"))
;; (int<taskspace>:naming:get:number :work (int<taskspace>:dir:list:date :work "2020-08-26"))
;; (int<taskspace>:naming:get:number :default '("zort/troz/2000_0_baz"))
;; (int<taskspace>:naming:get:number :default '())
;; (int<taskspace>:naming:get:number :default
;;                       '("zort/troz/2000_0_baz" "zort/troz/2000_qux"
;;                         "zort/troz/2000_qux_jeff" "zort/troz/2000_8_quux"))


(defun int<taskspace>:naming:get:date (group arg)
  "Return a date in the correct string format.

ARG must be nil or 'today (for today), or numberp.

GROUP should be return value from `int<taskspace>:group' (assoc from
`taskspace:groups').

Return date requested by ARG, or nil."
  (let ((day nil))
    ;; check/convert input arg
    (cond ((null arg)
           ;; nil -> today -> 0
           (setq day 0))

          ((numberp arg)
           ;; if arg numberp: 0 today, negative before, positive after
           (setq day arg))

          ((string= arg 'today)
           ;; 'today -> 0
           (setq day 0))

          ;; error case(s): nil
          (t
           (setq day nil)))

    (unless (eq day nil)
      (let* ((now (current-time)) ;; right now
             (now-adjust-secs (* day 24 60 60)) ;; day arg to seconds
             (target (time-add now now-adjust-secs))) ;; actually when we want
        ;; format to spec and return
        (format-time-string (int<taskspace>:config group :format/datetime)
                            target)))))
;; Examples/Tests:
;;                 Today: (int<taskspace>:naming:get:date :default nil)
;;            Also Today: (int<taskspace>:naming:get:date :default 'today)
;; Today Too... I guess?: (int<taskspace>:naming:get:date :default "today")
;;             Not Today: (int<taskspace>:naming:get:date :default -1)
;;             Not Today: (int<taskspace>:naming:get:date :default 1.9)
;;                 Error: (int<taskspace>:naming:get:date :default "jeff")
;;                 Error: (int<taskspace>:naming:get:date :default 'jeff)


(defun int<taskspace>:naming:verify (group name)
  "Verify that NAME is an allowable part of the directory name.

GROUP should be return value from `int<taskspace>:group' (assoc from
`taskspace:groups').

Return nil/non-nil."

  ;; Sanity check 1: `name' must be a valid filename, for a very loose
  ;;                 definition of valid.
  ;; Sanity check 2: Not a path sep in there?
  ;; Valid check:    Verify name obeys my regexp.
  (let ((matched-invalid (string-match file-name-invalid-regexp name))
        (dir-sep-check (file:name name))
        (valid-name (string-match (int<taskspace>:config group :naming/description/rx/valid) name)))

    ;; Check for bad input, fail if so... Bad if:
    ;;   - DOES match /invalid/ filename regexp
    (if (or matched-invalid
            ;; - or non-dir name DOES NOT match input name
            (not (string= name dir-sep-check))
            ;; - or DOES NOT match /valid/ name regexp
            (null valid-name))
        ;; Just return nil for fail.
        nil

      ;; else... Ok name. Do something?

      ;; Verify they didn't try to give us the whole thing? (check for date?)
      ;; (Eh... Not gonna bother right now.)

      ;; return input when valid
      name)))
;; weird name: (int<taskspace>:naming:verify :default "\0")
;; too short:  (int<taskspace>:naming:verify :default "0")
;; good!:      (int<taskspace>:naming:verify :default "hello-there")
;; dir sep:    (int<taskspace>:naming:verify :default "hello-there/here")
;; (int<taskspace>:naming:verify :home "testing-testing")


(defun int<taskspace>:naming:make (group date number description)
  "Create a full name from imputs.

Name created obeys first formatting order found in parts-alists.

GROUP should be return value from `int<taskspace>:group' (assoc from
`taskspace:groups').

DATE should be a date stirng.

NUMBER should be the day's monotonically increasing serial number.

DESCRIPTION should be a string."
  ;; How long is the parts-alist we're looking for?
  ;;   - Stringify each (don't want nulls here...)
  (let* ((name-parts (seq-map (lambda (x) (format "%s" x))
                              ;; But take out nulls?
                              (seq-remove #'null
                                          ;; turn inputs into list
                                          (list date number description))))
         (name-len (length name-parts))
         split-alist)

    ;; find the right alist for building the dir string
    (dolist (alist (int<taskspace>:config group :naming/parts-alists) split-alist)
      (when (= name-len (length alist))
        (setq split-alist alist)))

    ;; (message "make-name: %s->%s %s %s null?%s"
    ;;          name-parts (seq-remove #'null name-parts)
    ;;          name-len
    ;;          split-alist (null split-alist))

    (unless (null split-alist)
      (mapconcat #'identity (seq-remove #'null name-parts)
                 (int<taskspace>:config group :naming/separator)))))
;; (int<taskspace>:naming:make :default "2000" "1" "hi")
;; (int<taskspace>:naming:make :default "2000" nil "hi")
;; (int<taskspace>:naming:make :default "hi" nil nil)
;; (int<taskspace>:naming:make :default "2019-05-14" 0 "testcreate")


;; util to split up dir name and then give desired bit back
;;  - should work for manually made ones that don't have the middle <#> part
(defun int<taskspace>:naming:split (group name part)
  "Split name based on taskspace naming/separator rules.

GROUP should be return value from `int<taskspace>:group' (assoc from
`taskspace:groups').

NAME should just be directory name; do not use path.

Return the requested PART. PART can be one of: 'date 'number 'description"
  (unless (or (null name) (null part))
    ;; unless or if/error?
    (let* ((split-name (split-string name
                                     (int<taskspace>:config group
                                                        :naming/separator)))
           (len-split (length split-name))
           split-alist)

      ;; find the right alist for parsing the split dir string
      (dolist (alist (int<taskspace>:config group :naming/parts-alists) split-alist)
        (when (= len-split (length alist))
          (setq split-alist alist)))

      ;; now try to pull out part requested
      (if (not (assoc part split-alist))
          nil ;; they requested something invalid for this `name'

        ;; figure out what index is desired,
        ;; then pull out the desired string (and return it)
        (nth (cdr (assoc part split-alist)) split-name)))))
;; (int<taskspace>:naming:split :home "2020-03-13_0_container-couchbase" 'date)
;; (int<taskspace>:naming:split :default "2000_0_zort" 'date)
;; (int<taskspace>:naming:split :default "2000_0_zort" nil)
;; (int<taskspace>:naming:split :default "2000_0_zort" 'number)
;; (int<taskspace>:naming:split :default "2000_zort" 'number)


;;------------------------------
;; Org-Mode Helpers
;;------------------------------

(defun int<taskspace>:org:keywords:list (&optional to-lower)
  "Get keyword elements from this org document.

Elements (return value) will be an alist of (key . value).

'Keyword elements' are lines like this in `org-mode' files:
#+PROPERTY: value

If TO-LOWER is not nil, converts all keys to lowercase. DOES NOT CHANGE VALUES!"
  ;; map func to elements...
  (org-element-map
      (org-element-parse-buffer 'element) ;; parse this buffer at 'element level
      'keyword ;; we only care about keywords
    ;; for each keyword element, get it's key and value into the return.
    (lambda (keyword) (cons
                       (if to-lower
                           (downcase (org-element-property :key keyword))
                         (org-element-property :key keyword))
                       (org-element-property :value keyword)))))


(defun int<taskspace>:org:keyword:get (keyword &optional to-lower)
  "Get the specified KEYWORD from this `org-mode' document.

Will be case insensitive if TO-LOWER is non-nil.

If there are more than one, it will return whatever is first.

'Keyword elements' are lines like this in `org-mode' files:
#+PROPERTY: value

So in the non-nil TO-LOWER case, we will return 'value' if asked for:
  'PROPERTY', 'property', 'PrOpeRtY', etc..."
  (alist-get (if to-lower
                 (downcase keyword)
               keyword)
             (int<taskspace>:org:keywords:list to-lower)
             nil nil
             #'string=))


;;------------------------------
;; Kill Ring (Copy/Paste)
;;------------------------------

(defun int<taskspace>:kill-and-return (string &optional msg msg-args)
  "Copy STRING to kill ring and also return STRING.

Optionally output MSG via `message' with MSG-ARGS."
  ;; copy to kill-ring
  (kill-new string)
  ;; say what we did
  (message msg msg-args)
  ;; return it
  string)
;; (int<taskspace>:kill-and-return "hello")


;;----------------------------------Taskspace-----------------------------------
;;--                          Interactive Commands                            --
;;------------------------------------------------------------------------------

;;;###autoload
(defun taskspace:dwim:name ()
  "DWIM with the taskspace in order to save task name to kill ring and return it.

Create if none.
Return if just the one.
Prompt to choose from multiple."
  (interactive)

  ;; Get task's full path, reduce to just task directory...
  (let* ((fullpath (call-interactively #'taskspace:dwim:dir))
         (taskname (file:name fullpath)))

    ;; copy to kill-ring
    (kill-new taskname)

    ;; return it
    taskname))
;; (taskspace:dwim:name)
;; M-x taskspace:dwim:name


;;;###autoload
(defun taskspace:dwim:dir (date-input)
  "DWIM with taskspace in order to save task dir to kill ring and return it.

DATE-INPUT is prefix arg:
  - No prefix arg means today.
  - Prefix arg means tomorrow.

If in an `org-mode' doc with a taskspace keyword defined and if the directory it
specifices exists:
  - Return the full path'd version of that `org-mode' keyword's value.

Else:
  - Create if none.
  - Return if just the one.
  - Choose from multiple."
  ;; Numeric arg but don't let lower case "p" auto-magic nothing (no prefix arg)
  ;; into 1. Nothing/0/nil is today. 1 is tomorrow.
  (interactive (list current-prefix-arg))

  ;; Try to get group from context.
  (let ((group (int<taskspace>:group:auto t))
        task-dir-shortcut
        task-msg-shortcut)

    ;; Prompt for group if we couldn't guess it.
    (unless group
      (setq group (int<taskspace>:prompt:group 'auto t)))

    ;; Check for a taskspace keyword if we're in an org-mode buffer. Just use
    ;; that and skip the rest if we find it and it's a directory that exists and
    ;; stuff.
    (when (eq major-mode 'org-mode)
      (let ((task-dir (int<taskspace>:org:keyword:get
                       (int<taskspace>:config group :dir/tasks/org/keyword))))
        (when (and (not (null task-dir))
                   (path:exists? task-dir :dir))
          (setq task-dir-shortcut (path:canonicalize:dir task-dir))
          (setq task-msg-shortcut
                (format "Got Taskspace from org-mode keyword (#+%s). %s"
                        (int<taskspace>:config group :dir/tasks/org/keyword)
                        task-dir-shortcut)))))

    ;; Do we have a shortcut out, or do we go looking for the task-dir?
    (if (not (null task-dir-shortcut))
        (int<taskspace>:kill-and-return task-dir-shortcut task-msg-shortcut)

      ;; No short cut. Start looking at DWIM things in DWIM-y order...

      ;; Default to "today" if date-input isn't parsable string,
      ;; then get date, taskspaces, etc. for that numerical relative day.
      (let* ((date-input (cond
                          ;; no date-input is today is 0
                          ((null date-input) 0)
                          ;; strings should be converted to numbers
                          ((stringp date-input)
                           (string-to-number date-input))
                          ;; just allow numbers through unscathed
                          ((numberp date-input) date-input)
                          ;; default to... today/0 I guess?
                          (t 0)))
             (date (int<taskspace>:naming:get:date group date-input))
             (taskspaces (int<taskspace>:dir:list:date group date))
             (length-ts (length taskspaces)))

        (cond
         ;; error out if we have no idea what date to dwim with...
         ((null date)
          (nub:error
              :taskspace
              "taskspace:dwim:dir"
            "Date string is nil: %s"
            date))

         ;; if none, create one.
         ((null taskspaces)
          ;; call-interactively will give user prompt for description,
          ;; etc. as if they called themselves.
          (funcall #'taskspace:create
                                 group
                                 (int<taskspace>:prompt:name group)))

         ;; If just one, return it.
         ;; How to create second in that case? Use a non-dwim create func?
         ;;   - I think yes.
         ((= length-ts 1)

          ;; copy & return
          (int<taskspace>:kill-and-return (cl-first taskspaces)
                                     "Existing taskspace: %s"
                                     (cl-first taskspaces)))

         ;; For now, only give existing choices. User can use a non-dwim create
         ;; func if they want new.
         ((> length-ts 1)

          ;; list available choices to user, get the taskspace they chose
          (let ((choice (int<taskspace>:prompt:task/existing group
                                                taskspaces
                                                'nondirectory)))
            (int<taskspace>:kill-and-return choice
                                       "Chose taskspace: %s"
                                       choice)))

         ;; Don't need a default case... Fall through with nil.
         ;;(t nil)
         )))))
;; M-x taskspace:dwim:dir
;; (taskspace:dwim:dir)
;; (taskspace:dwim:dir -1 :home)


;;;###autoload
(defun taskspace:create (group description)
  "Create a new taskspace for today with the supplied GROUP & DESCRIPTION."
  ;; Do we need a max len? Leaving out until I feel otherwise.
  (interactive
   (let ((group-prompt (int<taskspace>:prompt:group 'auto t)))
     (list group-prompt
           (int<taskspace>:prompt:name group-prompt))))

  ;; Is DESCRIPTION ok as description part?
  (if (not (int<taskspace>:naming:verify group description))
      ;; fail w/ message and return nil?
      ;; (progn
      ;;   (message "Invalid description: %s" description)
      ;;   nil)
      ;; Trying out just erroring out instead.
      ;; We are up to the interactive level now.
      (nub:error
          :taskspace
          "taskspace:create"
        "Invalid description: %s"
        description)

    ;; Create the dir/project for today.
    (let ((taskpath (int<taskspace>:dir:create group description 'today)))
      (if (null taskpath)
          ;; Couldn't create it for some reason...
          ;; TODO: Better reasons if known. "already exists" would be nice for
          ;; that case.
          (nub:error
              :taskspace
              "taskspace:create"
            "Error creating taskspace directory for: %s"
            description)

        ;; Copy files into new taskspace.
        (when (path:exists? (int<taskspace>:config group :file/new/copy) :dir)
          (apply #'int<taskspace>:file:copy
                 ;; arg 1: our new taskpath
                 taskpath
                 ;; arg &rest: all the files to copy with:
                 ;;   - full path name
                 ;;   - no dot files
                 ;;     - no '.', '..'
                 ;;     - yes actual dotfiles somehow?
                 ;;     - This is what I want, so... ok.
                 (path:children (int<taskspace>:config group :file/new/copy)
                                :absolute-paths
                                nil
                                path:rx:dirs:not-parent-or-current-dot)))

        ;; Generate files into new taskspace.
        (when (int<taskspace>:config group :file/new/generate)
          (let ((gen-errors (int<taskspace>:file:generate
                             group
                             taskpath
                             (int<taskspace>:config group :file/new/generate))))
            (when gen-errors
              (nub:error
                  :taskspace
                  "taskspace:create"
                "Taskspace file generation errors: %s"
                gen-errors))))

        ;; Either of those can put a projectile file into the taskspace.
        ;; Just name it: .projectile
        ;;   See: https://projectile.readthedocs.io/en/latest/projects/#ignoring-files

        ;; Can also put skeleton org file. Or just org file with yasnippet ready
        ;; to go...

        ;; Copy taskpath to kill-ring.
        (kill-new taskpath)
        ;; Say something.
        (message "Created taskspace: %s" (file:name taskpath))
        ;; (message "Created taskspace: %s" taskpath)

        ;; Open the notes file.
        (int<taskspace>:notes:open group taskpath nil t)

        ;; Return the path.
        taskpath))))
;; M-x taskspace:create
;; (taskspace:create "testing-create")


;;;###autoload
(defun taskspace:dired:task ()
  "Open the current taskspace's root dir in an Emacs (Dired?) buffer.

If in a :noteless file, go to that note's task dir, if possible.
If in a file or sub-dir of the task dir, go to the task's dir."
  (interactive)

  (let* ((group (or (int<taskspace>:group:auto t)
                    (int<taskspace>:prompt:group 'auto t)))
         (taskpath (int<taskspace>:org:keyword:get
                    (int<taskspace>:config group :dir/tasks/org/keyword))))

    ;; Can short-cut past this if we were in an org file and found the keyword
    ;; link to the task's dir.
    (unless taskpath
      ;; Deduce group or prompt for it.
      (let ((path (int<taskspace>:path:current))
            (root (int<taskspace>:config group :dir/tasks)))

        (setq taskpath
              ;; If we are in a dir/file under a task, get the topmost dir that
              ;; isn't the root.
              (cond ((path:descendant? path root)
                     ;; Go up from current until we get to direct child.
                     (let ((path/curr path)
                           child/found?)
                       (while (and path/curr
                                   (not child/found?))
                         (setq path/curr (path:parent path/curr)
                               child/found? (path:child? path/curr root)))
                       path/curr))

                    ;; If we are in a remote notes file, dunno.
                    ;;   *shrug* Fallthrough to 't.

                    ;; If we are elsewhere, really dunno.
                    (t
                     nil)))))

    (if (not (path:exists? taskpath :dir))
        ;; Not a dir - error out.
        (nub:error
            :taskspace
            "taskspace:dired:task"
          "'%s' taskspace directory doesn't exist?: '%s'"
          group taskpath)

      ;; Ok - message and open (probably in dired but let emacs decide).
      (find-file taskpath)
      (message "Opening '%s' taskspace directory: %s"
               group
               (file:name taskpath))
      ;; Return the task's path?
      taskpath)))
;; (taskspace:dired:task)
;; M-x taskspace:dired:task


;;;###autoload
(defun taskspace:dired:root (group)
  "Open the root directory of GROUP's taskspaces in an Emacs (Dired?) buffer."
  (interactive (list (int<taskspace>:prompt:group 'auto t)))

  (if (not (file:exists? (int<taskspace>:config group :dir/tasks) :dir))
      ;; not a dir - error out
      (nub:error
          :taskspace
          "taskspace:dired:root"
        "Can't find taskspace root directory: '%s'"
        (int<taskspace>:config group :dir/tasks))

    ;; ok - message and open (probably in dired but let emacs decide)
    (find-file (int<taskspace>:config group :dir/tasks))
    ;; say something
    (message "Opening taskspace parent: %s"
             (file:name (int<taskspace>:config group :dir/tasks)))
    ;; return the top dir?
    (int<taskspace>:config group :dir/tasks)))
;; (taskspace:dired:root :home)
;; M-x taskspace:dired:root


;;;###autoload
(defun taskspace:shell (group)
  "Open GROUP's root taskspace directory in an Emacs shell buffer.

Shell opened can be set by modifying:
  (int<taskspace>:config group :function/shell)."
  ;; §-TODO-§ [2020-08-18]: If in a taskspace folder or a remote notes file,
  ;; just let the shell open without prompts.
  (interactive (list (int<taskspace>:prompt:group 'auto t)))

  (let ((shell-fn (int<taskspace>:config group :function/shell)))
    (if (not (functionp shell-fn))
        (nub:error
            :taskspace
            "taskspace:shell"
          "`:function/shell' in taskspace settings for group `%S' is not bound to a fuction: %s"
          shell-fn)

      ;; prompt user for the taskspace with an attempt at DWIM
      (let ((task (call-interactively #'taskspace:dwim:dir)))
        ;; expecting a path from task-dir/dwim
        (if (not (path:exists? task :dir))
            ;; not a dir - error out
            (nub:error
                :taskspace
                "taskspace:shell"
              "Can't find taskspace (not a directory?): '%s'"
              task)

          ;; open with shell-fn
          (funcall shell-fn)
          ;; say something
          (message "Opening taskspace shell: %s" (file:name task))
          ;; return the chosen task's dir?
          task)))))
;; (taskspace:shell :work)
;; M-x taskspace:shell


;;;###autoload
(defun taskspace:notes (date-input group)
  "Open a taskspace's notes file.

DATE-INPUT is prefix arg:
  - No prefix arg means today.
  - Prefix arg means tomorrow.

GROUP should be a group keyword.

DWIM-ish actions:
  - If just one taskspace for DATE-INPUT, open today's notes file.
  - If more than one taskspace, prompt user with auto-complete options for
    DATE-INPUT's notes files.
  - If prefix arg supplied: prompt user with auto-complete options for all notes
    files.
  - If no taskspaces are found for the DATE-INPUT, list all taskspaces for
    GROUP."
  ;; Numeric arg but don't let lower case "p" auto-magic nothing (no prefix arg)
  ;; into 1. Nothing/0/nil is today. 1 is tomorrow.
  (interactive (list current-prefix-arg
                     (int<taskspace>:prompt:group 'auto t)))

  ;; Default to "today" if date-input isn't parsable string,
  ;; then get date, taskspaces, etc. for that numerical relative day.
  (let* ((date-parsed (cond
                       ;; no date-input is today is 0
                       ((null date-input) 0)
                       ;; strings should be converted to numbers
                       ((stringp date-input)
                        (string-to-number date-input))
                       ;; just allow numbers through unscathed
                       ((numberp date-input) date-input)
                       ;; default to... today/0 I guess?
                       (t 0)))
         (date        (int<taskspace>:naming:get:date group date-parsed))
         (taskspaces  (int<taskspace>:dir:list:date group date))
         (taskspaces  (or taskspaces
                          (int<taskspace>:dir:list:all group)))
         (length-ts   (length taskspaces))
         taskpath)

    (message "%S tasks for %S: %S" length-ts date taskspaces)

    (cond
     ;; error out if we have no idea what date to dwim with...
     ((null date)
      (nub:error
          :taskspace
          "taskspace:notes"
         "Date string is nil: %s"
         date))

     ;; If just one, open its notes file.
     ((= length-ts 1)
      (setq taskpath (cl-first taskspaces))
      (message "Only taskspace: %s" taskpath))

     ;; For now, only give existing choices. User can use a non-dwim create func
     ;; if they want new.
     ((> length-ts 1)

      ;; list available choices to user, get the taskspace they chose
      (let ((choice (int<taskspace>:prompt:task/existing group taskspaces 'nondirectory)))
        (setq taskpath choice)
        (message "Chose taskspace: %s" choice)))

     ;; Default case... Fall through with nil.
     (t nil))

    (if (null taskpath)
        (nub:error
            :taskspace
            "taskspace:notes"
          "No taskspace notes found for date: %s"
          date)

        ;; Exists; message and visit it.
        (int<taskspace>:notes:open group taskpath))))
;; M-x taskspace:notes
;; (taskspace:notes)
;; (taskspace:notes -1 (int<taskspace>:prompt:group 'auto t))


;; TODO: more options for making keybind:
;; (&optional orphaned description prefix)
(defun taskspace:keybind:doom ()
  "Create keybinds in Doom, or raise an error if not in Doom.

Create a keymap; insert into doom/evil or vanilla Emacs as
appropriate/parameters say.

Creates the taskspace keymap under the doom leader key (default SPC)
TODO: unless ORPHANED is not nil.

TODO: Uses DESCRIPTION if it is a string, else the description is 'taskspace'.

TODO: Uses PREFIX as the keymap prefix if not nil, else tries to use:
TODO:   - doom/evil emacs: doom-leader(?) n T
TODO:   - vanilla emacs:   TODO
TODO:
TODO: PREFIX for doom/evil should be either:
TODO:   1) a key string:
TODO:      - \"t\"
TODO:      - \"n t\"
TODO:        - \"nt\" is equivalent
TODO:   2) a cons of (key-string . description-str):
TODO:      - (\"t\" . \"taskspace\")
TODO:      - (\"n t\" . \"taskspace\")
TODO:   3) a list of 1 or 2 for setting into a sub-map:
TODO:      - (\"t\" (\"n\" (\"t\" . \"taskspace\")))"
  (if (null (symbolp 'doom!)) ;; Doom's loading function should mean this is doom?
      (nub:error
          :taskspace
          "taskspace:keybind:doom"
        "We are not in a Doom Emacs environment..? Cannot set Doom Emacs keybinds.")

    ;; Map under the Doom leader, probably.
    (map! :leader
          ;; Give it a description...
          :desc "taskspace"

          ;; ...and give it keybinds.
          (:prefix ("T" . "taskspace")

           ;; Top level commands...
           :desc "Create new..."  "T" #'taskspace:create
           :desc "Visit notes..." "v" #'taskspace:notes
           :desc "Shell..."       "s" #'taskspace:shell

           ;; 'Copy to kill ring' functions:
           (:prefix ("k" . "Kill...")

            :desc "dir"  "k" #'taskspace:dwim:dir
            :desc "name" "n" #'taskspace:dwim:name)

           ;; 'Open a dired buffer' functions:
           (:prefix ("d" . "dired")
            :desc "task dired buffer" "d" #'taskspace:dired:task
            :desc "root dired buffer" "r" #'taskspace:dired:root)))))
;; TODO: prefix: ;; Put it under the prefix(es).
;; TODO: prefix: (cond
;; TODO: prefix:  ;;---
;; TODO: prefix:  ;; NIL
;; TODO: prefix:  ;;---
;; TODO: prefix:  ;; Return just the keybinds.
;; TODO: prefix:  ((null prefix)
;; TODO: prefix:   (int<taskspace>:keybind/doom))
;; TODO: prefix:
;; TODO: prefix:  ;;---
;; TODO: prefix:  ;; STRING
;; TODO: prefix:  ;;---
;; TODO: prefix:  ;; Put keybinds under the prefix str.
;; TODO: prefix:  ((stringp prefix)
;; TODO: prefix:   (list prefix (int<taskspace>:keybind/doom)))
;; TODO: prefix:
;; TODO: prefix:  ;;---
;; TODO: prefix:  ;; CONS
;; TODO: prefix:  ;;---
;; TODO: prefix:  ;; Put keybinds under the prefix /CONS/.
;; TODO: prefix:  ;; ONLY CONS!!!
;; TODO: prefix:  ;; No lists.
;; TODO: prefix:  ;; Just cons.
;; TODO: prefix:  ;; https://emacs.stackexchange.com/questions/10489/predicate-function-for-dotted-pairs
;; TODO: prefix:  ((and (cdr var) (atom (cdr var)))
;; TODO: prefix:   ;; Create list with description, key, and our keybids.
;; TODO: prefix:   (list
;; TODO: prefix:    :desc (nth 1 prefix)
;; TODO: prefix:    (nth 0 prefix)
;; TODO: prefix:    (int<taskspace>:keybind/doom)))
;; TODO: prefix:
;; TODO: prefix:  ;;---
;; TODO: prefix:  ;; LIST
;; TODO: prefix:  ;;---
;; TODO: prefix:  ((listp prefix)
;; TODO: prefix:   ;; Build first layer and do a recursion...
;; TODO: prefix:   (-let [(this-prefix . rest) prefix]
;; TODO: prefix:     (
;; TODO: prefix:      )))
;; TODO: prefix:
;; TODO: prefix:   (list
;; TODO: prefix:    :desc (nth 1 prefix)
;; TODO: prefix:    (nth 0 prefix)
;; TODO: prefix:
;; TODO: prefix:    prefix (int<taskspace>:keybind/doom)))
;; TODO: prefix:
;; TODO: prefix:
;; TODO: prefix: (defun int<taskspace>:no-error-predicate (func value)
;; TODO: prefix:   "Checks if VALUE fulfills FUNC function predicate.
;; TODO: prefix:
;; TODO: prefix: Wraps check in a `condition-case-unless-debug', returns nil if error signal caught."
;; TODO: prefix:   (condition-case-unless-debug nil
;; TODO: prefix:       (funcall func value)
;; TODO: prefix:     (error nil)))
;; TODO: prefix:
;; TODO: prefix:
;; TODO: prefix: (defun int<taskspace>:keybind/doom.level (key description rest)
;; TODO: prefix:   "Creates a level of a doom `map!'.
;; TODO: prefix:
;; TODO: prefix: KEY must be a string for the key(s) to use for this level of the keybind tree.
;; TODO: prefix:
;; TODO: prefix: DESCRIPTION must be a (short) description string or nil.
;; TODO: prefix:
;; TODO: prefix: REST must be a function to call, or a deeper (child) level of the keybind tree."
;; TODO: prefix:   ;; Error check our inputs...
;; TODO: prefix:   (cond ((not (int<taskspace>:no-error-predicate #'stringp key))
;; TODO: prefix:          (error "%s: Key must be a string: %S"
;; TODO: prefix:                 "int<taskspace>:keybind/doom.level" key))
;; TODO: prefix:
;; TODO: prefix:         ((and (not (null description))
;; TODO: prefix:               (not (int<taskspace>:no-error-predicate #'stringp description)))
;; TODO: prefix:          (error "%s: Description must be nil or a string: %S"
;; TODO: prefix:                 "int<taskspace>:keybind/doom.level" description))
;; TODO: prefix:
;; TODO: prefix:         ((null rest)
;; TODO: prefix:          (error "%s: Must have something to bind: %S"
;; TODO: prefix:                 "int<taskspace>:keybind/doom.level" rest))
;; TODO: prefix:
;; TODO: prefix:         ;;---
;; TODO: prefix:         ;; Success - make the level.
;; TODO: prefix:         ;;---
;; TODO: prefix:         (t
;; TODO: prefix:          (if (null description)
;; TODO: prefix:              (list key rest)
;; TODO: prefix:            (list :desc description
;; TODO: prefix:                  key
;; TODO: prefix:                  rest)))))


;;------------------------------------------------------------------------------
;; Directory Local Variables
;;------------------------------------------------------------------------------

(defvar int<taskspace>:dlv:group nil
  "This should always be nil unless used via Directory Local Variables.

It should only be set via `taskspace:group:dlv'")

;; Mark our DLV variable as safe for DLV use.
(dlv:var:safe.predicate 'int<taskspace>:dlv:group #'int<taskspace>:group:valid?)


;;------------------------------------------------------------------------------
;; Tasks, Wants, Feature Requests, etc.
;;------------------------------------------------------------------------------

;; TODO-PKG:
;;   - Comments/layout like a real package.
;;     e.g. https://github.com/tarsius/moody/blob/master/moody.el


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :taskspace 'taskspace)

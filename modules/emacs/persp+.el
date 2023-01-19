;;; modules/emacs/persp+.el --- `persp-mode' commands -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2023-01-19
;; Modified:   2023-01-19
;; URL:        https://github.com/cole-brown/.config-emacs
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;  `persp-mode' commands & helpers
;;
;; Originally from Doom's "modules/ui/workspaces/autoload/workspaces.el".
;;
;;; Code:


(require 'cl-lib)


(imp:require :window)
(imp:require :buffer)


;;------------------------------------------------------------------------------
;; Constants & Variables
;;------------------------------------------------------------------------------

(defvar persp:filename:data "_perspectives"
  "The basename of the file to store single workspace perspectives.

Will be stored in `persp-save-dir'.")


(defvar int<persp>:last nil
  "Last selected workspace.")


;;------------------------------
;; Faces
;;------------------------------

;;;###autoload
(defface persp:face:tab/selected '((t (:inherit highlight)))
  "The face for selected tabs displayed by `persp:cmd:display'."
  :group 'persp-mode)

;;;###autoload
(defface persp:face:tab '((t (:inherit default)))
  "The face for selected tabs displayed by `persp:cmd:display'."
  :group 'persp-mode)


;;------------------------------------------------------------------------------
;; Predicates
;;------------------------------------------------------------------------------

(defun int<persp>:protected? (name)
  "Is NAME a protected workspace?"
  (equal name persp-nil-name))


;;;###autoload
(defun persp:exists? (name)
  "Return t if NAME is the name of an existing workspace."
  (member name (persp:name:list)))


;;------------------------------------------------------------------------------
;; Getters
;;------------------------------------------------------------------------------

;;;###autoload
(defalias #'persp:current #'get-current-persp
  "Return the currently active workspace.")


;;;###autoload
(defun persp:get (name &optional no-error?)
  "Return a workspace named NAME.

Throw an error if NAME doesn't exist, unless NO-ERROR? is non-nil."
  (cl-check-type name string)
  (when-let (persp (persp-get-by-name name))
    (cond ((perspective-p persp) persp)
          ((not no-error?)
           (error "No workspace called '%s' was found" name)))))


(defun int<persp>:name:generate ()
  "Generate a generic name for a new workspace."
  (format "#%s"
          (or (cl-loop for name in (persp:name:list)
                       when (string-match-p "^#[0-9]+$" name)
                       maximize (string-to-number (substring name 1)) into max
                       finally return (if max (1+ max)))
              1)))


;;;###autoload
(defun persp:name:current ()
  "Get the name of the current workspace."
  (safe-persp-name (persp:current)))


;;;###autoload
(defun persp:list ()
  "Return a list of workspace structs (satisifes `perspective-p')."
  ;; We don't use `hash-table-values' because it doesn't ensure order in older
  ;; versions of Emacs
  (cl-loop for name in persp-names-cache
           if (gethash name *persp-hash*)
           collect it))


;;;###autoload
(defun persp:name:list ()
  "Return the list of names of open workspaces."
  persp-names-cache)


;;;###autoload
(defun persp:list:buffer (&optional persp)
  "Return a list of buffers in PERSP.

PERSP can be a string (name of a workspace) or a workspace (satisfies
`perspective-p'). If nil or omitted, it defaults to the current workspace."
  (let ((persp (or persp (persp:current))))
    (unless (perspective-p persp)
      (user-error "Not in a valid workspace (%s)" persp))
    (persp-buffers persp)))


;;;###autoload
(defun persp:buffer:list/orphaned ()
  "Return a list of buffers that aren't associated with any perspective."
  (cl-remove-if #'persp--buffer-in-persps (buffer-list)))


;;------------------------------------------------------------------------------
;; Actions
;;------------------------------------------------------------------------------

;;;###autoload
(defun persp:action:load (name)
  "Load a single workspace (named NAME) into the current session.

Can only retrieve perspectives that were explicitly saved with
`persp:action:save'.

Returns t if successful, nil otherwise."
  (when (persp:exists? name)
    (user-error "A workspace named '%s' already exists." name))
  (persp-load-from-file-by-names
   (expand-file-name persp:filename:data persp-save-dir)
   *persp-hash*
   (list name))
  (persp:exists? name))


;;;###autoload
(defun persp:action:save (name)
  "Save a single workspace (NAME) from the current session.

Can be loaded again with `persp:action:load'. NAME can be the string name of a
workspace or its perspective hash table.

Return t on success, nil otherwise."
  (unless (persp:exists? name)
    (error "'%s' is an invalid workspace" name))
  (let ((fname (expand-file-name persp:filename:data persp-save-dir)))
    (persp-save-to-file-by-names fname *persp-hash* (list name))
    (and (member name (persp-list-persp-names-in-file fname))
         t)))


;;;###autoload
(defun persp:action:new (name)
  "Create a new workspace named NAME.

Return:
  - nil - NAME already exists
  - t   - success
  - nil - failure"
  (when (int<persp>:protected? name)
    (error "Can't create a new '%s' workspace" name))
  (when (persp:exists? name)
    (error "A workspace named '%s' already exists" name))
  (let ((persp (persp-add-new name))
        ;; (+popup--inhibit-transient t)
        )
    (save-window-excursion
      (let ((ignore-window-parameters t)
            ;; (+popup--inhibit-transient t)
            )
        (persp-delete-other-windows))
      (switch-to-buffer "*scratch*")
      (setf (persp-window-conf persp)
            (funcall persp-window-state-get-function (selected-frame))))
    persp))


;;;###autoload
(defun persp:action:rename (name new-name)
  "Rename the current workspace named NAME to NEW-NAME.

Return non-nil on success, nil otherwise."
  (when (int<persp>:protected? name)
    (error "Can't rename '%s' workspace" name))
  (persp-rename new-name (persp:get name)))


;;;###autoload
(defun persp:action:delete (workspace &optional inhibit-kill?)
  "Delete WORKSPACE.

WORKSPACE can be the name of a perspective or its hash table.

If INHIBIT-KILL? is non-nil, don't kill this workspace's buffers."
  (unless (stringp workspace)
    (setq workspace (persp-name workspace)))
  (when (int<persp>:protected? workspace)
    (error "Can't delete '%s' workspace" workspace))
  (persp:get workspace) ; error checking
  (persp-kill workspace inhibit-kill?)
  (not (persp:exists? workspace)))


;;;###autoload
(defun persp:action:switch (name &optional auto-create?)
  "Switch to another workspace named NAME (a string).

If AUTO-CREATE? is non-nil, create the workspace if it doesn't exist, otherwise
throws an error."
  (unless (persp:exists? name)
    (if auto-create?
        (persp:action:new name)
      (error "%s is not an available workspace" name)))
  (let ((old-name (persp:name:current)))
    (unless (equal old-name name)
      (setq int<persp>:last
            (or (and (not (string= old-name persp-nil-name))
                     old-name)
                mantle:user:perspective:default))
      (persp-frame-switch name))
    (equal (persp:name:current) name)))


;;------------------------------------------------------------------------------
;; Commands
;;------------------------------------------------------------------------------

;;;###autoload
(defun persp:cmd:load (name)
  "Load workspace NAME and switch to it.

If called with the prefix argument, try to reload the current workspace from
session files."
  (interactive
   (list
    (if current-prefix-arg
        (persp:name:current)
      (completing-read
       "Workspace to load: "
       (persp-list-persp-names-in-file
        (expand-file-name persp:filename:data persp-save-dir))))))
  (if (not (persp:action:load name))
      (persp:out:error (format "Couldn't load workspace %s" name))
    (persp:cmd:switch/index name)
    (persp:cmd:display)))


;;;###autoload
(defun persp:cmd:save (name)
  "Save the current workspace as NAME.

If called with the prefix argument, autosave the current workspace."
  (interactive
   (list
    (if current-prefix-arg
        (persp:name:current)
      (completing-read "Workspace to save: " (persp:name:list)))))
  (if (persp:action:save name)
      (persp:out:message (format "'%s' workspace saved" name) 'success)
    (persp:out:error (format "Couldn't save workspace %s" name))))


;;;###autoload
(defun persp:cmd:rename (new-name)
  "Rename the current workspace to NEW-NAME."
  (interactive (list (read-from-minibuffer "New workspace name: ")))
  (condition-case-unless-debug ex
      (let* ((current-name (persp:name:current))
             (old-name (persp:action:rename current-name new-name)))
        (unless old-name
          (error "Failed to rename %s" current-name))
        (persp:out:message (format "Renamed '%s'->'%s'" old-name new-name) 'success))
    ('error (persp:out:error ex t))))


;;;###autoload
(defun persp:cmd:delete (name)
  "Delete the NAME workspace.

If called with the prefix argument, prompts you for the name of the workspace to
delete."
  (interactive
   (let ((current-name (persp:name:current)))
     (list
      (if current-prefix-arg
          (completing-read (format "Delete workspace (default: %s): " current-name)
                           (persp:name:list)
                           nil nil nil nil current-name)
        current-name))))
  (condition-case-unless-debug ex
      ;; REVIEW refactor me
      (let ((workspaces (persp:name:list)))
        (if (not (member name workspaces))
            (persp:out:message (format "'%s' workspace doesn't exist" name) 'warn)
          ;; Sanity Check
          (cond ((delq (selected-frame) (persp-frames-with-persp (get-frame-persp)))
                 (user-error "Can't close workspace, it's visible in another frame"))
                ;; Delete some other perspective.
                ((not (equal (persp:name:current) name))
                 (persp:action:delete name))
                ;; Delete one of N perspectives (N>1).
                ((cdr workspaces)
                 (persp:action:delete name)
                 (persp:action:switch
                  (if (persp:exists? int<persp>:last)
                      int<persp>:last
                    (car (persp:name:list))))
                 ; doom: (unless (doom-buffer-frame-predicate (window-buffer))
                 (unless (or (buffer:type:real? (window-buffer))
                             (eq (window-buffer) (buffer:fallback:get-or-create)))
                   (switch-to-buffer (buffer:fallback:get-or-create))))
                ;; Delete the only perspective.
                (t
                 ;; Go to the default perspective...
                 (persp:action:switch mantle:user:perspective:default t)
                 ;; Delete that final perspective.
                 (unless (string= (car workspaces) mantle:user:perspective:default)
                   (persp:action:delete name))
                 ;; ...and kill all buffers?
                 ;;   - Either all buffers for the perspective, or just ALL buffers...
                 (buffer:kill:list (buffer:list))))
          (persp:out:message (format "Deleted '%s' workspace" name) 'success)))
    ('error (persp:out:error ex t))))


;;;###autoload
(defun persp:cmd:kill (&optional message?)
  "Delete all workspaces, windows, and their buffers.

If MESSAGE? is non-nil, output a message about how many of each were killed."
  ;; If we were called interactively, we should probably always output the message?
  (interactive (list t))
  (let ((windows (length (window-list)))
        (persps (length (persp:name:list)))
        (buffers 0))
    (let ((persp-autokill-buffer-on-remove t))
      (unless (cl-every #'persp:action:delete (persp:name:list))
        (persp:out:error "Could not kill workspace")))
    (persp:action:switch mantle:user:perspective:default t)
    ;; TODO: Doom's `doom/kill-all-buffers' also closes windows?
    (setq buffers (buffer:kill:list (buffer-list)))
    (when (called-interactively-p)
      (message "Killed %d workspace(s), %d window(s) & %d buffer(s)"
               persps windows buffers))))


;;;###autoload
(defun persp:cmd:new (&optional name clone?)
  "Create a new workspace named NAME.

If CLONE? is non-nil, clone the current workspace, otherwise the new workspace
is blank."
  (interactive (list nil current-prefix-arg))
  (unless name
    (setq name (int<persp>:name:generate)))
  (condition-case e
      (cond ((persp:exists? name)
             (error "%s already exists" name))
            (clone? (persp-copy name t))
            (t
             (persp:action:switch name t)
             (persp:cmd:display)))
    ((debug error) (persp:out:error (cadr e) t))))


;;;###autoload
(defun persp:cmd:new-named (name)
  "Create a new workspace with a given NAME."
  (interactive "sWorkspace Name: ")
  (persp:cmd:new name))


;;;###autoload
(defun persp:cmd:switch/index (index)
  "Switch to a workspace at a given INDEX.

A negative number will start from the end of the workspace list."
  (interactive
   (list (or current-prefix-arg
             (completing-read "Switch to workspace: " (persp:name:list)))))
  (when (and (stringp index)
             (string-match-p "^[0-9]+$" index))
    (setq index (string-to-number index)))
  (condition-case-unless-debug ex
      (let ((names (persp:name:list))
            (old-name (persp:name:current)))
        (cond ((numberp index)
               (let ((dest (nth index names)))
                 (unless dest
                   (error "No workspace at #%s" (1+ index)))
                 (persp:action:switch dest)))
              ((stringp index)
               (persp:action:switch index t))
              (t
               (error "Not a valid index: %s" index)))
        (unless (called-interactively-p 'interactive)
          (if (equal (persp:name:current) old-name)
              (persp:out:message (format "Already in %s" old-name) 'warn)
            (persp:cmd:display))))
    ('error (persp:out:error (cadr ex) t))))


;;;###autoload
(dotimes (i 9)
  (defalias (intern (format "persp:cmd:switch/index:%d" i))
    (lambda () (interactive) (persp:cmd:switch/index i))
    (format "Switch to workspace #%d" (1+ i))))


;;;###autoload
(defun persp:cmd:switch/final ()
  "Switch to the final workspace in open workspaces."
  (interactive)
  (persp:cmd:switch/index (car (last (persp:name:list)))))


;;;###autoload
(defun persp:cmd:switch/last ()
  "Switch to the last activated workspace."
  (interactive)
  (persp:cmd:switch/index int<persp>:last))


;; ###autoload
(defun persp:cmd:cycle (n)
  "Cycle N workspaces to the right or left.

N > 0: cycle right
N < 0: cycle left"
  (interactive (list 1))
  (let ((current-name (persp:name:current)))
    (if (equal current-name persp-nil-name)
        (persp:action:switch mantle:user:perspective:default t)
      (condition-case-unless-debug ex
          (let* ((persps (persp:name:list))
                 (perspc (length persps))
                 (index (cl-position current-name persps)))
            (when (= perspc 1)
              (user-error "No other workspaces"))
            (persp:cmd:switch/index (% (+ index n perspc) perspc))
            (unless (called-interactively-p 'interactive)
              (persp:cmd:display)))
        ('user-error (persp:out:error (cadr ex) t))
        ('error (persp:out:error ex t))))))


;;;###autoload
(defun persp:cmd:cycle/left ()
  "Cycle one workspace to the left."
  (interactive)
  (persp:cmd:cycle -1))


;;;###autoload
(defun persp:cmd:cycle/right ()
  "Cycle one workspace to the left."
  (interactive)
  (persp:cmd:cycle +1))


;;;###autoload
(defun persp:cmd:swap/left (&optional count)
  "Swap the current workspace with the COUNTth workspace on its left."
  (interactive "p")
  (let* ((current-name (persp:name:current))
         (count (or count 1))
         (index (- (cl-position current-name persp-names-cache :test #'equal)
                   count))
         (names (remove current-name persp-names-cache)))
    (unless names
      (user-error "Only one workspace"))
    (let ((index (min (max 0 index) (length names))))
      (setq persp-names-cache
            (append (cl-subseq names 0 index)
                    (list current-name)
                    (cl-subseq names index))))
    (when (called-interactively-p 'any)
      (persp:cmd:display))))


;;;###autoload
(defun persp:cmd:swap/right (&optional count)
  "Swap the current workspace with the COUNTth workspace on its right."
  (interactive "p")
  (funcall-interactively #'persp:cmd:swap/left (- count)))


;;------------------------------------------------------------------------------
;; Tabs Display In Minibuffer
;;------------------------------------------------------------------------------

(defun int<persp>:format:tabline (&optional names)
  "Format workspace NAMES into a display string."
  (let ((names (or names (persp:name:list)))
        (current-name (persp:name:current)))
    (mapconcat
     #'identity
     (cl-loop for name in names
              for i to (length names)
              collect
              (propertize (format " [%d] %s " (1+ i) name)
                          'face (if (equal current-name name)
                                    'persp:face:tab/selected
                                  'persp:face:tab)))
     " ")))


;;;###autoload
(defun persp:cmd:display ()
  "Display a list of workspaces (like tabs) in the echo area."
  (interactive)
  (let (message-log-max)
    (message "%s" (int<persp>:format:tabline))))


;;------------------------------------------------------------------------------
;; Messaging Helpers
;;------------------------------------------------------------------------------

(defun int<persp>:out:body (message &optional type)
  "Propertize and prefix MESSAGE for display in the modeline.

Prefix MESSAGE with function `int<persp>:format:tabline'.

Propertize MESSAGE according to TYPE:
  - `error'
  - `warn'
  - `success'
  - `info'"
  (concat (int<persp>:format:tabline)
          (propertize " | " 'face 'font-lock-comment-face)
          (propertize (format "%s" message)
                      'face (pcase type
                              ('error 'error)
                              ('warn 'warning)
                              ('success 'success)
                              ('info 'font-lock-comment-face)))))


;;;###autoload
(defun persp:out:message (message &optional type)
  "Show an 'elegant' MESSAGE in the echo area next to a listing of workspaces.

Prefix MESSAGE with function `int<persp>:format:tabline'.

Propertize MESSAGE according to TYPE:
  - `error'
  - `warn'
  - `success'
  - `info'"
  (message "%s" (int<persp>:out:body message type)))


;;;###autoload
(defun persp:out:error (message &optional no-error?)
  "Show an 'elegant' error in the echo area next to a listing of workspaces.

Prefix MESSAGE with function `int<persp>:format:tabline'.

Propertize MESSAGE according to TYPE:
  - `error'
  - `warn'
  - `success'
  - `info'

Will print using `error', unless NO-ERROR? is non-nil, in which case will print
via `message'."
  (funcall (if no-error? #'message #'error)
           "%s" (int<persp>:out:body message 'error)))


;;------------------------------------------------------------------------------
;; Hooks
;;------------------------------------------------------------------------------

;;;###autoload
(defun persp:hook:associated/delete (&optional frame)
  "Delete workspace associated with current FRAME.

A workspace gets associated with a frame when a new frame is interactively
created.

Example for `use-package persp-mode' `:config' section:
  ;; per-frame workspaces
  (setq persp-init-frame-behaviour t
        persp-init-new-frame-behaviour-override nil
        persp-interactive-init-frame-behaviour-override #'persp:hook:associted/create
        persp-emacsclient-init-frame-behaviour-override #'persp:hook:associted/create)
  (add-hook 'delete-frame-functions #'persp:hook:associated/delete)
  (add-hook 'server-done-hook #'persp:hook:associated/delete)"
  (when (and persp-mode (not (bound-and-true-p with-editor-mode)))
    (unless frame
      (setq frame (selected-frame)))
    (let ((frame-persp (frame-parameter frame 'workspace)))
      (when (string= frame-persp (persp:name:current))
        (persp:cmd:delete frame-persp)))))


;;;###autoload
(defun persp:hook:associted/create (frame &optional _new-frame?)
  "Create a blank, new perspective and associate it with FRAME.

Example for `use-package persp-mode' `:config' section:
  ;; per-frame workspaces
  (setq persp-init-frame-behaviour t
        persp-init-new-frame-behaviour-override nil
        persp-interactive-init-frame-behaviour-override #'persp:hook:associted/create
        persp-emacsclient-init-frame-behaviour-override #'persp:hook:associted/create)
  (add-hook 'delete-frame-functions #'persp:hook:associated/delete)
  (add-hook 'server-done-hook #'persp:hook:associated/delete)"
  (when persp-mode
    (if (not (persp-frame-list-without-daemon))
        (persp:action:switch mantle:user:perspective:default t)
      (with-selected-frame frame
        (persp:action:switch (int<persp>:name:generate) t)
        (unless (buffer:type:real? (current-buffer))
          (switch-to-buffer (buffer:fallback:get-or-create)))
        (set-frame-parameter frame 'workspace (persp:name:current))
        ;; ensure every buffer has a buffer-predicate
        (persp-set-frame-buffer-predicate frame))
      (run-at-time 0.1 nil #'persp:cmd:display))))


;;------------------------------------------------------------------------------
;; Keybinds
;;------------------------------------------------------------------------------

;; TODO: a (more) generic(ish) keybind func like taskspace.
;; (keybind:leader/global:def
;;    :infix   "TAB"
;;
;;    "TAB" (list #'persp:cmd:display :which-key "Display Persps")
;;    "n"   (list #'persp:cmd:new :which-key "New workspace")
;;    "N"   (list #'persp:cmd:new-named :which-key "New named workspace")
;;    "l"   (list #'persp:cmd:load :which-key "Load workspace from file")
;;    "s"   (list #'persp:cmd:save :which-key "Save workspace to file")
;;    "x"   (list #'persp:cmd:kill :which-key "Delete session")
;;    "d"   (list #'persp:cmd:delete :which-key "Delete this workspace")
;;    "r"   (list #'persp:cmd:rename :which-key "Rename workspace")
;;    "]"   (list #'persp:cmd:cycle/right :which-key "Next workspace")
;;    "["   (list #'persp:cmd:cycle/left :which-key "Previous workspace")
;;    "."   (list #'persp:cmd:switch/index :which-key "Switch workspace")
;;    "`"   (list #'persp:cmd:switch/last :which-key "Switch to last workspace")
;;    "1"   (list #'persp:cmd:switch/index:0 :which-key "Switch to 1st workspace")
;;    "2"   (list #'persp:cmd:switch/index:1 :which-key "Switch to 2nd workspace")
;;    "3"   (list #'persp:cmd:switch/index:2 :which-key "Switch to 3rd workspace")
;;    "4"   (list #'persp:cmd:switch/index:3 :which-key "Switch to 4th workspace")
;;    "5"   (list #'persp:cmd:switch/index:4 :which-key "Switch to 5th workspace")
;;    "6"   (list #'persp:cmd:switch/index:5 :which-key "Switch to 6th workspace")
;;    "7"   (list #'persp:cmd:switch/index:6 :which-key "Switch to 7th workspace")
;;    "8"   (list #'persp:cmd:switch/index:7 :which-key "Switch to 8th workspace")
;;    "9"   (list #'persp:cmd:switch/index:8 :which-key "Switch to 9th workspace")
;;    "0"   (list #'persp:cmd:switch/final :which-key "Switch to final workspace"))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :buffer 'persp-mode-cmds)

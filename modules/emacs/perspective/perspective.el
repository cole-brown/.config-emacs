;;; modules/emacs/perspective.el --- `persp-mode' commands -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2023-01-19
;; Timestamp:  2023-06-29
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; `persp-mode' commands & helpers
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

(defvar perspective:name:default "main"
  "The name of the primary and initial perspective.")


(defvar perspective:cache:uniquify-buffer-name-style nil
  "Cache for `uniquify-buffer-name-style'.

`persp-mode' has some issues with `uniquify' buffer names, so we have to cache
the style & disable it occasionally.")


(defvar perspective:filename:data "_perspectives"
  "The basename of the file to store single workspace perspectives.

Will be stored in `persp-save-dir'.")


(defvar int<perspective>:last nil
  "Last selected workspace.")


;;------------------------------
;; Faces
;;------------------------------

;;;###autoload
(defface perspective:face:tab/selected '((t (:inherit highlight)))
  "The face for selected tabs displayed by `perspective:cmd:display'."
  :group 'persp-mode)


;;;###autoload
(defface perspective:face:tab '((t (:inherit default)))
  "The face for selected tabs displayed by `perspective:cmd:display'."
  :group 'persp-mode)


;;------------------------------------------------------------------------------
;; Predicates
;;------------------------------------------------------------------------------

(defun int<perspective>:protected? (name)
  "Is NAME a protected workspace?

From Doom's `+workspace--protected-p' in
\"modules/ui/workspaces/autoload/workspaces.el\"."
  (equal name persp-nil-name))


;;;###autoload
(defun perspective:exists? (name)
  "Return t if NAME is the name of an existing workspace.

From Doom's `+workspace-exists-p' in
\"modules/ui/workspaces/autoload/workspaces.el\"."
  (member name (perspective:name:list)))


;;------------------------------------------------------------------------------
;; Getters
;;------------------------------------------------------------------------------

;;;###autoload
(defalias #'perspective:current #'get-current-persp
  "Return the currently active workspace.")


;;;###autoload
(defun perspective:get (name &optional no-error?)
  "Return a workspace named NAME.

Throw an error if NAME doesn't exist, unless NO-ERROR? is non-nil.

From Doom's `+workspace-get' in
\"modules/ui/workspaces/autoload/workspaces.el\"."
  (cl-check-type name string)
  (when-let (persp (persp-get-by-name name))
    (cond ((perspective-p persp) persp)
          ((not no-error?)
           (error "No workspace called '%s' was found" name)))))


(defun int<perspective>:name:generate ()
  "Generate a generic name for a new workspace."
  (format "#%s"
          (or (cl-loop for name in (perspective:name:list)
                       when (string-match-p "^#[0-9]+$" name)
                       maximize (string-to-number (substring name 1)) into max
                       finally return (if max (1+ max)))
              1)))


;;;###autoload
(defun perspective:name:current ()
  "Get the name of the current workspace.

From Doom's `+workspace-current-name' in
\"modules/ui/workspaces/autoload/workspaces.el\"."
  (safe-persp-name (perspective:current)))


;;;###autoload
(defun perspective:list ()
  "Return a list of workspace structs (satisifes `perspective-p').

From Doom's `+workspace-list' in
\"modules/ui/workspaces/autoload/workspaces.el\"."
  ;; We don't use `hash-table-values' because it doesn't ensure order in older
  ;; versions of Emacs
  (cl-loop for name in persp-names-cache
           if (gethash name *persp-hash*)
           collect it))


;;;###autoload
(defun perspective:name:list ()
  "Return the list of names of open workspaces.

From Doom's `+workspace-list-names' in
\"modules/ui/workspaces/autoload/workspaces.el\"."
  persp-names-cache)


;;;###autoload
(defun perspective:list:buffer (&optional persp)
  "Return a list of buffers in PERSP.

PERSP can be a string (name of a workspace) or a workspace (satisfies
`perspective-p'). If nil or omitted, it defaults to the current workspace.

From Doom's `+workspace-buffer-list' in
\"modules/ui/workspaces/autoload/workspaces.el\"."
  (let ((persp (or persp (perspective:current))))
    (unless (perspective-p persp)
      (user-error "Not in a valid workspace (%s)" persp))
    (persp-buffers persp)))


;;;###autoload
(defun perspective:buffer:list/orphaned ()
  "Return a list of buffers that aren't associated with any perspective.

From Doom's `+workspace-orphaned-buffer-list' in
\"modules/ui/workspaces/autoload/workspaces.el\"."
  (cl-remove-if #'persp--buffer-in-persps (buffer-list)))
;; (perspective:buffer:list/orphaned)


;;------------------------------------------------------------------------------
;; Actions
;;------------------------------------------------------------------------------

;;;###autoload
(defun perspective:action:load (name)
  "Load a single workspace (named NAME) into the current session.

Can only retrieve perspectives that were explicitly saved with
`persp:action:save'.

Returns t if successful, nil otherwise.

From Doom's `+workspace-load' in
\"modules/ui/workspaces/autoload/workspaces.el\"."
  (when (perspective:exists? name)
    (user-error "A workspace named '%s' already exists." name))
  (persp-load-from-file-by-names
   (expand-file-name perspective:filename:data persp-save-dir)
   *persp-hash*
   (list name))
  (perspective:exists? name))


;;;###autoload
(defun perspective:action:save (name)
  "Save a single workspace (NAME) from the current session.

Can be loaded again with `perspective:action:load'. NAME can be the string name
of a workspace or its perspective hash table.

Return t on success, nil otherwise.

From Doom's `+workspace-save' in
\"modules/ui/workspaces/autoload/workspaces.el\"."
  (unless (perspective:exists? name)
    (error "'%s' is an invalid workspace" name))
  (let ((fname (expand-file-name perspective:filename:data persp-save-dir)))
    (persp-save-to-file-by-names fname *persp-hash* (list name))
    (and (member name (persp-list-persp-names-in-file fname))
         t)))


;;;###autoload
(defun perspective:action:new (name)
  "Create a new workspace named NAME.

Return:
  - nil - NAME already exists
  - t   - success
  - nil - failure

From Doom's `+workspace-new' in
\"modules/ui/workspaces/autoload/workspaces.el\"."
  (when (int<perspective>:protected? name)
    (error "Can't create a new '%s' workspace" name))
  (when (perspective:exists? name)
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
      (let ((config (persp-window-conf persp)))
        (setf config
              (funcall persp-window-state-get-function (selected-frame)))))
    persp))


;;;###autoload
(defun perspective:action:rename (name new-name)
  "Rename the current workspace named NAME to NEW-NAME.

Return non-nil on success, nil otherwise.

From Doom's `+workspace-rename' in
\"modules/ui/workspaces/autoload/workspaces.el\"."
  (when (int<perspective>:protected? name)
    (error "Can't rename '%s' workspace" name))
  (persp-rename new-name (perspective:get name)))


;;;###autoload
(defun perspective:action:delete (workspace &optional inhibit-kill?)
  "Delete WORKSPACE.

WORKSPACE can be the name of a perspective or its hash table.

If INHIBIT-KILL? is non-nil, don't kill this workspace's buffers.

From Doom's `+workspace-delete' in
\"modules/ui/workspaces/autoload/workspaces.el\"."
  (unless (stringp workspace)
    (setq workspace (persp-name workspace)))
  (when (int<perspective>:protected? workspace)
    (error "Can't delete '%s' workspace" workspace))
  (perspective:get workspace) ; error checking
  (persp-kill workspace inhibit-kill?)
  (not (perspective:exists? workspace)))


;;;###autoload
(defun perspective:action:switch (name &optional auto-create?)
  "Switch to another workspace named NAME (a string).

If AUTO-CREATE? is non-nil, create the workspace if it doesn't exist, otherwise
throws an error.

From Doom's `+workspace-switch' in
\"modules/ui/workspaces/autoload/workspaces.el\"."
  (unless (perspective:exists? name)
    (if auto-create?
        (perspective:action:new name)
      (error "%s is not an available workspace" name)))
  (let ((old-name (perspective:name:current)))
    (unless (equal old-name name)
      (setq int<perspective>:last
            (or (and (not (string= old-name persp-nil-name))
                     old-name)
                perspective:name:default))
      (persp-frame-switch name))
    (equal (perspective:name:current) name)))


;;;###autoload
(defun perspective:action:open (&optional name clone?)
  "Open a (new?) workspace named NAME.

If NAME is empty/nil, generate a unique name.

If CLONE? is non-nil, clone the current workspace, otherwise the new workspace
is blank.

From Doom's `+workspace/new' in
\"modules/ui/workspaces/autoload/workspaces.el\"."
  (unless name
    (setq name (int<perspective>:name:generate)))
  (condition-case e
      (cond ((perspective:exists? name)
             (error "%s already exists" name))
            (clone? (persp-copy name t))
            (t
             (perspective:action:switch name t)
             (perspective:cmd:display)))
    ((debug error) (perspective:out:error (cadr e) t))))


;;------------------------------------------------------------------------------
;; Commands
;;------------------------------------------------------------------------------

;;;###autoload
(defun perspective:cmd:load (name)
  "Load workspace NAME and switch to it.

If called with the prefix argument, try to reload the current workspace from
session files.

From Doom's `+workspace/load' in
\"modules/ui/workspaces/autoload/workspaces.el\"."
  (interactive
   (list
    (if current-prefix-arg
        (perspective:name:current)
      (completing-read
       "Workspace to load: "
       (persp-list-persp-names-in-file
        (expand-file-name perspective:filename:data persp-save-dir))))))
  (if (not (perspective:action:load name))
      (perspective:out:error (format "Couldn't load workspace %s" name))
    (perspective:cmd:switch/index name)
    (perspective:cmd:display)))


;;;###autoload
(defun perspective:cmd:save (name)
  "Save the current workspace as NAME.

If called with the prefix argument, autosave the current workspace.

From Doom's `+workspace/save' in
\"modules/ui/workspaces/autoload/workspaces.el\"."
  (interactive
   (list
    (if current-prefix-arg
        (perspective:name:current)
      (completing-read "Workspace to save: " (perspective:name:list)))))
  (if (perspective:action:save name)
      (perspective:out:message (format "'%s' workspace saved" name) 'success)
    (perspective:out:error (format "Couldn't save workspace %s" name))))


;;;###autoload
(defun perspective:cmd:rename (new-name)
  "Rename the current workspace to NEW-NAME.

From Doom's `+workspace/rename' in
\"modules/ui/workspaces/autoload/workspaces.el\"."
  (interactive (list (read-from-minibuffer "New workspace name: ")))
  (condition-case-unless-debug ex
      (let* ((current-name (perspective:name:current))
             (old-name (perspective:action:rename current-name new-name)))
        (unless old-name
          (error "Failed to rename %s" current-name))
        (perspective:out:message (format "Renamed '%s'->'%s'" old-name new-name) 'success))
    ('error (perspective:out:error ex t))))


;;;###autoload
(defun perspective:cmd:delete (name)
  "Delete the NAME workspace.

If called with the prefix argument, prompts you for the name of the workspace to
delete.

From Doom's `+workspace/delete' in
\"modules/ui/workspaces/autoload/workspaces.el\"."
  (interactive
   (let ((current-name (perspective:name:current)))
     (list
      (if current-prefix-arg
          (completing-read (format "Delete workspace (default: %s): " current-name)
                           (perspective:name:list)
                           nil nil nil nil current-name)
        current-name))))
  (condition-case-unless-debug ex
      ;; REVIEW refactor me
      (let ((workspaces (perspective:name:list)))
        (if (not (member name workspaces))
            (perspective:out:message (format "'%s' workspace doesn't exist" name) 'warn)
          ;; Sanity Check
          (cond ((delq (selected-frame) (persp-frames-with-persp (get-frame-persp)))
                 (user-error "Can't close workspace, it's visible in another frame"))
                ;; Delete some other perspective.
                ((not (equal (perspective:name:current) name))
                 (perspective:action:delete name))
                ;; Delete one of N perspectives (N>1).
                ((cdr workspaces)
                 (perspective:action:delete name)
                 (perspective:action:switch
                  (if (perspective:exists? int<perspective>:last)
                      int<perspective>:last
                    (car (perspective:name:list))))
                 ; doom: (unless (doom-buffer-frame-predicate (window-buffer))
                 (unless (or (buffer:type:real? (window-buffer))
                             (eq (window-buffer) (buffer:fallback:get :create)))
                   (switch-to-buffer (buffer:fallback:get :create))))
                ;; Delete the only perspective.
                (t
                 ;; Go to the default perspective...
                 (perspective:action:switch perspective:name:default t)
                 ;; Delete that final perspective.
                 (unless (string= (car workspaces) perspective:name:default)
                   (perspective:action:delete name))
                 ;; ...and kill all buffers?
                 ;;   - Either all buffers for the perspective, or just ALL buffers...
                 (buffer:kill:list (buffer:list))))
          (perspective:out:message (format "Deleted '%s' workspace" name) 'success)))
    ('error (perspective:out:error ex t))))


;;;###autoload
(defun perspective:cmd:kill (&optional message?)
  "Delete all workspaces, windows, and their buffers.

If MESSAGE? is non-nil, output a message about how many of each were killed.

From Doom's `+workspace/kill-session' in
\"modules/ui/workspaces/autoload/workspaces.el\"."
  ;; If we were called interactively, we should probably always output the message?
  (interactive (list t))
  (let ((windows (length (window-list)))
        (persps (length (perspective:name:list)))
        (buffers 0))
    (let ((persp-autokill-buffer-on-remove t))
      (unless (cl-every #'perspective:action:delete (perspective:name:list))
        (perspective:out:error "Could not kill workspace")))
    (perspective:action:switch perspective:name:default t)
    ;; TODO: Doom's `doom/kill-all-buffers' also closes windows?
    (setq buffers (buffer:kill:list (buffer-list)))
    (when (called-interactively-p)
      (message "Killed %d workspace(s), %d window(s) & %d buffer(s)"
               persps windows buffers))))


;;;###autoload
(defun perspective:cmd:new/named (&optional name clone?)
  "Create a new workspace named NAME.

If NAME is empty/nil, generate a unique name.

If CLONE? is non-nil, clone the current workspace, otherwise the new workspace
is blank. CLONE? is the prefix argument when called interactively.

From Doom's `+workspace/new' in
\"modules/ui/workspaces/autoload/workspaces.el\"."
  (interactive "sWorkspace Name (Optional): \nP")
  (perspective:action:open name clone?))


;;;###autoload
(defun perspective:cmd:new (&optional clone?)
  "Create a new workspace with a generated, unique name.

If CLONE? is non-nil, clone the current workspace, otherwise the new workspace
is blank.

From Doom's `+workspace/new' in
\"modules/ui/workspaces/autoload/workspaces.el\"."
  (interactive "P")
  (perspective:action:open nil clone?))


;;;###autoload
(defun perspective:cmd:switch/index (index)
  "Switch to a workspace at a given INDEX.

A negative number will start from the end of the workspace list.

From Doom's `+workspace/switch-to' in
\"modules/ui/workspaces/autoload/workspaces.el\"."
  (interactive
   (list (or current-prefix-arg
             (completing-read "Switch to workspace: " (perspective:name:list)))))
  (when (and (stringp index)
             (string-match-p "^[0-9]+$" index))
    (setq index (string-to-number index)))
  (condition-case-unless-debug ex
      (let ((names (perspective:name:list))
            (old-name (perspective:name:current)))
        (cond ((numberp index)
               (let ((dest (nth index names)))
                 (unless dest
                   (error "No workspace at #%s" (1+ index)))
                 (perspective:action:switch dest)))
              ((stringp index)
               (perspective:action:switch index t))
              (t
               (error "Not a valid index: %s" index)))
        (unless (called-interactively-p 'interactive)
          (if (equal (perspective:name:current) old-name)
              (perspective:out:message (format "Already in %s" old-name) 'warn)
            (perspective:cmd:display))))
    ('error (perspective:out:error (cadr ex) t))))


;;;###autoload
(dotimes (i 9)
  (defalias (intern (format "perspective:cmd:switch/index:%d" i))
    (lambda () (interactive) (perspective:cmd:switch/index i))
    (format "Switch to workspace #%d" (1+ i))))


;;;###autoload
(defun perspective:cmd:switch/final ()
  "Switch to the final workspace in open workspaces.

From Doom's `+workspace/switch-to-final' in
\"modules/ui/workspaces/autoload/workspaces.el\"."
  (interactive)
  (perspective:cmd:switch/index (car (last (perspective:name:list)))))


;;;###autoload
(defun perspective:cmd:switch/last ()
  "Switch to the last activated workspace.

From Doom's `+workspace/other' in
\"modules/ui/workspaces/autoload/workspaces.el\"."
  (interactive)
  (perspective:cmd:switch/index int<perspective>:last))


;; ###autoload
(defun perspective:cmd:cycle (n)
  "Cycle N workspaces to the right or left.

N > 0: cycle right
N < 0: cycle left

From Doom's `+workspace/cycle' in
\"modules/ui/workspaces/autoload/workspaces.el\"."
  (interactive (list 1))
  (let ((current-name (perspective:name:current)))
    (if (equal current-name persp-nil-name)
        (perspective:action:switch perspective:name:default t)
      (condition-case-unless-debug ex
          (let* ((persps (perspective:name:list))
                 (perspc (length persps))
                 (index (cl-position current-name persps)))
            (when (= perspc 1)
              (user-error "No other workspaces"))
            (perspective:cmd:switch/index (% (+ index n perspc) perspc))
            (unless (called-interactively-p 'interactive)
              (perspective:cmd:display)))
        ('user-error (perspective:out:error (cadr ex) t))
        ('error (perspective:out:error ex t))))))


;;;###autoload
(defun perspective:cmd:cycle/left ()
  "Cycle one workspace to the left.

From Doom's `+workspace/switch-left' in
\"modules/ui/workspaces/autoload/workspaces.el\"."
  (interactive)
  (perspective:cmd:cycle -1))


;;;###autoload
(defun perspective:cmd:cycle/right ()
  "Cycle one workspace to the left.

From Doom's `+workspace/switch-right' in
\"modules/ui/workspaces/autoload/workspaces.el\"."
  (interactive)
  (perspective:cmd:cycle +1))


;;;###autoload
(defun perspective:cmd:swap/left (&optional count)
  "Swap the current workspace with the COUNTth workspace on its left.

From Doom's `+workspace/swap-left' in
\"modules/ui/workspaces/autoload/workspaces.el\"."
  (interactive "p")
  (let* ((current-name (perspective:name:current))
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
      (perspective:cmd:display))))


;;;###autoload
(defun perspective:cmd:swap/right (&optional count)
  "Swap the current workspace with the COUNTth workspace on its right.

From Doom's `+workspace/swap-right' in
\"modules/ui/workspaces/autoload/workspaces.el\"."
  (interactive "p")
  (funcall-interactively #'perspective:cmd:swap/left (- count)))


;;------------------------------------------------------------------------------
;; Tabs Display In Minibuffer
;;------------------------------------------------------------------------------

(defun int<perspective>:format:tabline (&optional names)
  "Format workspace NAMES into a display string.

From Doom's `+workspace--tabline' in
\"modules/ui/workspaces/autoload/workspaces.el\"."
  (let ((names (or names (perspective:name:list)))
        (current-name (perspective:name:current)))
    (mapconcat
     #'identity
     (cl-loop for name in names
              for i to (length names)
              collect
              (propertize (format " [%d] %s " (1+ i) name)
                          'face (if (equal current-name name)
                                    'perspective:face:tab/selected
                                  'perspective:face:tab)))
     " ")))


;;;###autoload
(defun perspective:cmd:display ()
  "Display a list of workspaces (like tabs) in the echo area.

From Doom's `+workspace/display' in
\"modules/ui/workspaces/autoload/workspaces.el\"."
  (interactive)
  (let (message-log-max)
    (message "%s" (int<perspective>:format:tabline))))


;;------------------------------------------------------------------------------
;; Messaging Helpers
;;------------------------------------------------------------------------------

(defun int<perspective>:out:body (message &optional type)
  "Propertize and prefix MESSAGE for display in the modeline.

Prefix MESSAGE with function `int<perspective>:format:tabline'.

Propertize MESSAGE according to TYPE:
  - `error'
  - `warn'
  - `success'
  - `info'

From Doom's `+workspace--message-body' in
\"modules/ui/workspaces/autoload/workspaces.el\"."
  (concat (int<perspective>:format:tabline)
          (propertize " | " 'face 'font-lock-comment-face)
          (propertize (format "%s" message)
                      'face (pcase type
                              ('error 'error)
                              ('warn 'warning)
                              ('success 'success)
                              ('info 'font-lock-comment-face)))))


;;;###autoload
(defun perspective:out:message (message &optional type)
  "Show an 'elegant' MESSAGE in the echo area next to a listing of workspaces.

Prefix MESSAGE with function `int<perspective>:format:tabline'.

Propertize MESSAGE according to TYPE:
  - `error'
  - `warn'
  - `success'
  - `info'

From Doom's `+workspace-message' in
\"modules/ui/workspaces/autoload/workspaces.el\"."
  (message "%s" (int<perspective>:out:body message type)))


;;;###autoload
(defun perspective:out:error (message &optional no-error?)
  "Show an 'elegant' error in the echo area next to a listing of workspaces.

Prefix MESSAGE with function `int<perspective>:format:tabline'.

Propertize MESSAGE according to TYPE:
  - `error'
  - `warn'
  - `success'
  - `info'

Will print using `error', unless NO-ERROR? is non-nil, in which case will print
via `message'.

From Doom's `+workspace-error' in
\"modules/ui/workspaces/autoload/workspaces.el\"."
  (funcall (if no-error? #'message #'error)
           "%s" (int<perspective>:out:body message 'error)))


;;------------------------------------------------------------------------------
;; Hooks
;;------------------------------------------------------------------------------

;;;###autoload
(defun perspective:hook:associated/delete (&optional frame)
  "Delete workspace associated with current FRAME.

A workspace gets associated with a frame when a new frame is interactively
created.

Example for `use-package persp-mode' `:config' section:
  ;; per-frame workspaces
  (setq persp-init-frame-behaviour t
        persp-init-new-frame-behaviour-override nil
        persp-interactive-init-frame-behaviour-override #'perspective:hook:associted/create
        persp-emacsclient-init-frame-behaviour-override #'perspective:hook:associted/create)
  (add-hook 'delete-frame-functions #'perspective:hook:associated/delete)
  (add-hook 'server-done-hook #'perspective:hook:associated/delete)

From Doom's `+workspaces-delete-associated-workspace-h' in
\"modules/ui/workspaces/autoload/workspaces.el\"."
  (when (and persp-mode (not (bound-and-true-p with-editor-mode)))
    (unless frame
      (setq frame (selected-frame)))
    (let ((frame-persp (frame-parameter frame 'workspace)))
      (when (string= frame-persp (perspective:name:current))
        (perspective:cmd:delete frame-persp)))))


;;;###autoload
(defun perspective:hook:associted/create (frame &optional _new-frame?)
  "Create a blank, new perspective and associate it with FRAME.

Example for `use-package persp-mode' `:config' section:
  ;; per-frame workspaces
  (setq persp-init-frame-behaviour t
        persp-init-new-frame-behaviour-override nil
        persp-interactive-init-frame-behaviour-override #'perspective:hook:associted/create
        persp-emacsclient-init-frame-behaviour-override #'perspective:hook:associted/create)
  (add-hook 'delete-frame-functions #'perspective:hook:associated/delete)
  (add-hook 'server-done-hook #'perspective:hook:associated/delete)

From Doom's `+workspaces-associate-frame-fn' in
\"modules/ui/workspaces/autoload/workspaces.el\"."
  (when persp-mode
    (if (not (persp-frame-list-without-daemon))
        (perspective:action:switch perspective:name:default t)
      (with-selected-frame frame
        (perspective:action:switch (int<perspective>:name:generate) t)
        (unless (buffer:type:real? (current-buffer))
          (switch-to-buffer (buffer:fallback:get :create)))
        (set-frame-parameter frame 'workspace (perspective:name:current))
        ;; ensure every buffer has a buffer-predicate
        (persp-set-frame-buffer-predicate frame))
      (run-at-time 0.1 nil #'perspective:cmd:display))))


;;------------------------------------------------------------------------------
;; Keybinds
;;------------------------------------------------------------------------------

;; TODO-meow: Are these bound? Where?

;;------------------------------
;; Generic
;;------------------------------

(defvar perspective:keymap
  ;; Create map...
  (let ((map (make-sparse-keymap)))
    ;; Add keys to map...
    (define-key map (kbd "TAB") #'perspective:cmd:display)
    (define-key map (kbd "n")   #'perspective:cmd:new/named)
    (define-key map (kbd "N")   #'perspective:cmd:new)
    (define-key map (kbd "l")   #'perspective:cmd:load)
    (define-key map (kbd "s")   #'perspective:cmd:save)
    ;; (define-key map (kbd "x")   #'perspective:cmd:kill)
    (define-key map (kbd "d")   #'perspective:cmd:delete)
    (define-key map (kbd "r")   #'perspective:cmd:rename)
    (define-key map (kbd "]")   #'perspective:cmd:cycle/right)
    (define-key map (kbd "[")   #'perspective:cmd:cycle/left)
    (define-key map (kbd ".")   #'perspective:cmd:switch/index)
    (define-key map (kbd "`")   #'perspective:cmd:switch/last)
    (define-key map (kbd "1")   #'perspective:cmd:switch/index:0)
    (define-key map (kbd "2")   #'perspective:cmd:switch/index:1)
    (define-key map (kbd "3")   #'perspective:cmd:switch/index:2)
    (define-key map (kbd "4")   #'perspective:cmd:switch/index:3)
    (define-key map (kbd "5")   #'perspective:cmd:switch/index:4)
    (define-key map (kbd "6")   #'perspective:cmd:switch/index:5)
    (define-key map (kbd "7")   #'perspective:cmd:switch/index:6)
    (define-key map (kbd "8")   #'perspective:cmd:switch/index:7)
    (define-key map (kbd "9")   #'perspective:cmd:switch/index:8)
    (define-key map (kbd "0")   #'perspective:cmd:switch/final)
    ;; ...and return the map.
    map)
  "Bind this keymap to a key of your choice.")

;; TODO-emacs-29: Change to using `defvar-keymap'.
;; E.g.: https://github.com/minad/vertico/blob/main/vertico.el


;;------------------------------
;; General
;;------------------------------
;; NOTE: Cannot figure out how to get something working that can be used in a
;; `use-package' `:general' block. So I'm just doing it the dumb KISS way.
;;
;; (keybind:leader/global:def
;;   :infix   "TAB"
;;   "" (list nil :which-key "Perspectives...")
;;   "TAB" (list #'perspective:cmd:display        :which-key "List")
;;   "n"   (list #'perspective:cmd:new/named      :which-key "New")
;;   "N"   (list #'perspective:cmd:new            :which-key "New Unnamed")
;;   "l"   (list #'perspective:cmd:load           :which-key "Load")
;;   "s"   (list #'perspective:cmd:save           :which-key "Save")
;;   ;; "x"   (list #'perspective:cmd:kill           :which-key "Kill All")
;;   "d"   (list #'perspective:cmd:delete         :which-key "Delete")
;;   "r"   (list #'perspective:cmd:rename         :which-key "Rename")
;;   "]"   (list #'perspective:cmd:cycle/right    :which-key "Cycle: Right")
;;   "["   (list #'perspective:cmd:cycle/left     :which-key "Cycle: Left")
;;   "."   (list #'perspective:cmd:switch/index   :which-key "Switch: _")
;;   "`"   (list #'perspective:cmd:switch/last    :which-key "Switch: Previous")
;;   "1"   (list #'perspective:cmd:switch/index:0 :which-key "Switch: 1st")
;;   "2"   (list #'perspective:cmd:switch/index:1 :which-key "Switch: 2nd")
;;   "3"   (list #'perspective:cmd:switch/index:2 :which-key "Switch: 3rd")
;;   "4"   (list #'perspective:cmd:switch/index:3 :which-key "Switch: 4th")
;;   "5"   (list #'perspective:cmd:switch/index:4 :which-key "Switch: 5th")
;;   "6"   (list #'perspective:cmd:switch/index:5 :which-key "Switch: 6th")
;;   "7"   (list #'perspective:cmd:switch/index:6 :which-key "Switch: 7th")
;;   "8"   (list #'perspective:cmd:switch/index:7 :which-key "Switch: 8th")
;;   "9"   (list #'perspective:cmd:switch/index:8 :which-key "Switch: 9th")
;;   "0"   (list #'perspective:cmd:switch/final   :which-key "Switch: Final"))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :perspective 'perspective)

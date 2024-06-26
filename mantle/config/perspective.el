;;; mantle/config/perspective.el --- Perspectives, Workspaces... Whatever. -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2022-11-29
;; Timestamp:  2023-10-20
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Perspectives, Workspaces... Whatever.
;;
;; https://github.com/Bad-ptr/persp-mode.el
;;
;; `persp-mode' gives "perspectives", a perspective-restricted `buffer-list', and
;; file-based session persistence.
;;
;;; Code:


(imp:require :buffer 'type)

;;------------------------------------------------------------------------------
;; Module: Perspective
;;------------------------------------------------------------------------------
;; Helpers for `persp-mode'
(imp:load :feature  '(:perspective)
          :path     (imp:path:join innit:path:module "emacs" "perspective")
          :filename "init")


;;------------------------------------------------------------------------------
;; Persp-Mode
;;------------------------------------------------------------------------------

(imp:use-package persp-mode
  :unless noninteractive
  ;; Want `persp-mode' quite early, and all the
  ;; "modules/emacs/perspective/perspective.el" functions need it and I don't
  ;; feel like figuring out autoloading everything that uses...
  :demand t

  ;; :commands persp-switch-to-buffer

  ;;------------------------------
  :init
  ;;------------------------------

  ;;---
  ;; Hooks
  ;;---

  ;; The default perspective persp-mode creates is special and doesn't represent
  ;; a real persp object, so buffers can't really be assigned to it, among other
  ;; quirks, so... replace it with a "main" perspective.
  (innit:hook:defun
      (:name    "persp:perspective:replace-nil"
       :docstr  (concat "The default perspective that 'persp-mode' creates is special and doesn't "
                        "represent a real persp object, so buffers can't really be assigned to "
                        "it, among other quirks, so... replace it with a \"main\" perspective."))
    (when persp-mode
      (dolist (frame (frame-list))
        (when (string= (safe-persp-name (get-current-persp frame)) persp-nil-name)
          ;; Take extra steps to ensure no frame ends up in the nil perspective
          (persp-frame-switch (or (cadr (hash-table-keys *persp-hash*))
                                  perspective:name:default)
                              frame)))))

  (innit:hook:defun
      (:name    "persp:perspective:init-first"
       :docstr  (concat "Ensure a main perspective exists."))
    (when persp-mode
      (let (persp-before-switch-functions)
        ;; Try our best to hide the nil perspective...
        (when (equal (car persp-names-cache) persp-nil-name)
          (pop persp-names-cache))
        ;; ...and create a *real* main perspective to fill this role.
        (unless (or (persp-get-by-name perspective:name:default)
                    ;; Start from 2 b/c persp-mode counts the nil perspective
                    (> (hash-table-count *persp-hash*) 2))
          (persp-add-new perspective:name:default))
        ;; HACK Fix #319: the warnings buffer gets swallowed when creating
        ;;      `perspective:name:default', so display it ourselves, if it exists.
        (when-let (warnings (get-buffer "*Warnings*"))
          (save-excursion
            (display-buffer-in-side-window warnings
                                           '((window-height . shrink-window-if-larger-than-buffer))))))))

  (innit:hook:defun
      (:name    "persp:uniquify:init-hack"
       :docstr "Hack around `uniquify' buffer renaming to keep `persp-mode' working.")
    ;; `uniquify' breaks persp-mode. It renames old buffers, which causes errors
    ;; when switching between perspective (their buffers are serialized by name
    ;; and persp-mode expects them to have the same name when restored).
    (cond (persp-mode
           ;; Cache & disable `uniquify' buffer renaming.
           (when (bound-and-true-p uniquify-buffer-name-style)
             (setq perspective:cache:uniquify-buffer-name-style uniquify-buffer-name-style))
           (setq uniquify-buffer-name-style nil)

           ;; Ensure `persp-kill-buffer-query-function' is last
           (remove-hook 'kill-buffer-query-functions #'persp-kill-buffer-query-function)
           (add-hook 'kill-buffer-query-functions #'persp-kill-buffer-query-function t))

          ;; Disabling `persp-mode': re-enable `uniquify' buffer renaming.
          (t
           (when perspective:cache:uniquify-buffer-name-style
             (setq uniquify-buffer-name-style perspective:cache:uniquify-buffer-name-style)))))


  (innit:hook:defun
      (:name    "persp:winner:data/save"
       :docstr "Save `winner' perspective data?")
    (when (and (bound-and-true-p winner-mode)
               (get-current-persp))
      (set-persp-parameter 'winner-ring (list winner-currents
                                              winner-ring-alist
                                              winner-pending-undo-ring))))

  (innit:hook:defun
      (:name    "persp:winner:data/load"
       :docstr "Load `winner' perspective data?")
    (when (bound-and-true-p winner-mode)
      (cl-destructuring-bind
          (currents alist pending-undo-ring)
          (or (persp-parameter 'winner-ring) (list nil nil nil))
        (setq winner-undo-frame nil
              winner-currents currents
              winner-ring-alist alist
              winner-pending-undo-ring pending-undo-ring))))

  (innit:hook:defun
      (:name    "persp:buffer:add-current"
       :docstr "Add current buffer to focused perspective.")
    (or (not (bound-and-true-p persp-mode))
        (persp-buffer-filtered-out-p
         (or (buffer-base-buffer (current-buffer))
             (current-buffer))
         persp-add-buffer-on-after-change-major-mode-filter-functions)
        (persp-add-buffer (current-buffer) (get-current-persp) nil nil)))

  (innit:hook:defun
      (:name     "persp:buffer:ignore/dead"
       :argslist (buffer)
       :docstr   "Don't try to persist dead buffers. They cause errors.")
      ;; Fix bug: Ignore dead buffers in `persp-mode' buffer list
    (not (buffer-live-p buffer)))

  (innit:hook:defun
      (:name     "persp:buffer:ignore/remote"
       :argslist (buffer)
       :docstr   "Don't try to persist remote buffers. They are super slow.")
      ;; Don't save TRAMP buffers; they're super slow to restore.
      (let ((dir (buffer-local-value 'default-directory buffer)))
        (ignore-errors (file-remote-p dir))))


  ;;---
  ;; Filters
  ;;---

  (innit:hook:defun
      (:name     "persp:buffer/filter:on-major-mode-change"
       :argslist (buffer-or-name)
       :docstr   (mapconcat #'identity
                            '("Do _not_ add BUFFER to current perspective?"
                              ""
                              "In the `use-package' `:hook':"
                              "  (persp-add-buffer-on-after-change-major-mode-filter-functions . mantle:persp/major-mode-hook:add-buffer-filter)")
                            "\n"))
    ;; NOTE: Filtering _out_ buffers so:
    ;;   - nil     = Want.
    ;;   - non-nil = Do not want.
    (cond
     ;;------------------------------
     ;; "Real" buffers are fine...
     ;;------------------------------
     ((buffer:type:real? buffer-or-name)
      nil) ; Do add buffer to persp.

     ;;------------------------------
     ;; "Unreal" buffers are usually not desired.
     ;;------------------------------
     ;; Major-Mode Exceptions:
     ((memq major-mode '(deadgrep-mode
                         magit-status-mode
                         helpful-mode))
      nil) ; Do add buffer to persp.

     ;; Any other unreal buffer: don't want.
     (t)))
  ;; (setq persp-add-buffer-on-after-change-major-mode-filter-functions '(mantle:hook:persp:buffer/filter:on-major-mode-change))


  ;;------------------------------
  :hook
  ;;------------------------------
  ;; Note: Use `innit:cmd:hook:func/name' to insert the func names created via the `innit:hook:defun' `:name' field.
  (((persp-mode-hook persp-after-load-state-functions) . mantle:hook:persp:perspective:replace-nil)
   (persp-mode-hook . mantle:hook:persp:perspective:init-first)
   (persp-mode-hook . mantle:hook:persp:uniquify:init-hack)
   (persp-before-deactivate-functions . mantle:hook:persp:winner:data/save)
   (persp-activated-functions . mantle:hook:persp:winner:data/load)
   ;; `window-buffer-change-functions' doesn't trigger for files visited via the
   ;; server, so also include `server-visit-hook'.
   ((window-buffer-change-functions server-visit-hook) . mantle:hook:persp:buffer:add-current)
   ;; Do *not* add buffer to perspective if function returns non-nil.
   (persp-add-buffer-on-after-change-major-mode-filter-functions . mantle:hook:persp:buffer/filter:on-major-mode-change)
   ;; Fix bug: Visual selection surviving perspective changes.
   (persp-before-deactivate-functions . deactivate-mark)
   (persp-filter-save-buffers-functions . (mantle:hook:persp:buffer:ignore/dead mantle:hook:persp:buffer:ignore/remote))
   ;; Otherwise, buffers opened via bookmarks aren't treated as "real" and are
   ;; excluded from the buffer list.
   (bookmark-after-jump-hook . mantle:hook:persp:buffer:add-current))


  ;;------------------------------
  :custom
  ;;------------------------------

  ;; `persp-save-dir': Set by `no-littering'.

  (persp-autokill-buffer-on-remove 'kill-weak)
  (persp-reset-windows-on-nil-window-conf nil)
  (persp-nil-hidden t)
  (persp-set-last-persp-for-new-frames t)
  (persp-switch-to-added-buffer nil)

  ;; Options: `persp-kill-foreign-buffer-behaviour-choices'
  ;;   - `ask' - "Ask what to do"
  ;;   - `dont-ask-weak' - "Don't ask if a buffer belongs only to weak perspectives"
  ;;   - `kill' - "Just kill"
  ;;   - `nil' - "Do not suggest foreign buffer to the user(kill buffer)"
  ;;   - `lambda' - "Run function"
  ;; Default: `dont-ask-weak'
  (persp-kill-foreign-buffer-behaviour 'kill)

  ;; Options:
  ;;   - `ask-to-rem-from-all' - "Ask to remove from all perspectives"
  ;;   - `ask-if-in-non-weak-persp' - "Ask only if buffer belongs to a non-weak perspective"
  ;;   - `nil' - "Don't ask"
  ;;   - lambda  - "Run this function"
  ;; Default: `ask-to-rem-from-all'
  (persp-remove-buffers-from-nil-persp-behaviour nil)

  (persp-auto-resume-time -1) ; Don't auto-resume on startup.

  ;; 0 -- do not auto save
  ;; 1 -- save on the emacs shutdown and only if the persp-mode active
  ;; 2 -- save on the persp-mode deactivation or the emacs shutdown
  (persp-auto-save-opt (if noninteractive 0 1))

  ;; per-frame perspectives
  (persp-init-frame-behaviour t)                ; "restore window configuration"
  (persp-init-new-frame-behaviour-override nil) ; "do not restore window configuration"


  ;;------------------------------
  :config
  ;;------------------------------

  ;; Per-perspective `winner-mode' history
  (add-to-list 'window-persistent-parameters '(winner-ring . t))

  ;; Replace an evil func with a perspective-aware varient.
  (when (imp:mode? 'evil-mode)
    (define-advice evil-alternate-buffer (:override (&optional window) mantle:user:persp:alternate-buffer)
      "Make `evil-alternate-buffer' ignore buffers outside the current perspective."
      (let* ((prev-buffers
              (if persp-mode
                  (cl-remove-if-not #'persp-contain-buffer-p (window-prev-buffers)
                                    :key #'car)
                (window-prev-buffers)))
             (head (car prev-buffers)))
        (if (eq (car head) (window-buffer window))
            (cadr prev-buffers)
          head))))

  ;; HACK: Fix: Selecting deleted buffer error when quitting Emacs or on some buffer listing ops.
  (define-advice persp-buffers-to-savelist (:before (persp) mantle:user:persp:remove-dead-buffers)
    "Remove dead buffers before `persp' can save them.

Fixes a bug in `persp' where selecting deleted buffer errors when quitting Emacs
or on some buffer listing ops."
    (when (perspective-p persp)
      ;; HACK: Can't use `persp-buffers' because of a race condition with its gv
      ;;      getter/setter not being defined in time.
      (setf (aref persp 2)
            (cl-delete-if-not #'persp-get-buffer-or-null (persp-buffers persp)))))

  (define-advice persp-asave-on-exit (:around (fn &rest args) mantle:user:persp:autosave:real-buffers?)
    "Don't bother auto-saving the session if no real buffers are open."
    (when (buffer:list :type :real)
      (apply fn args))
    t)

  ;; Fix bug: Stop session persistence from restoring a broken posframe.
  (imp:eval:after posframe
    (innit:hook:defun-and-add
        persp-after-load-state-functions
        (:name   "persp:state/load"
         :docstr "Fix bug: Stop session persistence from restoring a broken posframe."
         ;; :squelch t ;; TODO: Do I need to squelch?
         )
      (posframe-delete-all)))

  ;; Enable `persp-mode'!
  (persp-mode +1))

;;------------------------------
;; `marginalia' & `persp-mode'
;;------------------------------
(imp:use-package persp-mode
  :unless noninteractive
  :after marginalia

  ;;------------------------------
  :config
  ;;------------------------------

  ;; `persp-switch-to-buffer', by default, is not getting sent through
  ;; `marginalia' for additional annotations in the minibuffer. Tell
  ;; `marginalia' that `persp-switch-to-buffer' is a buffer command, since it
  ;; doesn't have metadata about itself or something?
  (add-to-list 'marginalia-command-categories '(persp-switch-to-buffer . buffer)))


;;------------------------------------------------------------------------------
;; Keybinds : Meow
;;------------------------------------------------------------------------------

(imp:use-package persp-mode
  :when  (imp:flag? :keybinds +meow)
  :unless noninteractive
  :after  meow

  ;;------------------------------
  :config
  ;;------------------------------

  ;;------------------------------
  ;; `General'
  ;;------------------------------
  (defun mantle:meow/keybind/general:perspective ()
    "Create the \"File...\" keybinds in `general' for `meow'."
    ;;---
    ;; "Buffer" Keybinds
    ;;---
    (keybind:leader/global:def
      :infix   "b"
      "b" (list #'persp-switch-to-buffer :which-key "Switch Perspective Buffer"))

    ;;---
    ;; "Perspective" Keybinds
    ;;---
    (keybind:leader/global:def
      :infix   "TAB"
      "" (list nil :which-key "Perspectives...")
      "TAB" (list #'perspective:cmd:display        :which-key "List")
      "n"   (list #'perspective:cmd:new/named      :which-key "New...")
      "N"   (list #'perspective:cmd:new            :which-key "New Unnamed")
      "l"   (list #'perspective:cmd:load           :which-key "Load")
      "s"   (list #'perspective:cmd:save           :which-key "Save")
      ;; "x"   (list #'perspective:cmd:kill           :which-key "Kill All")
      "d"   (list #'perspective:cmd:delete         :which-key "Delete")
      "r"   (list #'perspective:cmd:rename         :which-key "Rename")
      "]"   (list #'perspective:cmd:cycle/right    :which-key "Cycle: Right")
      "["   (list #'perspective:cmd:cycle/left     :which-key "Cycle: Left")
      "."   (list #'perspective:cmd:switch/index   :which-key "Switch: Index...")
      "`"   (list #'perspective:cmd:switch/last    :which-key "Switch: Previous")
      "1"   (list #'perspective:cmd:switch/index:0 :which-key "Switch: 1st")
      "2"   (list #'perspective:cmd:switch/index:1 :which-key "Switch: 2nd")
      "3"   (list #'perspective:cmd:switch/index:2 :which-key "Switch: 3rd")
      "4"   (list #'perspective:cmd:switch/index:3 :which-key "Switch: 4th")
      "5"   (list #'perspective:cmd:switch/index:4 :which-key "Switch: 5th")
      "6"   (list #'perspective:cmd:switch/index:5 :which-key "Switch: 6th")
      "7"   (list #'perspective:cmd:switch/index:6 :which-key "Switch: 7th")
      "8"   (list #'perspective:cmd:switch/index:7 :which-key "Switch: 8th")
      "9"   (list #'perspective:cmd:switch/index:8 :which-key "Switch: 9th")
      "0"   (list #'perspective:cmd:switch/final   :which-key "Switch: Final")))


  ;;------------------------------
  ;; `Transient'
  ;;------------------------------

  (defun mantle:meow/keybind/transient:perspective ()
    "Create the \"File...\" keybinds in `transient' for `meow'."
    ;; Replace "Switch Buffer" with `persp-mode' aware function.
    ;; See: "mantle/config/keybinds/buffer.el" for buffer transient.
    (transient-replace-suffix 'mantle:meow/transient:buffer
      "b"
      '("b" "Switch Perspective Buffer" persp-switch-to-buffer))
    ;; (transient-get-suffix 'mantle:meow/transient:buffer "b")

    ;; Transient for common persp-mode commands.
    (transient-define-prefix mantle:meow/transient:perspective ()
      "Keymap for perspective commands that should be available globally."
      ["Perspectives..."
       [:description ""
        ("TAB" "List" perspective:cmd:display)
        ("n"   "New..." perspective:cmd:new/named)
        ("N"   "New Unnamed" perspective:cmd:new)
        ("l"   "Load" perspective:cmd:load)
        ("s"   "Save" perspective:cmd:save)
        ;; ("x"   "Kill All" perspective:cmd:kill)
        ("d"   "Delete" perspective:cmd:delete)
        ("r"   "Rename" perspective:cmd:rename)
        ("]"   "Cycle: Right" perspective:cmd:cycle/right)
        ("["   "Cycle: Left" perspective:cmd:cycle/left)]
       [:description "Switch Indexed..."
        ("1"  "Switch: 1st"         perspective:cmd:switch/index:0)
        ("2"  "Switch: 2nd"         perspective:cmd:switch/index:1)
        ("3"  "Switch: 3rd"         perspective:cmd:switch/index:2)
        ("4"  "Switch: 4th"         perspective:cmd:switch/index:3)
        ("5"  "Switch: 5th"         perspective:cmd:switch/index:4)
        ("6"  "Switch: 6th"         perspective:cmd:switch/index:5)
        ("7"  "Switch: 7th"         perspective:cmd:switch/index:6)
        ("8"  "Switch: 8th"         perspective:cmd:switch/index:7)
        ("9"  "Switch: 9th"         perspective:cmd:switch/index:8)]
       [:description "Switch..."
        ("0"  "Switch to Final"       perspective:cmd:switch/final)
        ("."  "Switch to index..." perspective:cmd:switch/index)
        ("`"  "Switch to Previous"    perspective:cmd:switch/last)]])

    (meow-leader-define-key
     '("TAB" . mantle:meow/transient:perspective)))


  ;;------------------------------
  ;; Actually Create Keybinds:
  ;;------------------------------

  (if (imp:provided? :keybinds 'general 'meow)
      (mantle:meow/keybind/general:perspective)
    (mantle:meow/keybind/transient:perspective)))


;;------------------------------------------------------------------------------
;; Keybinds : Evil
;;------------------------------------------------------------------------------

(imp:use-package persp-mode
  :when  (imp:flag? :keybinds +evil)
  :unless noninteractive
  :after (:and evil evil-collection)

  ;;------------------------------
  :general ; evil
  ;;------------------------------

  ;;---
  ;; "Buffer" Keybinds
  ;;---
  (keybind:leader/global:def
   :infix   "b"
   "b" (list #'persp-switch-to-buffer :which-key "Switch Perspective Buffer"))

  ;;---
  ;; "Perspective" Keybinds
  ;;---
  (keybind:leader/global:def
    :infix   "TAB"
    "" (list nil :which-key "Perspectives...")
    "TAB" (list #'perspective:cmd:display        :which-key "List")
    "n"   (list #'perspective:cmd:new/named      :which-key "New")
    "N"   (list #'perspective:cmd:new            :which-key "New Unnamed")
    "l"   (list #'perspective:cmd:load           :which-key "Load")
    "s"   (list #'perspective:cmd:save           :which-key "Save")
    ;; "x"   (list #'perspective:cmd:kill           :which-key "Kill All")
    "d"   (list #'perspective:cmd:delete         :which-key "Delete")
    "r"   (list #'perspective:cmd:rename         :which-key "Rename")
    "]"   (list #'perspective:cmd:cycle/right    :which-key "Cycle: Right")
    "["   (list #'perspective:cmd:cycle/left     :which-key "Cycle: Left")
    "."   (list #'perspective:cmd:switch/index   :which-key "Switch: _")
    "`"   (list #'perspective:cmd:switch/last    :which-key "Switch: Previous")
    "1"   (list #'perspective:cmd:switch/index:0 :which-key "Switch: 1st")
    "2"   (list #'perspective:cmd:switch/index:1 :which-key "Switch: 2nd")
    "3"   (list #'perspective:cmd:switch/index:2 :which-key "Switch: 3rd")
    "4"   (list #'perspective:cmd:switch/index:3 :which-key "Switch: 4th")
    "5"   (list #'perspective:cmd:switch/index:4 :which-key "Switch: 5th")
    "6"   (list #'perspective:cmd:switch/index:5 :which-key "Switch: 6th")
    "7"   (list #'perspective:cmd:switch/index:6 :which-key "Switch: 7th")
    "8"   (list #'perspective:cmd:switch/index:7 :which-key "Switch: 8th")
    "9"   (list #'perspective:cmd:switch/index:8 :which-key "Switch: 9th")
    "0"   (list #'perspective:cmd:switch/final   :which-key "Switch: Final"))


  ;; ;;------------------------------
  ;; :config
  ;; ;;------------------------------
  ;;
  ; TODO: Doom remaps these functions to delete the perspective. Do we want this?
  ;; ;; Delete the current perspective if closing the last open window
  ;; (define-key! persp-mode-map
  ;;   [remap delete-window] #'+workspace/close-window-or-workspace
  ;;   [remap evil-window-delete] #'+workspace/close-window-or-workspace)
  )


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'config 'perspective)

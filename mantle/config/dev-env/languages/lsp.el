;;; mantle/config/dev-env/languages/lsp.el --- Serve Languages over a Protocol. -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2021-09-27
;; Timestamp:  2023-09-12
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Use Language Servers for auto-complete, docs, errors, warnings, etc.
;;
;; NOTE: This just sets up the LSP stuff itself. Each language needs to have a
;; hook or something for enabling `lsp-mode' in their buffers, and may need to
;; install the actual Language Server (e.g. OmniSharp for C#).
;;
;; https://microsoft.github.io/language-server-protocol/
;;
;; https://melpa.org/#/?q=lsp
;;
;;; Code:


(imp:require :jerky)
(imp:require :path)
(imp:require :elisp 'utils 'units)


;;------------------------------------------------------------------------------
;; Language Server Protocol
;;------------------------------------------------------------------------------

;; https://github.com/emacs-lsp/lsp-mode
;; https://emacs-lsp.github.io/lsp-mode/page/installation/
(imp:use-package lsp-mode
  :when (imp:flag? :dev-env +lsp)
  :after no-littering

  ;;------------------------------
  :commands
  ;;------------------------------
  (lsp-install-server
   lsp-update-server
   lsp-update-servers
   lsp
   lsp-deferred)

  ;;------------------------------
  :init
  ;;------------------------------

  ;;---
  ;; "Smarter" Shutdown
  ;;---
  (defcustom mantle:lsp:defer-shutdown/sec 3
    "If non-nil, defer shutdown of LSP servers for this many seconds after last
workspace buffer is closed.

This delay prevents premature server shutdown when a user still intends on
working on that project after closing the last buffer, or when programmatically
killing and opening many LSP/eglot-powered buffers.

Borrowed from Doom."
    :group 'innit:group
    :type '(natnum))


  (defvar mantle:lsp:defer-shutdown/timer nil
    "Timer used to defer LSP shutdown for `mantle:lsp:defer-shutdown/sec' seconds.")


  ;;---
  ;; Optimize Inter-Process Communication?
  ;;---

  (defvar mantle:lsp/ipc:cache/read-process-output-max/cache nil
    "Alternative value for `read-process-output-max' for LSP use.")


  (defvar mantle:lsp/ipc:cache/gcmh-high-cons-threshold nil
    "Alternative value for `gcmh-high-cons-threshold' for LSP use.")


  (defvar mantle:lsp/ipc:initialized? nil
    "Have the LSP/Emacs IPC optimizations been applied?")


  (define-minor-mode mantle:lsp/ipc:optimization-mode
    "Deploys _universal_ GC and IPC optimizations for `lsp-mode' and `eglot'.

Borrowed from Doom's `+lsp-optimization-mode' in \"modules/tools/lsp/config.el\"."
    :global t
    :init-value nil
    ;; TODO: Why are these all `setq-default'?
    ;;   Not Buffer-Local:
    ;;     - `read-process-output-max'
    ;;     - `gcmh-high-cons-threshold'
    ;;     - `mantle:lsp/ipc:initialized?'
    ;;   Buffer-Local:
    ;;     - `mantle:lsp/ipc:optimization-mode'
    ;;
    ;; Not that it matters, since for non-buffer-local variables `setq-default'
    ;; is just `setq' and just sets the current value, but that's also very
    ;; different from its intended purpose for buffer-local vars, where it
    ;; _doesn't_ set the current value, just the value that _new_ buffers will
    ;; use.
    ;;
    ;; TODO: change to just using `setq'?
    (if (not mantle:lsp/ipc:optimization-mode)
        (progn
          ;;------------------------------
          ;; Disable: Revert Settings
          ;;------------------------------
          (setq-default read-process-output-max     mantle:lsp/ipc:cache/read-process-output-max)
          (setq-default gcmh-high-cons-threshold    mantle:lsp/ipc:cache/gcmh-high-cons-threshold)
          (setq-default mantle:lsp/ipc:initialized? nil))

      ;;------------------------------
      ;; Enable
      ;;------------------------------
      ;; Only apply these settings once!
      (unless mantle:lsp/ipc:initialized?
        ;;---
        ;; Save the existing defaults...
        ;;---
        (setq mantle:lsp/ipc:cache/read-process-output-max  (default-value 'read-process-output-max))
        (setq mantle:lsp/ipc:cache/gcmh-high-cons-threshold (default-value 'gcmh-high-cons-threshold))

        ;;---
        ;; ...and give more wiggle room.
        ;;---
        (setq-default read-process-output-max (max read-process-output-max
                                                   (unit:byte 2 'mb)))
        ;; NOTE: LSP causes a lot of allocations, with or without the native
        ;; JSON library, so we up the GC threshold to stave off GC-induced
        ;; slowdowns/freezes. We use `gcmh' to enforce our GC strategy, so we
        ;; modify its variables rather than `gc-cons-threshold' directly.
        (setq-default gcmh-high-cons-threshold (max (default-value 'gcmh-high-cons-threshold)
                                                    (* 2 mantle:lsp/ipc:cache/gcmh-high-cons-threshold)))
        (gcmh-set-high-threshold)
        (setq mantle:lsp/ipc:initialized? t))))


  ;;---
  ;; Modeline
  ;;---

  (defvar-local lsp-modeline-icon nil
    "LSP enabled/disabled icon for modeline.")


  (innit:hook:defun
      (:name    'lsp:modeline/update
       :docstr  "Update modeline with LSP state."
       :squelch t)
    (let* ((workspaces (lsp-workspaces))
           (face       (if workspaces 'success 'warning))
           (label      (if workspaces "LSP Connected" "LSP Disconnected")))
      (setq lsp-modeline-icon (concat
                               " "
                               (propertize
                                ;; Just use an icon if possible, else
                                (let ((icon (mantle:user:icon/font-awesome "rocket"
                                                                           ""
                                                                           :face     face
                                                                           :v-adjust -0.0575)))
                                  (if (str:empty? icon :full)
                                      ;; TODO: Get icon working?
                                      (propertize "LSP-TODO-THIS" 'face face)
                                    icon))
                                  'help-echo label)
                               " "))
      (add-to-list 'global-mode-string
                   '(t (:eval lsp-modeline-icon))
                   'append)))


  ;;---
  ;; Headerline
  ;;---

  (innit:hook:defun
     (:name   'lsp:header/breadcrumb
      :docstr "Set up `lsp-mode' to show some information in the header line.")
    (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
    (lsp-headerline-breadcrumb-mode))


  ;;---
  ;; Hook for Enabling LSP in other `use-package' `:hook' sections.
  ;;---

  (innit:hook:defun
      (:name   'lsp:enable
       :docstr (mapconcat #'identity
                          '("Basic hook to enable `lsp-mode' for a buffer."
                            ""
                            "NOTE: Use this hook in languages' `use-package' blocks!"
                            "Example:"
                            "  (imp:use-package csharp-mode"
                            "    :hook (csharp-mode-hook . mantle:hook:lsp:enable))")
                          "\n"))
    ;; Wait to start language server until a buffer is visible.
    (lsp-deferred)
    ;; Integrate with `which-key' for all major modes active in the buffer
    ;; (e.g. `web-mode' can have a few modes active).
    (lsp-enable-which-key-integration :all))


  ;;------------------------------
  :hook
  ;;------------------------------

  ((lsp-mode-hook . mantle:lsp/ipc:optimization-mode)
   ((lsp-before-initialize-hook
     lsp-after-initialize-hook
     lsp-after-uninitialized-functions
     lsp-before-open-hook
     lsp-after-open-hook)
    . mantle:hook:lsp:modeline/update)
   (lsp-mode-hook . mantle:hook:lsp:header/breadcrumb)
   ;; Add LSP servers to our upgrade/update command.
   (innit:upgrade:hook . lsp-update-servers))


  ;;------------------------------
  :custom
  ;;------------------------------

  ;; Don't auto-kill LSP server after last workspace buffer is killed, because I
  ;; will do it for you, after `mantle:lsp:defer-shutdown/sec' seconds.
  (lsp-keep-workspace-alive nil)


  ;; NOTE I tweak LSP's defaults in order to make its more expensive or imposing
  ;;      features opt-in. Some servers implement these poorly and, in most
  ;;      cases, it's safer to rely on Emacs' native mechanisms (eldoc vs
  ;;      lsp-ui-doc, open in popup vs sideline, etc).

  ;; Disable features that have great potential to be slow.
  (lsp-enable-folding nil)
  (lsp-enable-text-document-color nil)
  ;; Reduce unexpected modifications to code
  (lsp-enable-on-type-formatting nil)
  ;; Make breadcrumbs opt-in; they're redundant with the modeline and imenu
  (lsp-headerline-breadcrumb-enable nil)

  ;; "With semantic token support you get more contextual information via different faces."
  ;;   https://emacs-lsp.github.io/lsp-mode/page/lsp-terraform-ls/#semantic-token-support
  ;; "Make sure to enable these two variables to ensure that you have semantic token support for terraform mode."
  ;;   https://emacs-lsp.github.io/lsp-mode/page/lsp-terraform-ls/#semantic-token-support
  (lsp-semantic-tokens-enable t)
  (lsp-semantic-tokens-honor-refresh-requests t)

  ;;------------------------------
  ;; Keybinds
  ;;------------------------------

  ;; TODO-meow: Bind the lsp keymap ourselves?
  (lsp-keymap-prefix nil)

  ;; Link to Documentation?
  ;; default keybinds: M-RET & middle mouse click.
  ;; (lsp-enable-links t) ; default: t


  ;;------------------------------
  :config
  ;;------------------------------

  ;; TODO: Do I need this?
  ;; (add-hook! 'doom-escape-hook
  ;;   (defun +lsp-signature-stop-maybe-h ()
  ;;     "Close the displayed `lsp-signature'."
  ;;     (when lsp-signature-mode
  ;;       (lsp-signature-stop)
  ;;       t)))
  ;;
  ;; (set-popup-rule! "^\\*lsp-\\(help\\|install\\)" :size 0.35 :quit t :select t)
  ;; (set-lookup-handlers! 'lsp-mode
  ;;   :definition #'+lsp-lookup-definition-handler
  ;;   :references #'+lsp-lookup-references-handler
  ;;   :documentation '(lsp-describe-thing-at-point :async t)
  ;;   :implementations '(lsp-find-implementation :async t)
  ;;   :type-definition #'lsp-find-type-definition)


  (define-advice lsp-diagnostics-flycheck-enable (:around (fn &rest args) mantle:advice:lsp:respect-user-defined-checker)
    "Ensure user-defined `flycheck-checker' isn't overwritten by `lsp'.

From Doom's `+lsp--respect-user-defined-checkers-a' in \"modules/tools/lsp/lsp.el\"."
    (if flycheck-checker
        (let ((old-checker flycheck-checker))
          (apply fn args)
          (setq-local flycheck-checker old-checker))
      (apply fn args)))


  (define-advice lsp--shutdown-workspace (:around (fn &optional restart) mantle:advice:lsp:defer-shutdown)
    "Defer server shutdown for a few seconds.

This gives the user a chance to open other project files before the server is
auto-killed (which is a potentially expensive process). It also prevents the
server getting expensively restarted when reverting buffers.

From Doom's `+lsp-defer-server-shutdown-a' in \"modules/tools/lsp/lsp.el\"."
    (if (or lsp-keep-workspace-alive
            restart
            (null mantle:lsp:defer-shutdown/sec)
            (= mantle:lsp:defer-shutdown/sec 0))
        ;; Do not defer shutdown for some reason, so cancel our optimizations minor mode.
        (prog1 (funcall fn restart)
          (mantle:lsp/ipc:optimization-mode -1))
      ;;------------------------------
      ;; Defer shutdown!
      ;;------------------------------
      ;; Cancel timer if running; will make another one.
      (when (timerp mantle:lsp:defer-shutdown/timer)
        (cancel-timer mantle:lsp:defer-shutdown/timer))
      ;; Make a timer for the actual shutdown.
      (setq mantle:lsp:defer-shutdown/timer
            (run-at-time
             ;; N seconds from now...
             (if (numberp mantle:lsp:defer-shutdown/sec)
                 mantle:lsp:defer-shutdown/sec
               3)
             ;; Never repeat.
             nil
             ;; Shutdown workspace lambda! Also cancel out of optimization mode!
             (lambda (workspace)
               (with-lsp-workspace workspace
                                   (unless (lsp--workspace-buffers workspace)
                                     (let ((lsp-restart 'ignore))
                                       (funcall fn))
                                     (mantle:lsp/ipc:optimization-mode -1))))
             lsp--cur-workspace))))

  ;; TODO-lsp: Do I want the lsp icon or no?
  ;; (when (modulep! :ui modeline +light)
  ;;   (defvar-local lsp-modeline-icon nil)
  )


;;------------------------------------------------------------------------------
;; `lsp-ui'
;;------------------------------------------------------------------------------
;; https://github.com/emacs-lsp/lsp-ui

(imp:use-package lsp-ui
  :after lsp

  ;; NOTE: `lsp-mode' automatically configures `lsp-ui' unless
  ;; `lsp-auto-configure' is set to nil.

  ;;------------------------------
  :custom
  ;;------------------------------

  ;; TODO-lsp: Are these from Doom ok tweaks to the default?
  ;; ;; (lsp-ui-peek-enable nil)
  ;; (lsp-ui-doc-max-height            8)
  ;; (lsp-ui-doc-max-width             72)        ; 150 (default) is too wide
  ;; (lsp-ui-doc-delay                 0.75)          ; 0.2 (default) is too naggy
  ;; (lsp-ui-doc-show-with-mouse       nil) ; don't disappear on mouseover
  ;; (lsp-ui-doc-position              'at-point)
  ;; (lsp-ui-sideline-ignore-duplicate t)
  ;; Don't show symbol definitions in the sideline. They are pretty noisy,
  ;; and there is a bug preventing Flycheck errors from being shown (the
  ;; errors flash briefly and then disappear).
  (lsp-ui-sideline-show-hover nil)
  ;; Re-enable icon scaling (it's disabled by default upstream for Emacs
  ;; 26.x compatibility; see emacs-lsp/lsp-ui#573)
  (lsp-ui-sideline-actions-icon lsp-ui-sideline-actions-icon-default))


;;------------------------------
;; Keybinds - Meow
;;------------------------------

;; TODO-meow: `lsp-ui' keybinds?
;; (imp:use-package lsp-ui
;;   :when  (imp:flag? :keybinds +meow)
;;   :after (:and lsp meow)
;;
;;   ;;------------------------------
;;   :custom
;;   ;;------------------------------
;;
;;   (map! :map lsp-ui-peek-mode-map
;;         "j"   #'lsp-ui-peek--select-next
;;         "k"   #'lsp-ui-peek--select-prev
;;         "C-k" #'lsp-ui-peek--select-prev-file
;;         "C-j" #'lsp-ui-peek--select-next-file))


;;------------------------------------------------------------------------------
;; LSP integration with `consult'
;;------------------------------------------------------------------------------

(imp:use-package consult-lsp
  :defer t
  :after (:and lsp consult)
  ;;------------------------------
  :general ; meow, evil, or vanilla
  ;;------------------------------
  (:map lsp-mode-map
   [remap xref-find-apropos] #'consult-lsp-symbols))


;;------------------------------------------------------------------------------
;; Flycheck
;;------------------------------------------------------------------------------

;; https://github.com/flycheck/flycheck
(imp:use-package flycheck
  :commands flycheck-list-errors flycheck-buffer

  ;;------------------------------
  :custom
  ;;------------------------------

  ;; Emacs Lisp syntax checker load path(s).
  ;; `inherit'       - Use Emacs' current `load-path'
  ;; list of strings - Add each dir to 'load-path' before invoking byte compiler.
  ;; nil             - Do not explicitly set `load-path'.
  (flycheck-emacs-lisp-load-path 'inherit)

  ;; Don't recheck on idle as often; wait this many seconds (default: 0.5).
  (flycheck-idle-change-delay 1.0)

  ;; Should we finish the syntax checking for buffers that get switched in and
  ;; out of focus faster than `flycheck-idle-buffer-switch-delay' seconds?
  (flycheck-buffer-switch-check-intermediate-buffers t)

  ;; Display errors a little quicker (default is 0.9s)
  (flycheck-display-errors-delay 0.25)


  ;;------------------------------
  :config
  ;;------------------------------

  ;; Rerunning checks on every newline is a mote excessive.
  (innit:customize-set-variable flycheck-check-syntax-automatically
                                (delq 'new-line flycheck-check-syntax-automatically))

  ;; TODO-lsp: How in the flying fuck to do this in vanilla without all of Doom's popup shit?
  ;;   - popup funcs: https://github.com/doomemacs/doomemacs/blob/master/modules/ui/popup/autoload/settings.el
  ;;
  ;; ;; Don't commandeer input focus if the error message pops up (happens when
  ;; ;; tooltips and childframes are disabled).
  ;; (set-popup-rules!
  ;;   '(("^\\*Flycheck error messages\\*" :select nil)
  ;;     ("^\\*Flycheck errors\\*" :size 0.25)))
  )


;;------------------------------
;; Keybinds : Meow
;;------------------------------

(imp:use-package flycheck
  :commands flycheck-list-errors flycheck-buffer

  ;;------------------------------
  :init ; meow-specific hook
  ;;------------------------------

  ;; This should be hooked into `meow-normal-mode-hook' or its `evil' equivalent.
  (innit:hook:defun
      (:name    'lsp/flycheck:syntax/trigger
       :docstr  "Trigger a syntax check on switch to normal mode."
       :squelch nil)
    (when (and flycheck-mode
               (not meow-normal-mode))
      (ignore-errors (flycheck-buffer))
      nil))

  ;;------------------------------
  :hook
  ;;------------------------------
  (meow-normal-mode-hook . mantle:hook:lsp/flycheck:syntax/trigger)


  ;;------------------------------
  :config
  ;;------------------------------

  ;; TODO: do we want/need to change the `flycheck-error-list-mode-map' keybinds?
  ;; Original:
  ;;   (defvar flycheck-error-list-mode-map
  ;;     (let ((map (make-sparse-keymap)))
  ;;       (define-key map (kbd "f") #'flycheck-error-list-set-filter)
  ;;       (define-key map (kbd "F") #'flycheck-error-list-reset-filter)
  ;;       (define-key map (kbd "n") #'flycheck-error-list-next-error)
  ;;       (define-key map (kbd "p") #'flycheck-error-list-previous-error)
  ;;       (define-key map (kbd "g") #'flycheck-error-list-check-source)
  ;;       (define-key map (kbd "e") #'flycheck-error-list-explain-error)
  ;;       (define-key map (kbd "RET") #'flycheck-error-list-goto-error)
  ;;       map)
  ;;     "The keymap of `flycheck-error-list-mode'.")
  ;;
  ;; Doom's tweaks for `evil':
  ;;   (map! :map flycheck-error-list-mode-map
  ;;         :n "C-n"    #'flycheck-error-list-next-error
  ;;         :n "C-p"    #'flycheck-error-list-previous-error
  ;;         :n "j"      #'flycheck-error-list-next-error
  ;;         :n "k"      #'flycheck-error-list-previous-error
  ;;         :n "RET"    #'flycheck-error-list-goto-error
  ;;         :n [return] #'flycheck-error-list-goto-error))
  )


;;------------------------------------------------------------------------------
;; `flycheck-popup-tip'
;;------------------------------------------------------------------------------

;; https://github.com/flycheck/flycheck-popup-tip
(imp:use-package flycheck-popup-tip
  :after flycheck)


;;------------------------------
;; `meow' Tweaks
;;------------------------------

(imp:use-package flycheck-popup-tip
  :when  (imp:flag? :keybinds +meow)
  :after (:and flycheck meow)

  ;;------------------------------
  :hook
  ;;------------------------------

  ;; Don't display popups while in an insert mode.
  ;; Popups can affect the cursor's position or cause disruptive input delays.
  ;; Or, at least, in evil insert or replace mode they can? So treat the same in meow insert mode.
  ((meow-insert-enter-hook . flycheck-popup-tip-delete-popup)
   (flycheck-mode-hook . flycheck-popup-tip-mode))


  ;;------------------------------
  :config
  ;;------------------------------

  (define-advice flycheck-popup-tip-show-popup (:before-while (fn &rest args) mantle:lsp/flycheck-popup-tip:no-popup-in-modal-insert)
    "Don't display popups while in an insert mode.

Popups can affect the cursor's position or cause disruptive input delays.
Or, at least, in evil insert or replace mode the can?
So treat the same in meow insert mode."
    (and (bound-and-true-p meow-mode)
         (not (meow-insert-mode-p)))))


;;------------------------------------------------------------------------------
;; `flycheck-posframe'
;;------------------------------------------------------------------------------

(imp:use-package flycheck-posframe
  :hook (flycheck-mode . +syntax-init-popups-h)

  ;; ;;------------------------------
  ;; :custom
  ;; ;;------------------------------
  ;;
  ;; (flycheck-posframe-info-prefix    "··· ")
  ;; (flycheck-posframe-warning-prefix "! ")
  ;; (flycheck-posframe-error-prefix   "X ")

  ;;------------------------------
  :config
  ;;------------------------------

  ;; Configure to show warnings and errors with nicer faces (derived from
  ;; `warning' and `error'). Set prefixes to pretty unicode characters.
  (flycheck-posframe-configure-pretty-defaults))


;;------------------------------
;; `meow' Tweaks
;;------------------------------

(imp:use-package flycheck-posframe
  :after meow
  :hook
  ;; Don't display popups while in insert or replace mode, as it can affect
  ;; the cursor's position or cause disruptive input delays.
  (flycheck-posframe-inhibit-functions . meow-insert-mode-p))


;;------------------------------
;; `company' Tweaks
;;------------------------------
;; Don't currently use `company', but just in case?
(imp:use-package flycheck-posframe
  :after company
  :hook
  ;; Don't display popups if company is open
  (flycheck-posframe-inhibit-functions . company--active-p))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'config 'dev-env 'languages 'lsp)

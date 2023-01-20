;;; mantle/config/completion.el --- Ivy, Helm, SMOCE, or... -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2022-07-20
;; Modified:   2022-07-20
;; URL:        https://github.com/cole-brown/.config-emacs
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;  Ivy, Helm, SMOCE, or...
;;    VMOCE?
;;
;;
;; [2022-07-20] "SMOCE" Stack:
;;   There's the standards like `ivy' and `helm', but I want to try newer next gen stuff.
;;   Like this:
;;     https://www.reddit.com/r/emacs/comments/ppg98f/comment/hd3hll1/?utm_source=share&utm_medium=web2x&context=3
;;   From:
;;     https://www.reddit.com/r/emacs/comments/ppg98f/which_completion_framework_do_you_use_and_why/
;;
;;   So... Introducing the "SMOCE Stack":
;;     - https://github.com/radian-software/selectrum#complementary-extensions
;;     - Selectrum:       https://github.com/raxod502/selectrum
;;       - (or Vertico?): https://github.com/minad/vertico
;;     - Marginalia:      https://github.com/minad/marginalia
;;     - Orderless:       https://github.com/oantolin/orderless
;;     - Consult:         https://github.com/minad/consult
;;     - Embark:          https://github.com/oantolin/embark
;;
;;
;; [2023-01-17] Deprecate Selectrum:
;;   Selectrum's maintainer(s) decided Vertico is better (same features, simpler
;;   code); instruct everyone to switch to Vertico.
;;     - https://github.com/radian-software/selectrum#selectrum-is-replaced
;;
;;   So... I guess "SMOCE" is now... "VMOCE"?
;;   "VMOCE Stack" doesn't have quite the ring to it compared to "SMOCE Stack"...
;;
;;   Migration Guide:
;;     https://github.com/minad/vertico/wiki/Migrating-from-Selectrum-to-Vertico
;;
;;; Code:


;;------------------------------------------------------------------------------
;; "VMOCE Stack": Vertico & Friends
;;------------------------------------------------------------------------------
;; There's the standards like `ivy' and `helm', but I want to try newer next gen stuff.
;; Like this:
;;   https://www.reddit.com/r/emacs/comments/ppg98f/comment/hd3hll1/?utm_source=share&utm_medium=web2x&context=3
;; From:
;;   https://www.reddit.com/r/emacs/comments/ppg98f/which_completion_framework_do_you_use_and_why/
;;
;; So... Introducing the "VMOCE Stack"):
;;   - Vertico:    https://github.com/minad/vertico
;;   - Marginalia: https://github.com/minad/marginalia
;;   - Orderless:  https://github.com/oantolin/orderless
;;   - Consult:    https://github.com/minad/consult
;;   - Embark:     https://github.com/oantolin/embark


;;------------------------------------------------------------------------------
;; Vertico: VERTical Interactive COmpletion
;;------------------------------------------------------------------------------
;; https://github.com/minad/vertico

;;------------------------------
;; Vertico
;;------------------------------

;; TODO: Finish README? https://github.com/minad/vertico#completion-styles-and-tab-completion
(imp:use-package vertico
  ;;------------------------------
  :custom
  ;;------------------------------
  ;; https://github.com/minad/vertico/blob/main/vertico.el
  ;;   Search for: (defcustom

  ;; Show more candidates (default 10).
  (vertico-count 20)

  ;; Different scroll margin (default 2).
  ;; (vertico-scroll-margin 0)

  ;; Grow and shrink the Vertico minibuffer.
  ;;   - options: t, nil, `grow-only'
  ;;   - default: `resize-mini-windows' (?!)
  ;; See `resize-mini-windows' for details.
  ;; (vertico-resize t)

  ;; Enable cycling for `vertico-next' and `vertico-previous' (default nil).
  ;; (vertico-cycle t)


  ;;------------------------------
  :config
  ;;------------------------------
  (vertico-mode +1))

;; TODO-keybinds: Are there (evil) Vertico keybinds?
;; TODO-keybinds: Do any of my keybinds need editted to be ok with Vertico?
;;   - https://github.com/minad/vertico#key-bindings
;;   - See `C-h v vertico-map' for keybinds, or:
;;     - https://github.com/minad/vertico/blob/main/vertico.el


;;------------------------------
;; Vertico w/ Prescient?
;;------------------------------
;; TODO: Do I want this:
;;   > "In some cases you may want to consider to use Prescient on top of
;;   > Orderless. Prescient can be used to provide frecency-based sorting (a
;;   > combination of frequency and recency) and history persistence by adding
;;   > the following."
;; TODO: If so: https://github.com/minad/vertico/wiki#using-prescientel-filtering-and-sorting


;;------------------------------
;; Save History
;;------------------------------
;; Persist history over Emacs restarts. Vertico sorts by history position.
(imp:use-package savehist
  :ensure nil ; This is an Emacs built-in feature.

  ;;------------------------------
  :init
  ;;------------------------------
  (savehist-mode +1))


;;------------------------------
;; Minibuffer Tweaks
;;------------------------------
;; TODO: Move minibuffer config elsewhere. Some of this was originally in "core/boot/10-init/10-settings.el"... Probably should be "mantle/config/minibuffer.el"?
(imp:use-package emacs
  :after vertico
  :ensure nil ; This is an Emacs built-in feature.

  ;;------------------------------
  :init
  ;;------------------------------

  ;; Typing yes/no is obnoxious when y/n will do
  (advice-add #'yes-or-no-p :override #'y-or-n-p)

  ;; Try to keep the cursor out of the read-only portions of the minibuffer.
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  (define-advice completing-read-multiple (:filter-args (fn &rest args) mantle:minibuffer:completing-read-multiple:indicator)
    "Add a prompt indicator to `completing-read-multiple'.

We display a prefix with the `crm-separator' in the prompt.
Assuming:
  1. The separator is a comma.
  2. The prompt is \"prompt❯ \"
Example:
  ├CRM:,┤ prompt❯ _"
    ;; Add prefix to PROMPT in ARGS.
    (cons (format "├CRM:%s┤ %s" ; Unicode?
                  ;; "[CRM:%s] %s" ; ASCII only
                  (replace-regexp-in-string "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                                            crm-separator)
                  (car args))
          (cdr args)))


  ;;------------------------------
  :custom
  ;;------------------------------

  ;; Allow for minibuffer-ception. Sometimes we need another minibuffer command
  ;; while we're in the minibuffer.
  (enable-recursive-minibuffers t)

  ;; Show current key-sequence in minibuffer ala 'set showcmd' in vim. Any
  ;; feedback after typing is better UX than no feedback at all.
  (echo-keystrokes 0.02)

  ;; Expand the minibuffer to fit multi-line text displayed in the echo-area. This
  ;; doesn't look too great with direnv, however...
  (resize-mini-windows 'grow-only)

  ;; Try to keep the cursor out of the read-only portions of the minibuffer.
  (minibuffer-prompt-properties '(read-only         t
                                  intangible        t
                                  cursor-intangible t
                                  face              minibuffer-prompt))

  ;; TODO: Do I want to try this?
  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (read-extended-command-predicate #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (enable-recursive-minibuffers t))


;; ;;------------------------------------------------------------------------------
;; ;; Selectrum: Better Completion UI
;; ;;------------------------------------------------------------------------------
;; ;; https://github.com/raxod502/selectrum

;; ;;------------------------------
;; ;; Selectrum
;; ;;------------------------------
;; ;; Not a whole lot of configuration...

;; (imp:use-package selectrum
;;   ;;------------------------------
;;   :config
;;   ;;------------------------------
;;   (selectrum-mode +1))

;; ;; TODO: Are there /evil/ Selectrum keybinds?
;; ;;   - https://github.com/radian-software/selectrum#keybindings


;; ;;------------------------------
;; ;; Selectrum + Orderless
;; ;;------------------------------
;; ;; https://github.com/radian-software/selectrum#alternative-2-orderless
;; ;; https://github.com/oantolin/orderless#selectrum

;; (imp:eval:after (:and selectrum orderless)

;;   ;; Persist history over Emacs restarts
;;   (savehist-mode +1)

;;   ;; Optional performance optimization
;;   ;; by highlighting only the visible candidates.
;;   (innit:customize-set-variable orderless-skip-highlighting             (lambda () selectrum-is-active))
;;   (innit:customize-set-variable selectrum-refine-candidates-function    #'orderless-filter)
;;   (innit:customize-set-variable selectrum-highlight-candidates-function #'orderless-highlight-matches))


;; ;; TODO: Do I want this:
;; ;;   > "In some cases you may want to consider to use Prescient on top of
;; ;;   > Orderless. Prescient can be used to provide frecency-based sorting (a
;; ;;   > combination of frequency and recency) and history persistence by adding
;; ;;   > the following."
;; ;; TODO: If so:
;; ;; TODO:   1) Uncomment this.
;; ;; TODO:   2) Make an `imp:use-package' section for `prescient'.
;; ;; ;;------------------------------
;; ;; ;; Selectrum + Orderless + Prescient
;; ;; ;;------------------------------
;; ;; (imp:eval:after (:and selectrum orderless prescient)
;; ;;
;; ;;   (innit:customize-set-variable selectrum-prescient-enable-filtering nil)
;; ;;   (selectrum-prescient-mode +1)
;; ;;   (prescient-persist-mode +1))


;;------------------------------------------------------------------------------
;; Marginalia: Notes & Info in the Minibuffer Margin
;;------------------------------------------------------------------------------
;; https://github.com/minad/marginalia

(imp:use-package marginalia
  ;; The :init configuration is always executed (Not lazy!)
  ;;------------------------------
  :init
  ;;------------------------------

  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode +1)

  ;;------------------------------
  :bind
  ;;------------------------------
  ;; TODO: What keybind to give this? What does it do?
  ;; Either bind `marginalia-cycle` globally or only in the minibuffer
  (("M-A" . marginalia-cycle)
   :map minibuffer-local-map
   ("M-A" . marginalia-cycle)))


;;------------------------------------------------------------------------------
;; Orderless: Orderless, Space-Separated Completion Style
;;------------------------------------------------------------------------------
;; https://github.com/oantolin/orderless

(imp:use-package orderless

  ;;------------------------------
  :custom
  ;;------------------------------
  ;; > The `basic' completion style is specified as fallback in addition to
  ;; > `orderless' in order to ensure that completion commands which rely on
  ;; > dynamic completion tables, e.g., `completion-table-dynamic' or
  ;; > `completion-table-in-turn', work correctly. Furthermore the `basic'
  ;; > completion style needs to be tried /first/ (not as a fallback) for
  ;; > TRAMP hostname completion to work. In order to achieve that, we add an
  ;; > entry for the `file' completion category in the
  ;; > `completion-category-overrides' variable. In addition, the
  ;; > `partial-completion' style allows you to use wildcards for file
  ;; > completion and partial paths, e.g., '/u/s/l' for '/usr/share/local'.
  ;;   - https://github.com/oantolin/orderless#overview
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))


;;------------------------------------------------------------------------------
;; Consult: Consulting `completing-read'
;;------------------------------------------------------------------------------
;; https://github.com/minad/consult
;;
;; Available Commands: https://github.com/minad/consult#available-commands

(imp:use-package consult
  ;;------------------------------
  :init
  ;;------------------------------
  ;; Non-lazy things that need to happen?
  ;;   - See: https://github.com/minad/consult#use-package-example
  ;;     - Moved the setting of custom vars to `:custom'.

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)


  ;;------------------------------
  :bind
  ;;------------------------------
  ;; TODO: Replace this `:bind' section with a `:general' section.
  ;; Replace bindings. Lazily loaded due by `use-package'.
  (;; C-c bindings (mode-specific-map)
   ("C-c h" . consult-history)
   ("C-c m" . consult-mode-command)
   ("C-c k" . consult-kmacro)
   ;; C-x bindings (ctl-x-map)
   ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
   ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
   ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
   ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
   ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
   ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
   ;; Custom M-# bindings for fast register access
   ("M-#" . consult-register-load)
   ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
   ("C-M-#" . consult-register)
   ;; Other custom bindings
   ("M-y" . consult-yank-pop)                ;; orig. yank-pop
   ("<help> a" . consult-apropos)            ;; orig. apropos-command
   ;; M-g bindings (goto-map)
   ("M-g e" . consult-compile-error)
   ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
   ("M-g g" . consult-goto-line)             ;; orig. goto-line
   ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
   ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
   ("M-g m" . consult-mark)
   ("M-g k" . consult-global-mark)
   ("M-g i" . consult-imenu)
   ("M-g I" . consult-imenu-multi)
   ;; M-s bindings (search-map)
   ("M-s d" . consult-find)
   ("M-s D" . consult-locate)
   ("M-s g" . consult-grep)
   ("M-s G" . consult-git-grep)
   ("M-s r" . consult-ripgrep)
   ("M-s l" . consult-line)
   ("M-s L" . consult-line-multi)
   ("M-s m" . consult-multi-occur)
   ("M-s k" . consult-keep-lines)
   ("M-s u" . consult-focus-lines)
   ;; Isearch integration
   ("M-s e" . consult-isearch-history)
   :map isearch-mode-map
   ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
   ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
   ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
   ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
   ;; Minibuffer history
   :map minibuffer-local-map
   ("M-s" . consult-history)                 ;; orig. next-matching-history-element
   ("M-r" . consult-history))                ;; orig. previous-matching-history-element


  ;;------------------------------
  :hook
  ;;------------------------------
  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  (completion-list-mode-hook . consult-preview-at-point-mode)


  ;;------------------------------
  :custom
  ;;------------------------------

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (register-preview-delay    0.5)
  (register-preview-function #'consult-register-format)

  ;; Use Consult to select xref locations with preview.
  (xref-show-xrefs-function       #'consult-xref)
  (xref-show-definitions-function #'consult-xref)


  ;;------------------------------
  :config
  ;;------------------------------

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))

  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)

   consult-ripgrep
   consult-git-grep
   consult-grep
   consult-bookmark
   consult-recent-file
   consult-xref
   consult--source-bookmark
   consult--source-recent-file
   consult--source-project-recent-file
   :preview-key (kbd "M-."))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  ;; TODO: EVIL???
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;; There are multiple reasonable alternatives to chose from.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 3. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 4. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")
  )

;; TODO: Consult packages for integrating with other packages?
;; https://github.com/minad/consult#recommended-packages


;;------------------------------------------------------------------------------
;; Embark: Emacs Mini-Buffer Action Rooted in Keymaps
;;------------------------------------------------------------------------------
;; https://github.com/oantolin/embark
;;
;; > You can think of `embark-act' as a keyboard-based version of a right-click
;; > contextual menu. The `embark-act' command (which you should bind to a
;; > convenient key), acts as a prefix for a keymap offering you relevant actions
;; > to use on a target determined by the context:
;; >  - In the minibuffer, the target is the current top completion candidate.
;; >  - In the *Completions* buffer the target is the completion at point.
;; >  - In a regular buffer, the target is the region if active, or else the file,
;; >    symbol, URL, s-expression or defun at point.

(imp:use-package embark

  ;;------------------------------
  :init
  ;;------------------------------

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)


  ;;------------------------------
  :bind
  ;;------------------------------
  ;; TODO: EVIL bindings?
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'


  ;;------------------------------
  :config
  ;;------------------------------

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;;------------------------------
;; Embark + Consult
;;------------------------------
;; https://github.com/oantolin/embark#quick-start
;; https://github.com/minad/consult#embark-integration

(imp:use-package embark-consult
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer

  ;;------------------------------
  :hook
  ;;------------------------------
  (embark-collect-mode-hook . consult-preview-at-point-mode))


;;------------------------------
;; Embark + Which-Key
;;------------------------------
;; https://github.com/oantolin/embark/wiki/Additional-Configuration#use-which-key-like-a-key-menu-prompt

(imp:eval:after (:and embark which-key)

  (defun embark-which-key-indicator ()
    "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
    (lambda (&optional keymap targets prefix)
      (if (null keymap)
          (which-key--hide-popup-ignore-command)
        (which-key--show-keymap
         (if (eq (plist-get (car targets) :type) 'embark-become)
             "Become"
           (format "Act on %s '%s'%s"
                   (plist-get (car targets) :type)
                   (embark--truncate-target (plist-get (car targets) :target))
                   (if (cdr targets) "…" "")))
         (if prefix
             (pcase (lookup-key keymap prefix 'accept-default)
               ((and (pred keymapp) km) km)
               (_ (key-binding prefix 'accept-default)))
           keymap)
         nil nil t (lambda (binding)
                     (not (string-suffix-p "-argument" (cdr binding))))))))

  (setq embark-indicators
        '(embark-which-key-indicator
          embark-highlight-indicator
          embark-isearch-highlight-indicator))

  (defun embark-hide-which-key-indicator (fn &rest args)
    "Hide the which-key indicator immediately when using the completing-read prompter."
    (which-key--hide-popup-ignore-command)
    (let ((embark-indicators
           (remq #'embark-which-key-indicator embark-indicators)))
      (apply fn args)))

  (advice-add #'embark-completing-read-prompter
              :around #'embark-hide-which-key-indicator))


;;------------------------------------------------------------------------------
;; Mini-Frame
;;------------------------------------------------------------------------------
;; https://github.com/muffinmad/emacs-mini-frame
;;
;; TODO: Do I want the minibuffer at the top of the current frame during
;; `read-from-minibuffer'?
;;
;; TODO: In order to find out, try this:
;; (imp:use-package mini-frame
;;   ;; Not all that much config help on the GitHub...
;;   ;; May have to go Googling to see how people use this.
;;
;;   ;;------------------------------
;;   :config
;;   ;;------------------------------
;;   (mini-frame-mode +1))
;;
;; TODO: If I like this, move it elsewhere. It was suggested by a "SMOCE"
;; completion framework package, but it's a minibuffer package, not a completion
;; package.


;;------------------------------------------------------------------------------
;; Corfu: Completion Overlay Region FUnction
;;------------------------------------------------------------------------------

;; https://github.com/minad/corfu
(imp:use-package corfu
  ;; ;;------------------------------
  ;; :custom
  ;; ;;------------------------------
  ;;
  ;; ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  ;; ;; (corfu-auto t)                 ;; Enable auto completion (default nil)
  ;; ;; (corfu-separator ?\s)          ;; Orderless field separator (default: `?\s' aka space)
  ;; ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;;------------------------------
  :init
  ;;------------------------------
  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since Dabbrev can be used globally (M-/).
  ;; See also `corfu-excluded-modes'.
  (global-corfu-mode +1)

  ;; ;;------------------------------
  ;; :hook
  ;; ;;------------------------------
  ;; ;; Alternative: Enable Corfu only for certain modes.
  ;; ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))
  )


;; ;; Completion settings in Emacs that could be useful for `corfu'.
;; (imp:use-package emacs
;;    :ensure nil ; This is an Emacs built-in feature.

;;   ;;------------------------------
;;   :custom
;;   ;;------------------------------
;;   ;; TAB cycle if there are only few candidates
;;   (completion-cycle-threshold 3)

;;   ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
;;   ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
;;   ;; (read-extended-command-predicate #'command-completion-default-include-p)

;;   ;; Enable indentation+completion using the TAB key?
;;   ;; `completion-at-point' is often bound to M-TAB.
;;   ;;
;;   ;; Options:
;;   ;;   - t          - TAB always just indents the current line.
;;   ;;   - nil        - TAB indents the current line if point is at the left margin or in the line's indentation.
;;   ;;                - Otherwise it inserts a TAB character or spaces, depending.
;;   ;;   - `complete' - TAB first tries to indent the current line, and if the line
;;   ;;                  was already indented, then try to complete the thing at point.
;;   ;;
;;   ;; Also see `tab-first-completion'.
;;   ;;
;;   ;; Some programming language modes have their own variable to control this,
;;   ;; e.g., `c-tab-always-indent', and do not respect this variable.
;;   ;;
;;   ;; NOTE: We'll just bind completion to something other than TAB...
;;   ;; (tab-always-indent 'complete)
;;   )


;; ;;------------------------------------------------------------------------------
;; ;; Dabbrev: Dynamic Abbrev Expansion: Dynamic Abbreviation Expansion
;; ;;------------------------------------------------------------------------------

;; ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Dynamic-Abbrevs.html
;; (imp:use-package dabbrev
;;   :ensure nil ; This is an Emacs built-in feature.

;;   ;;------------------------------
;;   :bind
;;   ;;------------------------------
;;   ;; Swap 'M-/' and 'C-M-/'
;;   ;; TODO: Yes or no on this? Doom doesn't swap 'em.
;;   (("M-/" . dabbrev-completion)
;;    ("C-M-/" . dabbrev-expand))

;;   ;; TODO: `:general' keybinds?

;;   ;;------------------------------
;;   :custom
;;   ;;------------------------------
;;   ;; Other useful Dabbrev configurations.
;;   (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'config 'user 'completion)

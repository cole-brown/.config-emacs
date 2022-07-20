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
;;
;;; Code:

;;; completion.el --- Change basic keybinds. -*- lexical-binding: t; -*-
;;; general.el --- General Keybinds -*- lexical-binding: t; -*-
;;
;;
;;; Commentary:
;;
;; Change basic keybinds.
;;
;;; Code:


;;------------------------------------------------------------------------------
;; "SMOCE Stack": Selectrum & Friends
;;------------------------------------------------------------------------------
;; There's the standards like `ivy' and `helm', but I want to try newer next gen stuff.
;; Like this:
;;   https://www.reddit.com/r/emacs/comments/ppg98f/comment/hd3hll1/?utm_source=share&utm_medium=web2x&context=3
;; From:
;;   https://www.reddit.com/r/emacs/comments/ppg98f/which_completion_framework_do_you_use_and_why/
;;
;; So... Introducing the "SMOCE Stack":
;;   - https://github.com/radian-software/selectrum#complementary-extensions
;;   - Selectrum:       https://github.com/raxod502/selectrum
;;     - (or Vertico?): https://github.com/minad/vertico
;;   - Marginalia:      https://github.com/minad/marginalia
;;   - Orderless:       https://github.com/oantolin/orderless
;;   - Consult:         https://github.com/minad/consult
;;   - Embark:          https://github.com/oantolin/embark


;;------------------------------------------------------------------------------
;; Selectrum: Better Completion UI
;;------------------------------------------------------------------------------
;; https://github.com/raxod502/selectrum

;;------------------------------
;; Selectrum
;;------------------------------
;; Not a whole lot of configuration...

(imp:use-package selectrum
  ;;--------------------
  :config
  ;;--------------------
  (selectrum-mode +1))

;; TODO: Are there /evil/ Selectrum keybinds?
;;   - https://github.com/radian-software/selectrum#keybindings


;;------------------------------
;; Selectrum + Orderless
;;------------------------------
;; https://github.com/radian-software/selectrum#alternative-2-orderless
;; https://github.com/oantolin/orderless#selectrum

(imp:eval:after (:and selectrum orderless)

  ;; Persist history over Emacs restarts
  (savehist-mode)

  ;; Optional performance optimization
  ;; by highlighting only the visible candidates.
  (customize-set-variable 'orderless-skip-highlighting (lambda () selectrum-is-active))
  (customize-set-variable 'selectrum-refine-candidates-function    #'orderless-filter)
  (customize-set-variable 'selectrum-highlight-candidates-function #'orderless-highlight-matches))


;; TODO: Do I want this:
;;   > "In some cases you may want to consider to use Prescient on top of
;;   > Orderless. Prescient can be used to provide frecency-based sorting (a
;;   > combination of frequency and recency) and history persistence by adding
;;   > the following."
;; TODO: If so:
;; TODO:   1) Uncomment this.
;; TODO:   2) Make an `imp:use-package' section for `prescient'.
;; ;;------------------------------
;; ;; Selectrum + Orderless + Prescient
;; ;;------------------------------
;; (imp:eval:after (:and selectrum orderless prescient)
;;
;;   (customize-set-variable 'selectrum-prescient-enable-filtering nil)
;;   (selectrum-prescient-mode +1)
;;   (prescient-persist-mode +1))


;;------------------------------------------------------------------------------
;; Marginalia: Notes & Info in the Minibuffer Margin
;;------------------------------------------------------------------------------
;; https://github.com/minad/marginalia

(imp:use-package marginalia
  ;; The :init configuration is always executed (Not lazy!)
  ;;--------------------
  :init
  ;;--------------------

  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode)

  ;;--------------------
  :bind
  ;;--------------------
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

  ;;--------------------
  :custom
  ;;--------------------
  ;; Selectrum suggests only `orderless' when using Selectrum & Orderless together.
  ;; Orderless suggests `orderless' and `basic'.
  ;; Orderless explains, so go with Orderless' settings?
  ;;
  ;;   > The `basic' completion style is specified as fallback in addition to
  ;;   > `orderless' in order to ensure that completion commands which rely on
  ;;   > dynamic completion tables, e.g., `completion-table-dynamic' or
  ;;   > `completion-table-in-turn', work correctly. Furthermore the `basic'
  ;;   > completion style needs to be tried /first/ (not as a fallback) for
  ;;   > TRAMP hostname completion to work. In order to achieve that, we add an
  ;;   > entry for the `file' completion category in the
  ;;   > `completion-category-overrides' variable. In addition, the
  ;;   > `partial-completion' style allows you to use wildcards for file
  ;;   > completion and partial paths, e.g., '/u/s/l' for '/usr/share/local'.
  ;;     - https://github.com/oantolin/orderless#overview
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))


;;------------------------------------------------------------------------------
;; Consult: Consulting `completing-read'
;;------------------------------------------------------------------------------
;; https://github.com/minad/consult
;;
;; Available Commands: https://github.com/minad/consult#available-commands

(imp:use-package consult
  ;;--------------------
  :init
  ;;--------------------
  ;; Non-lazy things that need to happen?
  ;;   - See: https://github.com/minad/consult#use-package-example
  ;;     - Moved the setting of custom vars to `:custom'.

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)


  ;;--------------------
  :bind
  ;;--------------------
  ;; TODO: Evil Consult Keybinds?
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


  ;;--------------------
  :hook
  ;;--------------------
  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  (completion-list-mode . consult-preview-at-point-mode)


  ;;--------------------
  :custom
  ;;--------------------

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (register-preview-delay    0.5)
  (register-preview-function #'consult-register-format)

  ;; Use Consult to select xref locations with preview.
  (xref-show-xrefs-function       #'consult-xref)
  (xref-show-definitions-function #'consult-xref)


  ;;--------------------
  :config
  ;;--------------------

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

  ;;--------------------
  :init
  ;;--------------------

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)


  ;;--------------------
  :bind
  ;;--------------------
  ;; TODO: EVIL bindings?
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'


  ;;--------------------
  :config
  ;;--------------------

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

  ;;--------------------
  :hook
  ;;--------------------
  (embark-collect-mode . consult-preview-at-point-mode))


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
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'config 'user 'completion)

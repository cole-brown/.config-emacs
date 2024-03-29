;;; mantle/config/dev-env/emacs-lisp.el --- Configure Emacs Lisp -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2022-07-28
;; Timestamp:  2023-06-29
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Configure Emacs Lisp
;; aka Elisp
;;
;;; Code:


(imp:require :path)


;;------------------------------------------------------------------------------
;; Keybind Prefixes : Evil
;;------------------------------------------------------------------------------

(imp:use-package emacs
  :when  (imp:flag? :keybinds +evil)
  :after  (:and evil evil-collection
           (:keybinds user general evil))

  ;;------------------------------
  :init
  ;;------------------------------

  (keybind:leader/local:def
    :keymaps '(emacs-lisp-mode-map lisp-interaction-mode-map)
    :infix   "d"                     ; debug
    "" '(nil :which-key "debug...")) ; infix's title


  (keybind:leader/local:def
    :keymaps '(emacs-lisp-mode-map lisp-interaction-mode-map)
    :infix   "e"                    ; eval
    "" '(nil :which-key "eval...")) ; infix's title


  (keybind:leader/local:def
    :keymaps '(emacs-lisp-mode-map lisp-interaction-mode-map)
    :infix   "g"                    ; goto
    "" '(nil :which-key "goto...")) ; infix's title


  (keybind:leader/local:def
    :keymaps '(emacs-lisp-mode-map lisp-interaction-mode-map)
    :infix   "t"                     ; test
    "" '(nil :which-key "test..."))) ; infix's title


;;------------------------------------------------------------------------------
;; Emacs Lisp Mode
;;------------------------------------------------------------------------------

(imp:use-package elisp-mode
  :ensure nil ; This is an Emacs built-in feature.

  ;; TODO: Add cask?
  ;; https://github.com/cask/cask
  ;; :mode ("\\.Cask\\'" . emacs-lisp-mode)

  ;;------------------------------
  :init
  ;;------------------------------

  (defun mantle:user:emacs-lisp:edebug:instrument-defun/on ()
    "Enable instrumentalization for the function under `defun'."
    (interactive)
    (eval-defun 'edebugit))

  (defun mantle:user:emacs-lisp:edebug:instrument-defun/off ()
    "Disable instrumentalization for the function under `defun'."
    (interactive)
    (eval-defun nil))


  (defun mantle:user:emacs-lisp:indent-function (indent-point state)
    "Indent plists more sensibly than `lisp-indent-function'.

Adapted from Doom's `+emacs-lisp-indent-function'in
\"modules/lang/emacs-lisp/autoload.el\", which was adapted from
https://emacs.stackexchange.com/questions/10230/how-to-indent-keywords-aligned"
    (let ((normal-indent (current-column))
          (orig-point (point))
          target)
      (goto-char (1+ (elt state 1)))
      (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
      (cond ((and (elt state 2)
                  (or (not (looking-at-p "\\sw\\|\\s_"))
                      (eq (char-after) ?:)))
             (unless (> (save-excursion (forward-line 1) (point))
                        calculate-lisp-indent-last-sexp)
               (goto-char calculate-lisp-indent-last-sexp)
               (beginning-of-line)
               (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t))
             (backward-prefix-chars)
             (current-column))
            ((and (save-excursion
                    (goto-char indent-point)
                    (skip-syntax-forward " ")
                    (not (eq (char-after) ?:)))
                  (save-excursion
                    (goto-char orig-point)
                    (and (eq (char-after) ?:)
                         (eq (char-before) ?\()
                         (setq target (current-column)))))
             (save-excursion
               (move-to-column target t)
               target))
            ((let* ((function (buffer-substring (point) (progn (forward-sexp 1) (point))))
                    (method (or (function-get (intern-soft function) 'lisp-indent-function)
                                (get (intern-soft function) 'lisp-indent-hook))))
               (cond ((or (eq method 'defun)
                          (and (null method)
                               (> (length function) 3)
                               (string-match-p "\\`def" function)))
                      (lisp-indent-defform state indent-point))
                     ((integerp method)
                      (lisp-indent-specform method state indent-point normal-indent))
                     (method
                      (funcall method indent-point state))))))))

  (innit:hook:defun
      (:name   'emacs-lisp:settings
       :docstr "Settings for Emacs Lisp mode. Non-LSP stuff.")
    ;; Emacs' built-in elisp files use a hybrid tab->space indentation scheme
    ;; with a tab width of 8. Any smaller and the indentation will be
    ;; unreadable. Since Emacs' lisp indenter doesn't respect this variable it's
    ;; safe to ignore this setting otherwise.
    (setq tab-width 8)

    ;; Shorter name in modeline
    (setq mode-name "Elisp")

    ;; ;; Don't treat autoloads or sexp openers as outline headers, we have
    ;; ;; hideshow for that.
    ;; (setq outline-regexp +emacs-lisp-outline-regexp)

    ;; Fixed indenter that intends plists sensibly.
    (setq lisp-indent-function #'mantle:user:emacs-lisp:indent-function))


  ;;------------------------------
  :hook
  ;;------------------------------
  ((emacs-lisp-mode-hook . mantle:hook:emacs-lisp:settings)
   ;; TODO: Is Flycheck usable without this?
   ;; ;; Flycheck's two emacs-lisp checkers produce a *lot* of false positives in
   ;; ;; emacs configs, so we disable `emacs-lisp-checkdoc' and reduce the
   ;; ;; `emacs-lisp' checker's verbosity.
   ;; (flycheck-mode-hook #'+emacs-lisp-reduce-flycheck-errors-in-emacs-config-h)
   (emacs-lisp-mode-hook . rainbow-delimiters-mode)
   (emacs-lisp-mode-hook . mantle:hook:time-stamp:settings)
   (before-save-hook     . mantle:hook:time-stamp:save))


  ;;------------------------------
  :config
  ;;------------------------------

  ;; ;; variable-width indentation is superior in elisp. Otherwise, `dtrt-indent'
  ;; ;; and `editorconfig' would force fixed indentation on elisp.
  ;; (add-to-list 'doom-detect-indentation-excluded-modes 'emacs-lisp-mode)

  ;; ;; Enhance elisp syntax highlighting, by highlighting Doom-specific
  ;; ;; constructs, defined symbols, and truncating :pin's in `package!' calls.
  ;; (font-lock-add-keywords
  ;;  'emacs-lisp-mode
  ;;  (append `(;; custom Doom cookies
  ;;            ("^;;;###\\(autodef\\|if\\|package\\)[ \n]" (1 font-lock-warning-face t)))
  ;;          ;; Shorten the :pin of `package!' statements to 10 characters
  ;;          `(("(package!\\_>" (0 (+emacs-lisp-truncate-pin))))
  ;;          ;; highlight defined, special variables & functions
  ;;          (when +emacs-lisp-enable-extra-fontification
  ;;            `((+emacs-lisp-highlight-vars-and-faces . +emacs-lisp--face)))))

  (define-advice elisp-get-var-docstring (:around (fn sym) mantle:advice:append-value)
    "Display variable value next to documentation in eldoc."
    (when-let (ret (funcall fn sym))
      (if (boundp sym)
          (concat ret " "
                  (let* ((truncated " [...]")
                         (print-escape-newlines t)
                         (str (symbol-value sym))
                         (str (prin1-to-string str))
                         (limit (- (frame-width) (length ret) (length truncated) 1)))
                    (format (format "%%0.%ds%%s" (max limit 0))
                            (propertize str 'face 'warning)
                            (if (< (length str) limit) "" truncated))))
        ret))))


;;------------------------------------------------------------------------------
;; `elisp-mode' Keybinds : Meow
;;------------------------------------------------------------------------------

(imp:use-package elisp-mode
  :ensure nil ; This is an Emacs built-in feature.
  :when  (imp:flag? :keybinds +meow)
  :after meow

  ;;------------------------------
  :config
  ;;------------------------------

  ;;------------------------------
  ;; `General'
  ;;------------------------------

  (defun mantle:meow/keybind/general:elisp-mode ()
    "Create the `elisp-mode' keybinds in `general' for `meow'."
    (keybind:meow:leader/local:bind-keys
        '(emacs-lisp-mode-map lisp-interaction-mode-map)
      ;;---
      ;; Debug...
      ;;---
      "d f" (list #'mantle:user:emacs-lisp:edebug:instrument-defun/on :which-key "`edebug' instrument enable")
      "d F" (list #'mantle:user:emacs-lisp:edebug:instrument-defun/off :which-key "`edebug' instrument disable")

      ;;---
      ;; Eval...
      ;;---
      ;; TODO:meow: Try these without "Display Names" and see if it actually is better?
      ;; Doom doesn't bother with a pretty name.
      "e b" #'eval-buffer       ; (list #'eval-buffer       :which-key "Eval Buffer")
      "e d" #'eval-defun        ; (list #'eval-defun        :which-key "Eval Defun")
      "e e" #'eval-last-sexp    ; (list #'eval-last-sexp    :which-key "Eval Last Sexp")
      "e E" #'pp-eval-last-sexp ; (list #'pp-eval-last-sexp :which-key "Eval Last Sexp: Pretty Print")
      "e r" #'eval-region       ; (list #'eval-region       :which-key "Eval Region")
      "e l" #'load-library      ; (list #'load-library      :which-key "Load Library")

      ;;---
      ;; "Go To Considered Harmful"
      ;;---
      ;; TODO:meow: Try these without "Display Names" and see if it actually is better?
      ;; Doom doesn't bother with a pretty name.
      "g f" #'find-function
      "g F" #'find-function-at-point
      "g v" #'find-variable
      "g l" #'find-library))


  ;;------------------------------
  ;; `Transient'
  ;;------------------------------

  (defun mantle:meow/keybind/transient:elisp-mode ()
    "Create the `elisp-mode' keybinds in `transient' for `meow'."
    ;;---
    ;; Debug...
    ;;---
    (transient-define-prefix mantle:meow/transient:elisp:debug ()
      "Debug commands in Meow \"Local\" Leader for Emacs Lisp Mode."
      ["Debug..."
       ["`edebug'"
        ("f" "`edebug' instrument enable"  mantle:user:emacs-lisp:edebug:instrument-defun/on)
        ("F" "`edebug' instrument disable" mantle:user:emacs-lisp:edebug:instrument-defun/off)]])

    (mantle:meow:leader/local:key emacs-lisp-mode-map
                                  "d" #'mantle:meow/transient:elisp:debug)

    ;;---
    ;; Eval...
    ;;---
    (transient-define-prefix mantle:meow/transient:elisp:eval ()
      "Eval commands in Meow \"Local\" Leader for Emacs Lisp Mode."
      [["Eval..."
        ("b" "eval-buffer"       eval-buffer)
        ("d" "eval-defun"        eval-defun)
        ("e" "eval-last-sexp"    eval-last-sexp)
        ("E" "pp-eval-last-sexp" pp-eval-last-sexp)
        ("r" "eval-region"       eval-region)]
       ["Load..."
        ("l" "load-library"      load-library)]])

    (mantle:meow:leader/local:key emacs-lisp-mode-map
                                  "e" #'mantle:meow/transient:elisp:eval)

    ;;---
    ;; Find...
    ;;---
    (transient-define-prefix mantle:meow/transient:elisp:find ()
      "Find commands in Meow \"Local\" Leader for Emacs Lisp Mode."
      ["Find..."
       ("f" "find-function"          find-function)
       ("F" "find-function-at-point" find-function-at-point)
       ("v" "find-variable"          find-variable)
       ("l" "find-library"           find-library)])

    (mantle:meow:leader/local:key emacs-lisp-mode-map
                                  "g" #'mantle:meow/transient:elisp:find))


  ;;------------------------------
  ;; Actually Create Keybinds:
  ;;------------------------------

  (if (imp:provided? :keybinds 'general 'meow)
      (mantle:meow/keybind/general:elisp-mode)
    (mantle:meow/keybind/transient:elisp-mode)))


;;------------------------------------------------------------------------------
;; `elisp-mode' Keybinds : Evil
;;------------------------------------------------------------------------------

(imp:use-package elisp-mode
  :ensure nil ; This is an Emacs built-in feature.
  :when  (imp:flag? :keybinds +evil)
  :after (:and evil evil-collection)

  ;;------------------------------
  :general ; evil
  ;;------------------------------
  ;;---
  ;; Debug...
  ;;---
  (keybind:leader/local:def
    :infix "d"
    "f" (list #'mantle:user:emacs-lisp:edebug:instrument-defun/on :which-key "`edebug' instrument enable")
    "F" (list #'mantle:user:emacs-lisp:edebug:instrument-defun/off :which-key "`edebug' instrument disable"))

  ;;---
  ;; Eval...
  ;;---
  (keybind:leader/local:def
    :infix "e"
    ;; TODO: Try these without "Display Names" and see if it actually is better?
    ;; Doom doesn't bother with a pretty name.
    "b" #'eval-buffer       ; (list #'eval-buffer       :which-key "Eval Buffer")
    "d" #'eval-defun        ; (list #'eval-defun        :which-key "Eval Defun")
    "e" #'eval-last-sexp    ; (list #'eval-last-sexp    :which-key "Eval Last Sexp")
    "E" #'pp-eval-last-sexp ; (list #'pp-eval-last-sexp :which-key "Eval Last Sexp: Pretty Print")
    "r" #'eval-region       ; (list #'eval-region       :which-key "Eval Region")
    "l" #'load-library)     ; (list #'load-library      :which-key "Load Library")

  ;;---
  ;; "Go To Considered Harmful"
  ;;---
  (keybind:leader/local:def
    :infix "g"
    ;; TODO: Try these without "Display Names" and see if it actually is better?
    ;; Doom doesn't bother with a pretty name.
    "f" #'find-function
    "F" #'find-function-at-point
    "v" #'find-variable
    "l" #'find-library))



;;------------------------------------------------------------------------------
;; Highlight Quoted Symbols
;;------------------------------------------------------------------------------

;; Make quoted symbols easier to distinguish from free variables
;; https://github.com/Fanael/highlight-quoted
(imp:use-package highlight-quoted
  ;;------------------------------
  :hook
  ;;------------------------------
  (emacs-lisp-mode-hook . highlight-quoted-mode))


;; TODO-elisp: Add `elisp-def' package?
;;   https://github.com/Wilfred/elisp-def
;; TODO-elisp: in `elisp-def' use-package:
;;   ;; Recenter window after following definition
;;     (advice-add #'elisp-def :after #'doom-recenter-a)


;;------------------------------------------------------------------------------
;; REPL
;;------------------------------------------------------------------------------

(imp:use-package ielm
  :defer t

  ;;------------------------------
  :config
  ;;------------------------------

  ;; Adapted from http://www.modernemacs.com/post/comint-highlighting/ to add
  ;; syntax highlighting to ielm REPLs.
  (setq ielm-font-lock-keywords
        (append '(("\\(^\\*\\*\\*[^*]+\\*\\*\\*\\)\\(.*$\\)"
                   (1 font-lock-comment-face)
                   (2 font-lock-constant-face)))
                (when (require 'highlight-numbers nil t)
                  (highlight-numbers--get-regexp-for-mode 'emacs-lisp-mode))
                (cl-loop for (matcher . match-highlights)
                         in (append lisp-el-font-lock-keywords-2
                                    lisp-cl-font-lock-keywords-2)
                         collect
                         `((lambda (limit)
                             (when ,(if (symbolp matcher)
                                        `(,matcher limit)
                                      `(re-search-forward ,matcher limit t))
                               ;; Only highlight matches after the prompt
                               (> (match-beginning 0) (car comint-last-prompt))
                               ;; Make sure we're not in a comment or string
                               (let ((state (syntax-ppss)))
                                 (not (or (nth 3 state)
                                          (nth 4 state))))))
                           ,@match-highlights)))))


;;------------------------------------------------------------------------------
;; Tests: Overseer
;;------------------------------------------------------------------------------

;; https://github.com/tonini/overseer.el
(imp:use-package overseer
  ;;------------------------------
  :config
  ;;------------------------------

  (define-advice overseer--current-buffer-test-file-p (:override (&rest _) mantle:advice:content-not-filename)
    "Test files are test files because of their contents, not their filename?

`overseer' only checks if the filenam ends in \"-test.el\" exactly. That's too
restrictive. Instead, check if the word \"test\" is in the start of the file."
    ;; Will return nil or a buffer position, so already ok for a return value.
    (buffer:search:header "test" :case 'ignore)))


;;------------------------------------------------------------------------------
;; Tests: Buttercup
;;------------------------------------------------------------------------------

;; https://github.com/jorgenschaefer/emacs-buttercup
(imp:use-package buttercup
  :defer t

  ;;------------------------------
  :minor ; from `auto-minor-mode'
  ;;------------------------------
  ;; TODO:use-package: Am I allowed to use `rx-to-string' directly?
  ;; (rx-to-string
  ;;  '(sequence
  ;;    "/"
  ;;    "test"
  ;;    (or "/" "-")
  ;;    (one-or-more any)
  ;;    ".el"
  ;;    string-end)
  ;;  :no-group)
  ("/test[/-].+\\.el\\'" . buttercup-minor-mode)


  ;;------------------------------
  :preface
  ;;------------------------------
  ;; `buttercup' doesn't define a keymap for `buttercup-minor-mode', so we have
  ;; to fool its internal `define-minor-mode' call into thinking one exists, so
  ;; that it will associate it with the mode.
  (defvar buttercup-minor-mode-map (make-sparse-keymap))


  ;;------------------------------
  :init
  ;;------------------------------

  (defun mantle:user:emacs-lisp:buttercup:run-file ()
    "Run all buttercup tests in the focused buffer.

Originally from Doom's `+emacs-lisp/buttercup-run-file' in
\"modules/lang/emacs-lisp/autoload.el\"."
    (interactive)
    (let ((load-path
           (append (list (path:current:dir)
                         (or (path:project:root)
                             default-directory))
                   load-path))
          (buttercup-suites nil))
      (save-selected-window
        (eval-buffer)
        (buttercup-run))
      (message "File executed successfully")))

  (defun mantle:user:emacs-lisp:buttercup:run-project ()
    "Run all buttercup tests in the project.

Originally from Doom's `+emacs-lisp/buttercup-run-project' in
\"modules/lang/emacs-lisp/autoload.el\"."
    (interactive)
    (let* ((default-directory (path:project:root))
           (load-path (append (list (path:join default-directory "test")
                                    default-directory)
                              load-path))
           (buttercup-suites nil))
      (buttercup-run-discover)))


  ;;------------------------------
  :config
  ;;------------------------------

  ;; (set-popup-rule! "^\\*Buttercup\\*$" :size 0.45 :select nil :ttl 0)

  ;; Add any `buttercup' minor-mode snippets to `yasnippet'.
  (mantle:user:yasnippet:minor-mode/add buttercup-minor-mode)

  (imp:eval:after evil
    (add-hook 'buttercup-minor-mode-hook #'evil-normalize-keymaps)))


;;------------------------------------------------------------------------------
;; Tests: Buttercup: Keybinds : Meow
;;------------------------------------------------------------------------------

(imp:use-package buttercup
  :when  (imp:flag? :keybinds +meow)
  :after meow

  ;;------------------------------
  :config
  ;;------------------------------
  (keybind:meow:leader/local:bind-keys
      'buttercup-minor-mode-map
    "t t" (list #'mantle:user:emacs-lisp:buttercup:run-file    :which-key "buttercup: run file")
    "t a" (list #'mantle:user:emacs-lisp:buttercup:run-project :which-key "buttercup: run project")
    "t s" (list #'buttercup-run-at-point                       :which-key "buttercup: run at point")))


;;------------------------------------------------------------------------------
;; Tests: Buttercup: Keybinds : Evil
;;------------------------------------------------------------------------------

(imp:use-package buttercup
  :when  (imp:flag? :keybinds +evil)
  :after (:and evil evil-collection)

  ;;------------------------------
  :general ; evil
  ;;------------------------------
  (:prefix  (keybind:prefix :local "t") ; test
   :states  keybind:leader/local:states
   :keymaps 'buttercup-minor-mode-map
   "t" (list #'mantle:user:emacs-lisp:buttercup:run-file    :which-key "buttercup: run file")
   "a" (list #'mantle:user:emacs-lisp:buttercup:run-project :which-key "buttercup: run project")
   "s" (list #'buttercup-run-at-point                       :which-key "buttercup: run at point")))


;;------------------------------------------------------------------------------
;; Macros
;;------------------------------------------------------------------------------

;; Provides a very helpful elisp macro debugging tool: `macrostep-expand'
(imp:use-package macrostep)

;;------------------------------------------------------------------------------
;; `macrostep' Keybinds : Meow
;;------------------------------------------------------------------------------

(imp:use-package macrostep
  :when  (imp:flag? :keybinds +meow)
  :after meow

  ;;------------------------------
  :config
  ;;------------------------------

  ;; TODO:meow:macrostep: Make `macrostep' actually work again!!!
  (keybind:meow:leader/local:bind-keys
      '(emacs-lisp-mode-map lisp-interaction-mode-map)
    "m" #'macrostep-expand)) ;; Expand macro and enter macrostep-expand mode.


;;------------------------------------------------------------------------------
;; `macrostep' Keybinds : Evil
;;------------------------------------------------------------------------------

(imp:use-package macrostep
  :when  (imp:flag? :keybinds +evil)
  :after (:and evil evil-collection)

  ;;------------------------------
  :general ; evil
  ;;------------------------------
  (:prefix  (keybind:prefix :local)
   :states  keybind:leader/local:states
   :keymaps 'emacs-lisp-mode-map
   "m" (list #'macrostep-expand :which-key "Expand Macro")))


;; TODO: General Flycheck set-up in "common.el"? Steal from Doom?
;; ;;------------------------------------------------------------------------------
;; ;; Flycheck (TODO: & Cask?)
;; ;;------------------------------------------------------------------------------
;;
;; https://github.com/flycheck/flycheck-cask
;; (imp:use-package flycheck-cask
;;   :defer t
;;
;;   ;;------------------------------
;;   :init
;;   ;;------------------------------
;;
;;   (innit:hook:defun
;;       (:name   'emacs-lisp:flycheck-cask:setup
;;        :docstr (mapconcat #'identity
;;                           '("Hook `flycheck-cask-setup' into `flycheck-mode-hook'."
;;                             ""
;;                             "Better deffered/lazy loading, probably?")
;;                           "\n"))
;;     (add-hook 'flycheck-mode-hook #'flycheck-cask-setup
;;               nil
;;               'local))
;;
;;
;;   ;;------------------------------
;;   :hook
;;   ;;------------------------------
;;   (emacs-lisp-mode-hook . mantle:hook:emacs-lisp:flycheck-cask:setup))
;;
;;
;; (imp:use-package flycheck-package
;;   :after flycheck
;;
;;   ;;------------------------------
;;   :config
;;   ;;------------------------------
;;   (flycheck-package-setup))


;;------------------------------------------------------------------------------
;; Help
;;------------------------------------------------------------------------------
;; https://github.com/xuchunyang/elisp-demos

(imp:use-package elisp-demos
  ;;------------------------------
  :init
  ;;------------------------------
  (advice-add #'describe-function-1 :after #'elisp-demos-advice-describe-function-1)
  (advice-add #'helpful-update      :after #'elisp-demos-advice-helpful-update)

  (defun mantle:user:elisp-demos:search/file (symbol filepath)
    "Search for SYMBOL's demo/example for `elisp-demos' in FILEPATH."
    (with-temp-buffer
      (insert-file-contents filepath)
      (goto-char (point-min))
      (when (re-search-forward
             (format "^\\* %s$" (regexp-quote (symbol-name symbol)))
             nil t)
        (let (beg end)
          (forward-line 1)
          (setq beg (point))
          (if (re-search-forward "^\\*" nil t)
              (setq end (line-beginning-position))
            (setq end (point-max)))
          (string-trim (buffer-substring-no-properties beg end))))))

  (defun mantle:user:elisp-demos:search/all (symbol)
    "Search all non-`elisp-demos' demo files for SYMBOL's demo/example."
    (let ((filename "elisp-demos.org")
          ;; TODO: Make demos for all of my shit, and add them in here?
          ;; See Doom's 'demos.org' file: "modules/lang/emacs-lisp/demos.org"
          ;; NOTE: Raise error if one of our packages/modules goes AWOL.
          (paths (list (imp:path:root/get :mis      :error)
                       (imp:path:root/get :datetime :error)))
          demo)
      ;;------------------------------
      ;; Search each path.
      ;;------------------------------
      (while (and (null demo)
                  (not (null paths)))
        ;; If the demo file exists, search it for SYMBOL.
        (let ((path (imp:path:join (pop paths) filename)))
          (when (path:exists? path :file)
            (setq demo (mantle:user:elisp-demos:search/file symbol path)))))

      ;;------------------------------
      ;; Return demo string found or nil for "not found".
      ;;------------------------------
      demo))


  ;;------------------------------
  :config
  ;;------------------------------

  (define-advice elisp-demos--search (:around (fn symbol) mantle:user:elisp-demos:add)
    "Add our own demos to help buffers.

NOTE: This function is called when the help buffer is being built - no caching
or anything is done."
    ;;------------------------------
    ;; Search for SYMBOL's example in:
    ;;------------------------------
    ;; 1. `elisp-demos' file.
    (or (funcall fn symbol)
        ;; 2. Our file(s).
        (mantle:user:elisp-demos:search/all symbol))))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'config 'dev-env 'languages 'emacs-lisp)

;;; mantle/config/dev-env/javascript.el --- Development Environment -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2023-01-05
;; Modified:   2023-01-05
;; URL:        https://github.com/cole-brown/.config-emacs
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;  Development Environment
;;
;;; Code:


(imp:require :path)
(imp:require :keybind)


;;------------------------------------------------------------------------------
;; Check for Required Executables
;;------------------------------------------------------------------------------
;; Check that `node' and other exes required for JS dev are available on this system.

(unless (executable-find "node")
  (nub:warning
      :innit
      (imp:path:join (imp:path:current:dir/relative :mantle)
                     (imp:path:current:file))
    '("Could not find `node' executable. Is it installed? "
      "JavaScript config wants it.")))


(unless (executable-find "npm")
  (nub:warning
      :innit
      (imp:path:join (imp:path:current:dir/relative :mantle)
                     (imp:path:current:file))
    '("Could not find `npm' executable. Is it installed? "
      "JavaScript config wants it.")))


(unless (executable-find "yarn")
  (nub:warning
      :innit
      (imp:path:join (imp:path:current:dir/relative :mantle)
                     (imp:path:current:file))
    '("Could not find `yarn' executable. Is it installed? "
      "JavaScript config wants it.")))


;;------------------------------------------------------------------------------
;; Projects
;;------------------------------------------------------------------------------

(imp:eval:after projectile
  ;; 'package.json' is in the root of a JavaScript project that uses NPM.
  (unless (seq-contains-p projectile-project-root-files "package.json" #'string=)
    (push "package.json" projectile-project-root-files))

  ;; Ignore these dirs; nothing useful and can be giant.
  (unless (seq-contains-p projectile-globally-ignored-directories "^node_modules$" #'string=)
    (push "^node_modules$" projectile-globally-ignored-directories))
  (unless (seq-contains-p projectile-globally-ignored-directories "^flow-typed$" #'string=)
    (push "^flow-typed$" projectile-globally-ignored-directories)))


;;------------------------------------------------------------------------------
;; JavaScript
;;------------------------------------------------------------------------------

(imp:use-package js
  :ensure nil ; This is an Emacs built-in feature.

  :mode "\\.[mc]?js\\'"
  :mode "\\.es6\\'"
  :mode "\\.pac\\'"

  :interpreter "node"

  ;;------------------------------
  :init
  ;;------------------------------
  ;; Parse node stack traces in the compilation buffer.
  (imp:eval:after compile ; NOTE: Doom had this as `compilation', but it's `compile'... right?
    (add-to-list 'compilation-error-regexp-alist 'node)
    (add-to-list 'compilation-error-regexp-alist-alist
                 '(node "^[[:blank:]]*at \\(.*(\\|\\)\\(.+?\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\)"
                        2 3 4)))


  (innit:hook:defun
     (:name   'javascript:settings
      :file   macro<imp>:path/file
      :docstr "Settings for Javascript mode. Non-LSP stuff.")

   ;; Nothing currently, I guess?

   ;; Separate camel-case into separate words?
   ;; (subword-mode t)
   )


  ;;------------------------------
  :hook
  ;;------------------------------
  ((js-mode-hook . mantle:hook:javascript:settings)
   ;; (js-mode-hook . rainbow-delimiters-mode)
   )

  ;;------------------------------
  :custom
  ;;------------------------------

  ;; Indent multi-line chained calls:
  ;; let example = foo.function()
  ;;                  .indentedFunction()
  ;;                  .getExampleValue();
  (js-chain-indent t)

  ;; NOTE: See the rest (`js-.*-indent-level' variables) for more tweakability.
  (js-indent-level (jerky:get 'code 'tab 'standard)))


;;------------------------------------------------------------------------------
;; Node / NPM
;;------------------------------------------------------------------------------

(imp:use-package npm-mode
  ;;------------------------------
  :hook
  ;;------------------------------
  ((js-mode typescript-mode) . npm-mode)

  ;;------------------------------
  :general
  ;;------------------------------
  (:prefix  (keybind:prefix :local)
   :states  keybind:leader/local:states
   :keymaps (list 'npm-mode-keymap keybind:leader/local:keymaps)
   ;; TODO: ...Does this work? Binding a whole keymap to a key?
   "n" npm-mode-command-keymap))


;; TODO: A Node.js REPL? https://github.com/abicky/nodejs-repl.el


;;------------------------------------------------------------------------------
;; TypeScript
;;------------------------------------------------------------------------------

(imp:use-package typescript-mode
  ;;------------------------------
  :init
  ;;------------------------------

  ;; NOTE: From Doom; add if need (better?) TSX file support.
  ;; ;; REVIEW We associate TSX files with `typescript-tsx-mode' derived from
  ;; ;;        `web-mode' because `typescript-mode' does not officially support
  ;; ;;        JSX/TSX. See emacs-typescript/typescript.el#4
  ;; (when (featurep! :lang web)
  ;;   (autoload 'typescript-tsx-mode "typescript-mode" nil t))
  ;;
  ;; ;; REVIEW We associate TSX files with `typescript-tsx-mode' derived from
  ;; ;;        `web-mode' because `typescript-mode' does not officially support
  ;; ;;        JSX/TSX. See emacs-typescript/typescript.el#4
  ;; (add-to-list 'auto-mode-alist
  ;;              (cons "\\.tsx\\'"
  ;;                    (if (featurep! :lang web)
  ;;                        #'typescript-tsx-mode
  ;;                      #'typescript-mode)))

  ;; TODO-lsp: This from Doom?
  ;; (imp:eval:after flycheck
  ;;   (flycheck-add-mode 'javascript-eslint 'web-mode)
  ;;   (flycheck-add-mode 'javascript-eslint 'typescript-mode)
  ;;   (flycheck-add-mode 'javascript-eslint 'typescript-tsx-mode)
  ;;   (flycheck-add-mode 'typescript-tslint 'typescript-tsx-mode)
  ;;
  ;;   (add-hook! 'typescript-tsx-mode-hook
  ;;     (defun +javascript-disable-tide-checkers-h ()
  ;;       (pushnew! flycheck-disabled-checkers
  ;;                 'javascript-jshint
  ;;                 'tsx-tide
  ;;                 'jsx-tide))))

  (innit:hook:defun
      (:name   'typescript:settings
       :file   macro<imp>:path/file
       :docstr "Settings for Typescript mode. Non-LSP stuff.")

     ;; 'wide' is a decent default, probably?
     (setq fill-column (jerky:get 'fill-column 'wide))

     (setq tab-width (jerky:get 'code 'tab 'standard))

    ;; Separate camel-case into separate words?
    ;; (subword-mode t)

     ;; NOTE: Delete this if the line brakes act fucky.
     (setq comment-line-break-function #'js2-line-break)

     ;; Most projects use either eslint, prettier, .editorconfig, or tsf in order
     ;; to specify indent level and formatting. In the event that no
     ;; project-level config is specified (very rarely these days), the community
     ;; default is 2, not 4. However, respect what is in tsfmt.json if it is
     ;; present in the project
     (setq typescript-indent-level (or (and (bound-and-true-p tide-mode)
                                      (plist-get (tide-tsfmt-options) :indentSize))
                                 typescript-indent-level))

     ;; Doom Fix #5556: expand .x to className="x" instead of class="x", if
    ;; `emmet-mode' is used.
    (setq emmet-expand-jsx-className? t))


  ;;------------------------------
  :hook
  ;;------------------------------
  ((typescript-mode-hook . mantle:hook:typescript:settings)
   ;; (typescript-mode-hook . rainbow-delimiters-mode)
   ;;  (typescript-tsx-mode-hook . rainbow-delimiters-mode))
   )


  ;;------------------------------
  :config
  ;;------------------------------

  ;; NOTE: From Doom; add if need TSX file support.
  ;; (when (fboundp 'web-mode)
  ;;   (define-derived-mode typescript-tsx-mode web-mode "TypeScript-TSX")
  ;;   (when (featurep! +lsp)
  ;;     (after! lsp-mode
  ;;       (add-to-list 'lsp--formatting-indent-alist '(typescript-tsx-mode . typescript-indent-level)))))

    ;; HACK Fixes comment continuation on newline
  (autoload 'js2-line-break "js2-mode" nil t))







(imp:use-package javascript-mode
  ;; TODO: Add cask?
  ;; https://github.com/cask/cask
  ;; :mode ("\\.Cask\\'" . javascript-mode)

  ;;------------------------------
  :init
  ;;------------------------------

  (defun mantle:user:javascript:edebug:instrument-defun/on ()
    "Enable instrumentalization for the function under `defun'."
    (interactive)
    (eval-defun 'edebugit))

  (defun mantle:user:javascript:edebug:instrument-defun/off ()
    "Disable instrumentalization for the function under `defun'."
    (interactive)
    (eval-defun nil))


  (defun mantle:user:javascript:indent-function (indent-point state)
    "Indent plists more sensibly than `lisp-indent-function'.

Adapted from Doom's `+javascript-indent-function'in
\"modules/lang/javascript/autoload.el\", which was adapted from
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
      (:name   'javascript:settings
       :file   macro<imp>:path/file
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
    ;; (setq outline-regexp +javascript-outline-regexp)

    ;; Fixed indenter that intends plists sensibly.
    (setq lisp-indent-function #'mantle:user:javascript:indent-function))


  ;;------------------------------
  :hook
  ;;------------------------------
  ((javascript-mode-hook . mantle:hook:javascript:settings)
   ;; TODO: Is Flycheck usable without this?
   ;; ;; Flycheck's two javascript checkers produce a *lot* of false positives in
   ;; ;; emacs configs, so we disable `javascript-checkdoc' and reduce the
   ;; ;; `javascript' checker's verbosity.
   ;; (flycheck-mode-hook #'+javascript-reduce-flycheck-errors-in-emacs-config-h)
   )


  ;;------------------------------
  :general
  ;;------------------------------
  ;;---
  ;; Debug...
  ;;---
  (:prefix  (keybind:prefix :local "d")
   :states  keybind:leader/local:states
   :keymaps '(javascript-mode-map lisp-interaction-mode-map keybind:leader/local:keymaps)
   "f" (list #'mantle:user:javascript:edebug:instrument-defun/on :which-key "`edebug' instrument enable")
   "F" (list #'mantle:user:javascript:edebug:instrument-defun/off :which-key "`edebug' instrument disable"))

  ;;---
  ;; Eval...
  ;;---
  (:prefix  (keybind:prefix :local "e")
   :states  keybind:leader/local:states
   :keymaps '(javascript-mode-map lisp-interaction-mode-map keybind:leader/local:keymaps)
   ;; TODO: Try these without "Display Names" and see if it actually is better?
   ;; Doom doesn't bother with a pretty name.
   "b" #'eval-buffer       ; (list #'eval-buffer       :which-key "Eval Buffer")
   "d" #'eval-defun        ; (list #'eval-defun        :which-key "Eval Defun")
   "e" #'eval-last-sexp    ; (list #'eval-last-sexp    :which-key "Eval Last Sexp")
   "E" #'pp-eval-last-sexp ; (list #'pp-eval-last-sexp :which-key "Eval Last Sexp: Pretty Print")
   "r" #'eval-region       ; (list #'eval-region       :which-key "Eval Region")
   "l" #'load-library      ; (list #'load-library      :which-key "Load Library")
   )

  ;;---
  ;; "Go To Considered Harmful"
  ;;---
  (:prefix  (keybind:prefix :local "g")
   :states  keybind:leader/local:states
   :keymaps '(javascript-mode-map lisp-interaction-mode-map keybind:leader/local:keymaps)
   ;; TODO: Try these without "Display Names" and see if it actually is better?
   ;; Doom doesn't bother with a pretty name.
   "f" #'find-function
   "F" #'find-function-at-point
   "v" #'find-variable
   "l" #'find-library)

  ;;------------------------------
  :config
  ;;------------------------------

  ;; ;; variable-width indentation is superior in elisp. Otherwise, `dtrt-indent'
  ;; ;; and `editorconfig' would force fixed indentation on elisp.
  ;; (add-to-list 'doom-detect-indentation-excluded-modes 'javascript-mode)

  ;; ;; Enhance elisp syntax highlighting, by highlighting Doom-specific
  ;; ;; constructs, defined symbols, and truncating :pin's in `package!' calls.
  ;; (font-lock-add-keywords
  ;;  'javascript-mode
  ;;  (append `(;; custom Doom cookies
  ;;            ("^;;;###\\(autodef\\|if\\|package\\)[ \n]" (1 font-lock-warning-face t)))
  ;;          ;; Shorten the :pin of `package!' statements to 10 characters
  ;;          `(("(package!\\_>" (0 (+javascript-truncate-pin))))
  ;;          ;; highlight defined, special variables & functions
  ;;          (when +javascript-enable-extra-fontification
  ;;            `((+javascript-highlight-vars-and-faces . +javascript--face)))))

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


;; Make quoted symbols easier to distinguish from free variables
;; https://github.com/Fanael/highlight-quoted
(imp:use-package highlight-quoted
  ;;------------------------------
  :hook
  ;;------------------------------
  (javascript-mode-hook . highlight-quoted-mode))


;; TODO: in `elisp-def' use-package:
;; ;; Recenter window after following definition
;;   (advice-add #'elisp-def :after #'doom-recenter-a)


;;------------------------------
;; REPL
;;------------------------------

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
                  (highlight-numbers--get-regexp-for-mode 'javascript-mode))
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
;; Tests
;;------------------------------------------------------------------------------

;; https://github.com/tonini/overseer.el
(imp:use-package overseer
  ;;------------------------------
  :config
  ;;------------------------------

  (define-advice overseer--current-buffer-test-file-p (:override (fn sym) mantle:advice:content-not-filename)
    "Test files are test files because of their contents, not their filename?

`overseer' only checks if the filenam ends in \"-test.el\" exactly. That's too
restrictive. Instead, check if the word \"test\" is in the start of the file."
    ;; Will return nil or a buffer position, so already ok for a return value.
    (buffer:search:header "test" :case 'ignore)))


;; https://github.com/jorgenschaefer/emacs-buttercup
(imp:use-package buttercup
  :defer t

  ;;------------------------------
  :minor ; from `auto-minor-mode'
  ;;------------------------------
  ;; TODO: Am I allowed to use `rx-to-string' directly?
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

  (defun mantle:user:javascript:buttercup:run-file ()
    "Run all buttercup tests in the focused buffer.

Originally from Doom's `+javascript/buttercup-run-file' in
\"modules/lang/javascript/autoload.el\"."
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

  (defun mantle:user:javascript:buttercup:run-project ()
    "Run all buttercup tests in the project.

Originally from Doom's `+javascript/buttercup-run-project' in
\"modules/lang/javascript/autoload.el\"."
    (interactive)
    (let* ((default-directory (path:project:root))
           (load-path (append (list (path:join default-directory "test")
                                    default-directory)
                              load-path))
           (buttercup-suites nil))
      (buttercup-run-discover)))


  ;;------------------------------
  :general
  ;;------------------------------
  (:prefix  (keybind:prefix :local "t") ; test
   :states  keybind:leader/local:states
   :keymaps '(buttercup-minor-mode-map keybind:leader/local:keymaps)
   "t" (list #'mantle:user:javascript:buttercup:run-file :which-key "buttercup: run file")
   "a" (list #'mantle:user:javascript:buttercup:run-project :which-key "buttercup: run project")
   "s" (list #'buttercup-run-at-point :which-key "buttercup: run at point"))


  ;;------------------------------
  :config
  ;;------------------------------

  ;; (set-popup-rule! "^\\*Buttercup\\*$" :size 0.45 :select nil :ttl 0)

  ;; Add any `buttercup' minor-mode snippets to `yasnippet'.
  (mantle:user:yasnippet:minor-mode/add buttercup-minor-mode)

  (imp:eval:after evil
    (add-hook 'buttercup-minor-mode-hook #'evil-normalize-keymaps)))


;;------------------------------------------------------------------------------
;; Macros
;;------------------------------------------------------------------------------

;; Provides a very helpful elisp macro debugging tool: `macrostep-expand'
(imp:use-package macrostep

  ;;--------------------
  :general
  ;;--------------------
  (:prefix  (keybind:prefix :local "")
   :states  keybind:leader/local:states
   :keymaps '(javascript-mode-map keybind:leader/local:keymaps)
   "m" '(macrostep-expand :which-key "Expand Macro")))


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
;;       (:name   'javascript:flycheck-cask:setup
;;        :file   macro<imp>:path/file
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
;;   (javascript-mode-hook . mantle:hook:javascript:flycheck-cask:setup))
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
  :defer t

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
          ;; See Doom's 'demos.org' file: "modules/lang/javascript/demos.org"
          (paths (list (imp:path:root :mis)
                       (imp:path:root :datetime)))
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
;; Other Packages?
;;------------------------------------------------------------------------------

;; Skewer: Live web development with Emacs
;;   - https://github.com/skeeto/skewer-mode
;; "Provides live interaction with JavaScript, CSS, and HTML in a web browser.
;; Expressions are sent on-the-fly from an editing buffer to be evaluated in
;; the browser, just like Emacs does with an inferior Lisp process in Lisp
;; modes."


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'config 'user 'dev-env 'languages 'javascript)

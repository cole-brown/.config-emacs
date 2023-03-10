;;; mantle/config/dev-env/languages/python.el --- And now for something completely different... -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2021-09-27
;; Modified:   2022-12-19
;; URL:        https://github.com/cole-brown/.config-emacs
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.

;;; Commentary:
;;
;;  And now for something completely different...
;;
;;; Code:


(imp:require :jerky)
(imp:require :buffer 'type)
(imp:require :path)


;;------------------------------------------------------------------------------
;; Python Project Files
;;------------------------------------------------------------------------------

(imp:eval:after projectile
  (add-to-list 'projectile-project-root-files "pyproject.toml")
  (add-to-list 'projectile-project-root-files "requirements.txt")
  (add-to-list 'projectile-project-root-files "setup.py"))


;;------------------------------------------------------------------------------
;; Keybinds: General Infixes
;;------------------------------------------------------------------------------
;; Define the infix with the title here so we don't have to worry about what's
;; defined in what order since you can only define the title once or else things
;; overwrite each other?
;;
;; TODO: Make sure that's a correct assumption. Currently only 87% sure.

;; TODO-meow: only create these leaders if evil.
(imp:eval:after (:keybinds user general)
  (keybind:leader/local:def
   :keymaps 'python-mode-map
   :infix   "i"                      ; insert
   "" '(nil :which-key "insert...")) ; infix's title


  (keybind:leader/local:def
   :keymaps 'python-mode-map
   :infix   "t"                    ; test
   "" '(nil :which-key "test...")) ; infix's title


  (keybind:leader/local:def
   :keymaps 'python-mode-map
   :infix   "e"                           ; pipenv/conda/etc
   "" '(nil :which-key "environment...")) ; infix's title
  )


;;------------------------------------------------------------------------------
;; Helper Functions
;;------------------------------------------------------------------------------

;; TODO: Move to some elisp module/package?
(defun python:module:installed? (module)
  "Return non-nil if Python MODULE is installed.

MODULE should be a string of the module name."
  (unless (stringp module)
    (nub:error
        :innit
        "python:module:installed?"
      "MODULE must be a string! Got `%S': %S"
      (type-of module)
      module))

  ;; We use this in set-up so `python-shell-interpreter' might not exist yet...
  (let ((exe/python (or (and (bound-and-true-p python-shell-interpreter)
                             python-shell-interpreter)
                        "python")))
    ;; If we can import the module name, it exists.
    ;; If we cannot, it'll have a non-zero exit and we'll translate that to a "no".
    (= (call-process exe/python
                     nil nil nil
                     "-c" (concat "import " module))
       0)))
;; (python:module:installed? "setuptools")


;;------------------------------------------------------------------------------
;; Python Mode
;;------------------------------------------------------------------------------
;; A built-in module.

(imp:use-package python
  :mode
  (("[./]flake8\\'" . conf-mode)  ;; "ini format"
   ;; ;; NOTE: This is actually a TOML file, so set it to use TOML mode instead (in "toml.el").
   ;; ("/Pipfile\\'"   . conf-mode) ;; TOML format
   )

  ;;------------------------------
  :init
  ;;------------------------------

  (innit:hook:defun
      (:name   'python:settings
       :file   macro<imp>:path/file
       :docstr "Settings for Python mode. Non-LSP stuff.")

    ;; Python (`pycodestyle', technically) defaults to wanting 79, not 80, width code.
    ;; Otherwise `fill-column wide' is a decent default.
    ;;   (jerky:get 'fill-column 'wide))
    ;; We'll just try sticking with 79 to start with.
    (setq fill-column 79)

    (setq tab-width python-indent-offset)

    ;; Separate camel-case into separate words?
    ;; (subword-mode t)
    )

  (innit:hook:defun
      (:name   'python:flycheck
       :file   macro<imp>:path/file
       :docstr (mapconcat #'identity
                          '("Use the correct Python executables for Flycheck."
                            ""
                            "From Doom's `+python-use-correct-flycheck-executables-h' in \"modules/lang/python/config.el\".")
                          "\n"))
    (let ((executable python-shell-interpreter))
      ;; Try to get a Python executable from this Python file.
      (save-excursion
        (goto-char (point-min))
        (save-match-data
          (when (or (looking-at "#!/usr/bin/env \\(python[^ \n]+\\)")
                    (looking-at "#!\\([^ \n]+/python[^ \n]+\\)"))
            (setq executable (substring-no-properties (match-string 1))))))
      ;; Try to compile using the appropriate version of Python for
      ;; the file.
      (setq-local flycheck-python-pycompile-executable executable)
      ;; We might be running inside a virtualenv, in which case the
      ;; modules won't be available. But calling the executables
      ;; directly will work.
      (setq-local flycheck-python-pylint-executable "pylint")
      (setq-local flycheck-python-flake8-executable "flake8")))

  ;; TODO-LSP: disable some error messages:
  ;; ;; Tell some annoying LSP messages to f right off back where
  ;; ;; they came from...
  ;; (when (imp:flag? :dev-env 'languages 'lsp) ;; TODO-LSP: What is the "are we doing LSP?" check?
  ;;   (customize-set-variable 'lsp-pyls-plugins-pycodestyle-ignore
  ;;                           ;; spaces before colon
  ;;                           ;; I like to line things up.
  ;;                           '("E203"
  ;;                             ;; spaces after colon...
  ;;                             "E241"
  ;;                             ;; spaces before equals...
  ;;                             "E221"
  ;;                             ;; spaces after equals...
  ;;                             "E222"
  ;;                             ;; spaces before keyword...
  ;;                             "E271"
  ;;                             ;; spaces after keyword...
  ;;                             "E272"
  ;;                             ;; Breaking before binary operator
  ;;                             ;; ...Complains about valid PEP-8 things.
  ;;                             "W503"
  ;;                             ;; Breaking after binary operator
  ;;                             ;; ...Complains about valid PEP-8 things.
  ;;                             ;; Slightly less valid, still valid.
  ;;                             "W504"
  ;;                             ))
  ;;   ;; §-TODO-§ [2019-10-24]: this doesn't work.
  ;;   ;; pylint complains at top of file:
  ;;   ;; "No module named 'disabled=C0326'
  ;;   ;; (customize-set-variable 'lsp-pyls-plugins-pylint-args
  ;;   ;;                         ;; spaces before colon... also?
  ;;   ;;                         ;; I like to line things up.
  ;;   ;;                         '("disable=C0326"))
  ;;   )

  ;;------------------------------
  :hook
  ;;------------------------------
  ((python-mode-hook . mantle:hook:python:settings)
   (python-mode-hook . mantle:hook:python:flycheck))

  ;;------------------------------
  :custom
  ;;------------------------------
  ;; TODO: WTF is this?
  ;; Oh... it's for another, different package?
  ;; https://github.com/tkf/emacs-python-environment
  ;; (python-environment-directory doom-cache-dir)

  (python-indent-guess-indent-offset-verbose nil) ; Squelch the message about not being able to guess indent.
  (python-indent-offset (jerky:get 'code 'tab 'standard))
  (python-fill-docstring-style 'symmetric)


  ;;------------------------------
  :config
  ;;------------------------------

  ;; Default to Python 3. Prefer the versioned Python binaries since some
  ;; systems stupidly make the unversioned one point at Python 2.
  (when (and (executable-find "python3")
             (string= python-shell-interpreter "python"))
    (setq python-shell-interpreter "python3"))

  ;; Affects pyenv and conda
  (when (functionp #'mantle:advice:doom-modeline:env:update/all-windows)
    (advice-add #'pythonic-activate :after-while #'mantle:advice:doom-modeline:env:update/all-windows))
  (when (functionp #'mantle:advice:doom-modeline:env:clear/all-windows)
    (advice-add #'pythonic-deactivate :after #'mantle:advice:doom-modeline:env:clear/all-windows)))


;;------------------------------------------------------------------------------
;; Python Imports
;;------------------------------------------------------------------------------

(imp:use-package pyimport
  :after python)


(imp:use-package py-isort
  :after python)


;;------------------------------
;; Keybinds : Meow
;;------------------------------

(imp:use-package pyimport
  :when  (imp:flag? :keybinds +meow)
  :after (:and python meow)

  ;;------------------------------
  :config
  ;;------------------------------

  (transient-define-prefix mantle:meow/transient:python/import ()
    "`python-mode' import menu"
    ["Import..."
     ("i" "Insert missing import for item at point." pyimport-insert-missing)
     ("r" "Remove unused imports from file." pyimport-remove-unused)])

  (mantle:meow:leader/local:key python-mode-map
                                "i" 'mantle:meow/transient:python/import))


(imp:use-package py-isort
  :when  (imp:flag? :keybinds +meow)
  :after (:and python meow)

  ;;------------------------------
  :config
  ;;------------------------------

  (transient-define-prefix mantle:meow/transient:python/sort ()
    "`python-mode' sort menu"
    ["Sort..."
     ("s" "Sort imports" py-isort-buffer)
     ("r" "Sort region" py-isort-region)])

  (mantle:meow:leader/local:key python-mode-map
                                "s" 'mantle:meow/transient:python/sort))


;;------------------------------
;; Keybinds : Evil
;;------------------------------

(imp:use-package pyimport
  :when  (imp:flag? :keybinds +evil)
  :after (:and python evil evil-collection)

  ;;------------------------------
  :general ; evil
  ;;------------------------------
  (:prefix  (keybind:prefix :local "i")
   :states  keybind:leader/local:states
   :keymaps 'python-mode-map
   "i" (list #'pyimport-insert-missing :which-key "Insert missing imports")
   "r" (list #'pyimport-remove-unused :which-key "Remove unused imports")))


(imp:use-package py-isort
  :when  (imp:flag? :keybinds +evil)
  :after (:and python evil evil-collection)

  ;;------------------------------
  :general ; evil
  ;;------------------------------
  (:prefix  (keybind:prefix :local "i")
   :states  keybind:leader/local:states
   :keymaps 'python-mode-map
   "s" (list #'py-isort-buffer :which-key "Sort imports")
   "r" (list #'py-isort-region :which-key "Sort region")))


;;------------------------------------------------------------------------------
;; Python Environment
;;------------------------------------------------------------------------------
;; TODO: Do I need all of these?
;; Like `pipenv' and `pyvenv' AND `pyenv-mode'?
;; Like `conda' AND `anaconda'?

;;------------------------------
;; Pipenv (Pipfile, Pipfile.lock)
;;------------------------------
;; Pipenv is a tool that aims to bring the best of all packaging worlds to the
;; Python world. It manages virtual environments, adds and removes packages, and
;; enables deterministic build dependencies.

(imp:use-package pipenv
  :commands pipenv-project-p

  ;; ;;------------------------------
  ;; :init
  ;; ;;------------------------------
  ;; TODO: Doom does this but do I want to disable the `pipenv' integration with `projectile'?
  ;; (setq pipenv-with-projectile nil)

  ;;------------------------------
  :hook
  ;;------------------------------
  (python-mode . pipenv-mode))


;;------------------------------
;; Keybinds : Meow
;;------------------------------

(imp:use-package pipenv
  :when  (imp:flag? :keybinds +meow)
  :after (:and python meow)

  ;;------------------------------
  :config
  ;;------------------------------

  (transient-define-prefix mantle:meow/transient:python/pipenv ()
    "`pipenv' keybinds menu"
    ["`pipenv`"
     ("a" "activate" pipenv-activate)
     ("d" "deactivate" pipenv-deactivate)
     ("i" "install" pipenv-install)
     ("l" "lock" pipenv-lock)
     ("o" "open module" pipenv-open)
     ("r" "run" pipenv-run)
     ("s" "shell" pipenv-shell)
     ("u" "uninstall" pipenv-uninstall)])

  (mantle:meow:leader/local:key python-mode-map
                                "e" 'mantle:meow/transient:python/pipenv))


;;------------------------------
;; Keybinds : Evil
;;------------------------------

(imp:use-package pipenv
  :when  (imp:flag? :keybinds +evil)
  :after (:and python evil evil-collection)

  ;;------------------------------
  :general ; evil
  ;;------------------------------
  (:prefix  (keybind:prefix :local "e")
   :states  keybind:leader/local:states
   :keymaps 'python-mode-map
   "a" (list #'pipenv-activate   :which-key "activate")
   "d" (list #'pipenv-deactivate :which-key "deactivate")
   "i" (list #'pipenv-install    :which-key "install")
   "l" (list #'pipenv-lock       :which-key "lock")
   "o" (list #'pipenv-open       :which-key "open module")
   "r" (list #'pipenv-run        :which-key "run")
   "s" (list #'pipenv-shell      :which-key "shell")
   "u" (list #'pipenv-uninstall  :which-key "uninstall")))


;;------------------------------
;; Pyvenv
;;------------------------------
;; Replicate the changes done by `virtualenv' activation inside Emacs.

(imp:use-package pyvenv
  :after python

  ;;------------------------------
  :init
  ;;------------------------------
  (when (functionp #'mantle:advice:doom-modeline:env:update/all-windows)
    (add-hook 'pyvenv-post-activate-hooks  #'mantle:advice:doom-modeline:env:update/all-windows))
  (when (functionp #'mantle:advice:doom-modeline:env:clear/all-windows)
    (add-hook 'pyvenv-pre-deactivate-hooks #'mantle:advice:doom-modeline:env:clear/all-windows))

  ;;------------------------------
  :hook
  ;;------------------------------
  (python-mode-local-vars-hook . pyvenv-track-virtualenv)

  ;;------------------------------
  :config
  ;;------------------------------
  (add-to-list 'global-mode-string
               '(pyvenv-virtual-env-name (" venv:" pyvenv-virtual-env-name " "))
               'append))


;;------------------------------
;; Integrate `pyenv' with `python-mode'
;;------------------------------
;; This was optional in Doom, and I never opted for it.
;; Include if the exe is installed.
(when (executable-find "pyenv")
  (imp:use-package pyenv-mode
    :after python

    ;;------------------------------
    :init
    ;;------------------------------

    (defvar-local mantle:user:python:pyenv/version nil
      "`pyenv-mode' version")

    (defun mantle:user:python:pyenv:read-version-from-file ()
      "Read `pyenv' version from '.python-version' file."
      (when-let* ((_ (featurep 'projectile))
                  (path/root (projectile-locate-dominating-file default-directory ".python-version"))
                  (path/file (expand-file-name ".python-version" path/root))
                  (version
                   (with-temp-buffer
                     (insert-file-contents-literally path/file)
                     (string-trim (buffer-string)))))
        (if (member version (pyenv-mode-versions))
            version  ;; return.
          (message "pyenv: version `%s' is not installed (set by `%s')."
                   version path/file))))

    (innit:hook:defun
        (:name   'python:pyenv:version
         :file   macro<imp>:path/file
         :docstr "Set `pyenv-mode' version from buffer-local variable.")
      (when (eq major-mode 'python-mode)
        (when (not (local-variable-p 'mantle:user:python:pyenv/version)) ;; Not set in this buffer yet?
          ;; Don't need to make it local as it's `defvar-local', so automatic.
          (setq mantle:user:python:pyenv/version (mantle:user:python:pyenv:read-version-from-file)))
        ;; Do we actually have a `pyenv' version?
        (if mantle:user:python:pyenv/version
            (pyenv-mode-set mantle:user:python:pyenv/version)
          (pyenv-mode-unset))))

    ;;------------------------------
    :hook
    ;;------------------------------
    ((python-mode-local-vars-hook . mantle:hook:python:pyenv:version)
     ;; Doom had `doom-switch-buffer-hook', which just ran on both of these hooks.
     ;; `window-buffer-change-functions' doesn't trigger for files visited via the
     ;; server, so `server-visit-hook' is also used.
     ((window-buffer-change-functions server-visit-hook) . mantle:hook:python:pyenv:version))

    ;;------------------------------
    :config
    ;;------------------------------
    (pyenv-mode +1)
    (add-to-list 'exec-path (expand-file-name "shims" (or (getenv "PYENV_ROOT") "~/.pyenv")))))


;;------------------------------
;; Conda
;;------------------------------

(if (not (executable-find "conda"))
    (nub:info
        :innit
        (path:current:file)
      "Need executable `conda' installed in order to use package `conda'.")

  (imp:use-package conda
    :after python

    ;;------------------------------
    :config
    ;;------------------------------
    ;; The location of your anaconda home will be guessed from a list of common
    ;; possibilities, starting with `conda-anaconda-home''s default value (which
    ;; will consult the `ANACONDA_HOME' envvar, if it exists).
    ;;
    ;; If none of these work for you, `conda-anaconda-home' must be set
    ;; explicitly. Afterwards, run M-x `conda-env-activate' to switch between
    ;; environments
    (unless (cl-loop for dir in (list conda-anaconda-home
                                      "~/.anaconda"
                                      "~/.miniconda"
                                      "~/.miniconda3"
                                      "~/.miniforge3"
                                      "~/anaconda3"
                                      "~/miniconda3"
                                      "~/miniforge3"
                                      "~/opt/miniconda3"
                                      "/usr/bin/anaconda3"
                                      "/usr/local/anaconda3"
                                      "/usr/local/miniconda3"
                                      "/usr/local/Caskroom/miniconda/base"
                                      "~/.conda")
                     if (path:exists? dir :dir)
                     return (setq conda-anaconda-home (path:absolute dir)
                                  conda-env-home-directory (path:absolute dir)))
      (nub:warning
          :innit
          (path:current:file)
        "Cannot find `conda' directory."))

    ;; Integration with term/eshell
    (conda-env-initialize-interactive-shells)
    (imp:eval:after eshell
      (conda-env-initialize-eshell))

    (add-to-list 'global-mode-string
                 '(conda-env-current-name (" conda:" conda-env-current-name " "))
                 'append)))


;;------------------------------
;; Anaconda
;;------------------------------

(imp:eval:after python
  ;; `anaconda-mode' requires Python module "setuptools"
  (if (not (python:module:installed? "setuptools"))
      (nub:warning
          :innit
          (path:current:file)
        "Need Python module `setuptools' installed in order to use package `anaconda-mode'.")

    (imp:use-package anaconda-mode
      ;; Defer loading; we'll load in a hook if we want this.
      :defer t

      ;;------------------------------
      :init
      ;;------------------------------

      (innit:hook:defun
          (:name   'python:anaconda:enable/maybe
           :file   macro<imp>:path/file
           :docstr "Enable `anaconda-mode' if `lsp-mode' is absent and `python-shell-interpreter' is present.")
        (unless (or (bound-and-true-p lsp-mode)
                    (bound-and-true-p eglot--managed-mode)
                    (bound-and-true-p lsp--buffer-deferred)
                    (not (executable-find python-shell-interpreter)))
          (anaconda-mode +1)
          (when (imp:mode? 'evil-mode)
            (evil-normalize-keymaps))))

      (innit:hook:defun
          (:name   'python:anaconda:processes:auto-kill
           :file   macro<imp>:path/file
           :docstr "Kill anaconda processes if this buffer is the last python buffer.")
        (when (and (eq major-mode 'python-mode)
                   (not (delq (current-buffer)
                              (buffer:list:mode 'python-mode (buffer-list)))))
          (anaconda-mode-stop)))

      (innit:hook:defun
          (:name   'python:anaconda:processes:auto-kill/local
           :file   macro<imp>:path/file
           :docstr (mapconcat #'identity
                              '("Add a local hook to auto-kill the buffer's anaconda processes."
                                ""
                                "Add `mantle:hook:python:anaconda:processes:auto-kill' to `kill-buffer-hook'.")
                              "\n"))
        (add-hook 'kill-buffer-hook #'mantle:hook:python:anaconda:processes:auto-kill
                  nil
                  'local))


      ;;------------------------------
      :hook
      ;;------------------------------
      ((anaconda-mode-hook          . anaconda-eldoc-mode)
       (python-mode-local-vars-hook . mantle:hook:python:anaconda:enable/maybe)
       (python-mode-hook            . mantle:hook:python:anaconda:processes:auto-kill/local))


      ;; ;;------------------------------
      ;; :custom
      ;; ;;------------------------------
      ;;
      ;; ;; TODO: What does this do, exactly?
      ;; ;; (anaconda-mode-eldoc-as-single-line t)


      ;; ;;------------------------------
      ;; :config
      ;; ;;------------------------------
      ;;
      ;; TODO: If I add `company':
      ;; (set-company-backend! 'anaconda-mode '(company-anaconda))
      )


    ;;------------------------------
    ;; Keybinds : Meow
    ;;------------------------------

    (imp:use-package anaconda-mode
      ;; Defer loading; we'll load in a hook if we want this.
      :defer t
      :when  (imp:flag? :keybinds +meow)
      :after (:and python meow)

      ;;------------------------------
      :config
      ;;------------------------------
      (transient-define-prefix mantle:meow/transient:python/anaconda ()
        "`anaconda' keybinds menu"
        ["Anaconda..."
         ("d" "Find Definitions" anaconda-mode-find-definitions)
         ("h" "Show Doc" anaconda-mode-show-doc)
         ("a" "Find Assignments" anaconda-mode-find-assignments)
         ("f" "Find File" anaconda-mode-find-file)
         ("u" "Find References" anaconda-mode-find-references)])

      (mantle:meow:leader/local:key python-mode-map
                                    "a" 'mantle:meow/transient:python/anaconda))


    ;;------------------------------
    ;; Keybinds : Evil
    ;;------------------------------

    (imp:use-package anaconda-mode
      ;; Defer loading; we'll load in a hook if we want this.
      :defer t
      :when  (imp:flag? :keybinds +evil)
      :after (:and python evil evil-collection)

      ;;------------------------------
      :general ; evil
      ;;------------------------------
      (:prefix  (keybind:prefix :local "g")
       :states  keybind:leader/local:states
       :keymaps 'anaconda-mode-map
       "" '(nil :which-key "goto...") ; infix's title

       "d" (list #'anaconda-mode-find-definitions :which-key "Find Definitions")
       "h" (list #'anaconda-mode-show-doc         :which-key "Show Doc")
       "a" (list #'anaconda-mode-find-assignments :which-key "Find Assignments")
       "f" (list #'anaconda-mode-find-file        :which-key "Find File")
       "u" (list #'anaconda-mode-find-references  :which-key "Find References")))))


;;------------------------------
;; Autocompletion in PIP requirements files?
;;------------------------------

;; TODO: Do I want auto-completion in PIP requirements files? Does this use `company'? Do I use `company'?
;; (use-package! pip-requirements
;;   :defer t
;;   :config
;;   ;; HACK `pip-requirements-mode' performs a sudden HTTP request to
;;   ;;   https://pypi.org/simple, which causes unexpected hangs (see #5998). This
;;   ;;   advice defers this behavior until the first time completion is invoked.
;;   ;; REVIEW More sensible behavior should be PRed upstream.
;;   (defadvice! +python--init-completion-a (&rest args)
;;     "Call `pip-requirements-fetch-packages' first time completion is invoked."
;;     :before #'pip-requirements-complete-at-point
;;     (unless pip-packages (pip-requirements-fetch-packages)))
;;   (defadvice! +python--inhibit-pip-requirements-fetch-packages-a (fn &rest args)
;;     "No-op `pip-requirements-fetch-packages', which can be expensive."
;;     :around #'pip-requirements-mode
;;     (letf! ((#'pip-requirements-fetch-packages #'ignore))
;;       (apply fn args))))


;;------------------------------------------------------------------------------
;; Python Testing
;;------------------------------------------------------------------------------

(imp:use-package python-pytest
  :after python
  :commands python-pytest-dispatch)


;;------------------------------
;; Keybinds : Meow
;;------------------------------

(imp:use-package python-pytest
  ;; Defer loading; we'll load in a hook if we want this.
  :defer t
  :when  (imp:flag? :keybinds +meow)
  :after (:and python meow)
  :commands python-pytest-dispatch

  ;;------------------------------
  :config
  ;;------------------------------
  (transient-define-prefix mantle:meow/transient:python/pytest ()
    "`pytest' keybinds menu"
    ["Pytest..."
     ("a" "Run all tests" python-pytest)
     ("f" "DWIM: Run tests on file" python-pytest-file-dwim)
     ("F" "Run tests on file" python-pytest-file)
     ("t" "DWIM: Run tests on function" python-pytest-function-dwim)
     ("T" "Run tests on function" python-pytest-function)
     ("r" "Repeat tests" python-pytest-repeat)
     ("p" "Pytest Popup..." python-pytest-dispatch)])

  (mantle:meow:leader/local:key python-mode-map
                                "t" 'mantle:meow/transient:python/pytest))


;;------------------------------
;; Keybinds : Evil
;;------------------------------

(imp:use-package python-pytest
  ;; Defer loading; we'll load in a hook if we want this.
  :defer t
  :when  (imp:flag? :keybinds +evil)
  :after (:and python evil evil-collection)
  :commands python-pytest-dispatch

  ;;------------------------------
  :general ; evil
  ;;------------------------------
  (:prefix  (keybind:prefix :local "t")
   :states  keybind:leader/local:states
   :keymaps 'python-mode-map
   "a" (list #'python-pytest               :which-key "Run all tests")
   "f" (list #'python-pytest-file-dwim     :which-key "DWIM: Run tests on file")
   "F" (list #'python-pytest-file          :which-key "Run tests on file")
   "t" (list #'python-pytest-function-dwim :which-key "DWIM: Run tests on function")
   "T" (list #'python-pytest-function      :which-key "Run tests on function")
   "r" (list #'python-pytest-repeat        :which-key "Repeat tests")
   "p" (list #'python-pytest-dispatch      :which-key "Pytest Popup...")))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'config 'user 'dev-env 'languages 'python)

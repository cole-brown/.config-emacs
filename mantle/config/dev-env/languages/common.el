;;; mantle/config/dev-env/languages/common.el --- Configure All Languages -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2022-08-05
;; Modified:   2022-12-09
;; URL:        https://github.com/cole-brown/.config-emacs
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;  Configure for All Languages
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Comments
;;------------------------------------------------------------------------------

;;------------------------------
;; Block Commenting Style
;;------------------------------

;; Only use for languages that have ending comments, like C's "/* */".
(innit:hook:defun
    (:name   'dev-env:languages/comments:block/align
     :file   (path:current:file)
     :docstr "Set comment style for start/end line comment languages (C, HTML, ...).")
  ;; `aligned' is like (default) `indent' but also aligns end-of-line comment chars if the language has them.
  (setq 'comment-style 'aligned))
;; NOTE: Need to add this hook (`mantle:hook:dev-env:languages/comments:block/align') to whatever modes want it.

;; TODO: Move to "languages/html.el"?
;; `html-mode-hook' (HTML mode) or `mhtml-mode-hook' (HTML+ mode (derived from HTML mode))?
(add-hook 'html-mode-hook 'mantle:hook:comments:block/align)

;; NOTE: C-mode hook added (or not) in "language/c-and-cpp.el".


;;------------------------------------------------------------------------------
;; Defaults for All Programming Modes
;;------------------------------------------------------------------------------

;;------------------------------
;; Fill Columns
;;------------------------------
;;                                                                             80                                     120                 140       150

(jerky:set 'fill-column 'narrow
           :namespace :default
           :value 80
           :docstr "Ye olde default 80 columns.")
;; NOTE: Python likes to be weird - it defaults to wanting 79 columns.


(jerky:set 'fill-column 'wide
           :namespace :default
           :value 120
           :docstr "Current Year should be able to handle more than 80, right? How about 120 columns?")


(jerky:set 'fill-column 'maximized '1080px ; 1920x1080
           :namespace :default
           :value 150
           :docstr "Two side-by-side buffers can handle about 150 columns maximum (w/ current font, gutters, etc).")


(jerky:set 'fill-column 'maximized '1440px ; 2560x1440
           :namespace :default
           :value 150
           :docstr "Two side-by-side buffers can handle about 150 columns maximum (w/ current font, gutters, etc).")


(jerky:set 'fill-column 'maximized '1920px ; 1080x1920 aka sideways 1920x1080
           :namespace :default
           :value 170
           :docstr "One buffers can handle about 170 columns maximum (w/ current font, gutters, etc).")


(jerky:set 'fill-column 'maximized '2560px ; 1440x2560 aka sideways 2560x1440
           :namespace :default
           :value 170
           :docstr "One buffers can handle about 170 columns maximum (w/ current font, gutters, etc).")


;; TODO: Set `fill-column' for other modes too...
;;   Yes: org-mode, org-journal-mode
;;   No!: magit
;; Will have to move the `jerky:set' calls to earlier in config.
(innit:hook:defun-and-add prog-mode-hook
    (:name 'prog:settings
     :file (path:current:file)
     :docstr "Settings for all prog-mode derived modes. Non-LSP stuff.")

  ;; 'wide' is a decent default, probably?
  (setq fill-column (jerky:get 'fill-column 'wide))

  (setq tab-width (jerky:get 'code 'tab 'standard)))


;;------------------------------------------------------------------------------
;; Metasyntatic Variables
;;------------------------------------------------------------------------------

(defvar mantle:user:code:variables/metasyntatic
  '((:metasyntactic
     (foo (bar (baz (qux (quux (quuux (quuuux (quuuuux))))))
               (thud (grunt))
               (bletch)
               (fum)
               (bongo)
               (zot)))
     (bazola (ztesch))
     (fred (jim (sheila (barney))))
     (corge (grault (flarp)))
     (zxc (spqr (wombat)))
     (shme)
     (spam (eggs))
     (snork)
     (blarg (wibble))
     (toto (titi (tata (tutu))))
     (pippo (pluto (paperino)))
     (aap (noot (mies)))
     (oogle (foogle (boogle (zork (gork (bork)))))))

    (:pinky (narf (zort (poit (egad (troz (fiddely-posh))))))))
  "Alist of meaningless/placeholder variable name progressions.

See: http://www.catb.org/jargon/html/M/metasyntactic-variable.html")


;; (defun mantle:user:code:variable/metasyntatic (&optional type)
;;   "Insert a metasyntatic variable at point.
;;
;; If TYPE is a keyword, use that list in variable
;; `mantle:user:code:variable/metasyntatic'."
;;   (interactive)
;;   (let ((tree-of-meta (alist-get (or nil :metasyntactic) mantle:user:code:variables/metasyntatic)))
;;     ;; TODO: Would be nice to progress in the metavars usage but idk how to, easily.
;;     ;; So just... chose a random?
;;     ;; TODO: finish this?
;;     ))


;;------------------------------------------------------------------------------
;; Which Function?
;;------------------------------------------------------------------------------

(imp:use-package which-func
  :ensure  nil ; This is an Emacs built-in feature.

  ;;------------------------------
  :config
  ;;------------------------------

  ;; Enable `which-function-mode', which will enable the function info for
  ;; whatever modes it supports.
  (which-function-mode +1)

  ;; Display which-function info in a header line instead of in the mode line.
  (setq-default header-line-format
                '((which-func-mode ("" which-func-format " "))))

  (setq mode-line-misc-info
        ;; Remove the current function name from the mode line, because it's
        ;; mostly invisible here anyway.
        (assq-delete-all 'which-func-mode mode-line-misc-info)))


;; ;;------------------------------------------------------------------------------
;; ;; Which Language?
;; ;;------------------------------------------------------------------------------
;; Don't technically need this; it's just used by `format-all' and looks useful.
;; https://github.com/lassik/emacs-language-id
;; (imp:use-package language-id
;;   ;; TODO: Erm... why does my `use-package' not have the `:autoload' keyword in `use-package-keywords`?!
;;   ;; It exists in latest `use-package':
;;   ;;   https://github.com/jwiegley/use-package/blob/bcf0984cf55b70fe6896c6a15f61df92b24f8ffd/use-package-core.el#L71
;;   :autoload language-id-buffer)


;;------------------------------------------------------------------------------
;; Documentation
;;------------------------------------------------------------------------------

;; Turn on the online documentation mode for all programming modes (not all of
;; them support it).
(imp:use-package eldoc
  :ensure nil ; This is an Emacs built-in feature.
  :delight    ; Don't put in the modes in the modeline.
  :hook
  (prog-mode . turn-on-eldoc-mode))


;;------------------------------------------------------------------------------
;; Formatting
;;------------------------------------------------------------------------------

;; https://github.com/lassik/emacs-format-all-the-code
;; NOTE: This relies on external programs for the actual formatting (e.g. LSPs,
;; `rustfmt`, `terraform fmt`...)
(imp:use-package format-all
  :delight ; Don't put in the modes in the modeline.

  :autoload format-all--language-id-buffer

  ;;------------------------------
  :init
  ;;------------------------------

  (defcustom mantle:user:format-all:on-save/disabled
    '("Emacs Lisp"     ; elisp's mechanisms are good enough... but not 100% so auto-format would fuck up shit.

      ;; python-mode   ; Trial [2022-12-22]: Test out the python formatting.    ; Don't even want to know what this would do if it has one.
      ;; csharp-mode   ; TODO: Should I disable this one (again)?
      ;; sql-mode      ; Doom says: "sqlformat is currently broken"
      ;; tex-mode      ; Doom says: "latexindent is broken"
      ;; latex-mode    ; Doom says: "" (probably just ditto to `tex-mode')

      fundamental-mode) ; Doom hard-coded this one.
    "Modes/languages for which we do not enable `format-all' on save.

Values in this list should be one of:
  - major-mode symbols
  - language strings from `language-id' package
  - language strings from `format-all' package

For language strings, see:
  - `format-all-default-formatters' or `format-all--language-id-buffer'
  - `language-id' package
    - `language-id--definitions'
    - https://raw.githubusercontent.com/github/linguist/master/lib/linguist/languages.yml

Originally from Doom's `+format-on-save-enabled-modes' in \"modules/editor/format/config.el\"."
    :group 'innit:group
    :type  '(set (string :tag "`language-id' string")
                 (symbol :tag "`major-mode' symbol")))

  (innit:hook:defun
      (:name   'format-all:enable?
       :docstr "Decide whether or not to enable auto-formatting on save.")

    (let ((language (format-all--language-id-buffer))) ; string, likely from `language-id-buffer'.
      ;;------------------------------
      ;; Don't Enable If...
      ;;------------------------------
      ;; No known language for buffer.
      (cond ((null language))

            ;; Explicitly disabled.
            ((or (seq-contains-p mantle:user:format-all:on-save/disabled language)
                 (seq-contains-p mantle:user:format-all:on-save/disabled major-mode)))

            ;; Hidden buffers probably don't need auto-formatting?
            ((string-prefix-p " " (buffer-name)))

            ;; Can we actually load `format-all'?
            ((not (require 'format-all nil :no-error)))

            ;; NOTE: Formatting with the LSP vs the `format-all' exe is decided
            ;; in the `format-all--probe' advice in the `:config' section.

            ;;------------------------------
            ;; Ok; Enable.
            ;;------------------------------
            (t
             (format-all-mode +1)))))

  ;;------------------------------
  :hook
  ;;------------------------------
  (after-change-major-mode-hook . mantle:hook:format-all:enable?)


  ;;------------------------------
  :config
  ;;------------------------------

  (define-advice format-all--probe (:around (fn &rest args) mantle:user:format-all:use-lsp?)
    "Get the formatter for the current buffer.

Answer questions like:
  - Should buffer even be formatted (e.g. read-only buffers)?
  - Should formatting be done with LSP formatter instead?"
    ;;------------------------------
    ;; Never Format
    ;;------------------------------
    (cond ((or buffer-read-only (eq +format-with :none))
           (list nil nil))

          ;;------------------------------
          ;; Use the LSP?
          ;;------------------------------
          ;; Always use the LSP if it has the capability to format?
          ;; If that's too optimistic, could make another filter var like
          ;; `mantle:user:format-all:on-save/disabled'.
          ((and (bound-and-true-p lsp-managed-mode)
                (lsp-feature? "textDocument/formatting"))
           (list 'lsp nil))

          ((and (bound-and-true-p eglot--managed-mode)
                (eglot--server-capable :documentFormattingProvider))

           (list 'eglot nil))

          ;;------------------------------
          ;; Actually let `format-all' decide:
          ;;------------------------------
          ((funcall fn))))

  ;; NOTE: Do not enable `format-all-mode' globally; it's optionally enabled in its hook.
  )


;;------------------------------------------------------------------------------
;; <{[( Parentheseses )]}>
;;------------------------------------------------------------------------------

;; https://github.com/Fanael/rainbow-delimiters
(imp:use-package rainbow-delimiters

  ;;------------------------------
  ;; NOTE: Usage
  ;;------------------------------
  ;; Enable on a per-major-mode basis in a hook.
  ;; `use-package' Example:
  ;;   ;;------------------------------
  ;;   :hook
  ;;   ;;------------------------------
  ;;   (js-mode-hook . rainbow-delimiters-mode)


  ;;------------------------------
  :custom
  ;;------------------------------

  ;; Helps us distinguish stacked delimiter pairs, especially in parentheses-drunk
  ;; languages like Lisp (default: 9).
  (rainbow-delimiters-max-face-count 4))


(imp:use-package paren
  :ensure nil ; This is an Emacs built-in feature.

  ;;------------------------------
  :hook
  ;;------------------------------
  (innit:theme:load:hook . show-paren-mode)


  ;;------------------------------
  :custom
  ;;------------------------------

  ;; Seconds to delay before showing the matching parenthesis.
  ;; (show-paren-delay 0.1)

  (show-paren-highlight-openparen     t)
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'config 'user 'dev-env 'languages 'common)

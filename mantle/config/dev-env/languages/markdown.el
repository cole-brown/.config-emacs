;;; mantle/config/dev-env/markdown.el --- Markup with Markdown! -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2023-01-03
;; Modified:   2023-01-03
;; URL:        https://github.com/cole-brown/.config-emacs
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Markup with Markdown!
;;
;;; Code:


(imp:require :keybind)


;;------------------------------------------------------------------------------
;; Keybinds: General Infixes
;;------------------------------------------------------------------------------
;; Define the infix with the title here so we don't have to worry about what's
;; defined in what order since you can only define the title once or else things
;; overwrite each other?
;;
;; TODO: Make sure that's a correct assumption. Currently only 87% sure.

(keybind:leader/local:def
 :keymaps (list 'markdown-mode-map keybind:leader/local:keymaps)
 :infix (keybind:infix "i")        ; insert
 "" '(nil :which-key "insert...")) ; infix's title


;;------------------------------------------------------------------------------
;; Markdown Mode
;;------------------------------------------------------------------------------

;; https://github.com/jrblevin/markdown-mode
(imp:use-package markdown-mode
  ;;------------------------------
  :mode
  ;;------------------------------
  (("/README\\(?:\\.md\\)?\\'" . gfm-mode) ;; README files are definitely GitHub Flavored Markdown.
   ;; ("\\.md$" .  markdown-mode)) ;; standard markdown
   ;; What do I do that uses standard markdown, really? Make them all use `gfm-mode'?
   ("\\.md$" .  gfm-mode))


  ;;------------------------------
  :init
  ;;------------------------------

  ;; A shorter alias for org src blocks than "markdown"
  (imp:eval:after org-src
    (add-to-list 'org-src-lang-modes '("md" . markdown)))

 (innit:hook:defun
      (:name   'markdown:settings
       :file   macro<imp>:path/file
       :docstr "Settings for Markdown mode.")

   ;; Don't trigger autofill in code blocks (see `auto-fill-mode')
   (setq fill-nobreak-predicate (cons #'markdown-code-block-at-point-p fill-nobreak-predicate)))


  ;;------------------------------
  :hook
  ;;------------------------------
  (markdown-mode-hook . mantle:hook:markdown:settings)


  ;;------------------------------
  :custom
  ;;------------------------------
  ;; [2023-01-03] Settings initially stolen from Doom.
  ;;   https://github.com/doomemacs/doomemacs/tree/master/modules/lang/markdown/config.el
  ;;   https://docs.doomemacs.org/latest/modules/lang/markdown/

  (markdown-enable-math t) ; syntax highlighting for latex fragments
  (markdown-enable-wiki-links t)

  (markdown-italic-underscore t)

  (markdown-asymmetric-header t) ;; Place header characters only on left of headers?

  (markdown-gfm-additional-languages '("sh"))

  (markdown-make-gfm-checkboxes-buttons t)

  ;; TODO [2023-01-03]: From Doom; do we want?
  ;; ;; HACK Due to jrblevin/markdown-mode#578, invoking `imenu' throws a
  ;; ;;      'wrong-type-argument consp nil' error if you use native-comp.
  ;; (markdown-nested-imenu-heading-index (not (ignore-errors (native-comp-available-p))))

  ;; TODO [2023-01-03]: From Doom; do we want?
  ;; ;; `+markdown-compile' offers support for many transpilers (see
  ;; ;; `+markdown-compile-functions'), which it tries until one succeeds.
  ;; (markdown-command #'+markdown-compile)

  ;; This is set to `nil' by default, which causes a wrong-type-arg error
  ;; when you use `markdown-open'. These are more sensible defaults.
  (markdown-open-command (cond ((innit:os:mac?)   "open")
                               ((innit:os:linux?) "xdg-open")))

  ;; TODO [2023-01-03]: From Doom; do we want?
  ;; A sensible and simple default preamble for markdown exports that
  ;; takes after the github asthetic (plus highlightjs syntax coloring).
  ;; (markdown-content-type "application/xhtml+xml")
  ;; (markdown-css-paths
  ;; '("https://cdn.jsdelivr.net/npm/github-markdown-css/github-markdown.min.css"
  ;;   "https://cdn.jsdelivr.net/gh/highlightjs/cdn-release/build/styles/github.min.css"))
  ;; (markdown-xhtml-header-content
  ;;  (concat "<meta name='viewport' content='width=device-width, initial-scale=1, shrink-to-fit=no'>"
  ;;          "<style> body { box-sizing: border-box; max-width: 740px; width: 100%; margin: 40px auto; padding: 0 10px; } </style>"
  ;;          "<script id='MathJax-script' async src='https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js'></script>"
  ;;          "<script src='https://cdn.jsdelivr.net/gh/highlightjs/cdn-release/build/highlight.min.js'></script>"
  ;;          "<script>document.addEventListener('DOMContentLoaded', () => { document.body.classList.add('markdown-body'); document.querySelectorAll('pre[lang] > code').forEach((code) => { code.classList.add(code.parentElement.lang); }); document.querySelectorAll('pre > code').forEach((code) => { hljs.highlightBlock(code); }); });</script>"))


  ;;------------------------------
  :general
  ;;------------------------------
  ;;---
  ;; Misc.
  ;;---
  ;; TODO: is this emacs or evil? ...or both?
  (:prefix  (keybind:prefix :local)
   :states  keybind:leader/local:states
   :keymaps (list 'markdown-mode-map keybind:leader/local:keymaps)
   "'" #'markdown-edit-code-block
   "e" #'markdown-export
   ;; "i" == insert menu
   "o" #'markdown-open
   "p" #'markdown-preview)

  ;;---
  ;; Insert...
  ;;---
  ;; TODO: is this emacs or evil? ...or both?
  (:prefix  (keybind:prefix :local "i")
   :states  keybind:leader/local:states
   :keymaps (list 'markdown-mode-map keybind:leader/local:keymaps)
   "T" (list #'markdown-toc-generate-toc      :which-key "Table Of Content")
   "i" (list #'markdown-insert-image          :which-key "Image")
   "l" (list #'markdown-insert-link           :which-key "Link")
   "-" (list #'markdown-insert-hr             :which-key "<hr>")
   "1" (list #'markdown-insert-header-atx-1   :which-key "Heading 1")
   "2" (list #'markdown-insert-header-atx-2   :which-key "Heading 2")
   "3" (list #'markdown-insert-header-atx-3   :which-key "Heading 3")
   "4" (list #'markdown-insert-header-atx-4   :which-key "Heading 4")
   "5" (list #'markdown-insert-header-atx-5   :which-key "Heading 5")
   "6" (list #'markdown-insert-header-atx-6   :which-key "Heading 6")
   "C" (list #'markdown-insert-gfm-code-block :which-key "Code block")
   "P" (list #'markdown-pre-region            :which-key "Pre region")
   "Q" (list #'markdown-blockquote-region     :which-key "Blockquote region")
   "[" (list #'markdown-insert-gfm-checkbox   :which-key "Checkbox")
   "b" (list #'markdown-insert-bold           :which-key "Bold")
   "c" (list #'markdown-insert-code           :which-key "Inline code")
   "e" (list #'markdown-insert-italic         :which-key "Italic")
   "f" (list #'markdown-insert-footnote       :which-key "Footnote")
   "h" (list #'markdown-insert-header-dwim    :which-key "Header dwim")
   "i" (list #'markdown-insert-italic         :which-key "Italic")
   "k" (list #'markdown-insert-kbd            :which-key "Kbd")
   "l" (list #'markdown-insert-link           :which-key "Link")
   "p" (list #'markdown-insert-pre            :which-key "Pre")
   "q" (list #'markdown-insert-blockquote     :which-key "New blockquote")
   "s" (list #'markdown-insert-strike-through :which-key "Strike through")
   "t" (list #'markdown-insert-table          :which-key "Table")
   "w" (list #'markdown-insert-wiki-link      :which-key "Wiki Link"))

  ;;------------------------------
  :config
  ;;------------------------------

  ;; Add backtick as a parenthesis type if using smartparens.
  (imp:eval:after smartparens
    (sp-local-pair '(markdown-mode gfm-mode)
                   "`" "`"
                   :unless '(:add sp-point-before-word-p sp-point-before-same-p)))

  ;; Add a language's mode to highlight code blocks correctly if/when:
  ;;   1. Using a language that `markdown-mode' doesn't already support.
  ;;   2. Using a language mode that is not a `markdown-mode' default.
  ;;
  ;; Rust fulfills both, so add whichever we're using when it shows up.
  (imp:eval:after rust-mode
    (add-to-list 'markdown-code-lang-modes '("rust" . rust-mode)))
  (imp:eval:after rustic
    (add-to-list 'markdown-code-lang-modes '("rust" . rustic-mode)))

  ;; HACK Prevent mis-fontification of YAML metadata blocks in `markdown-mode'
  ;;      which occurs when the first line contains a colon in it.
  ;;      See: https://github.com/jrblevin/markdown-mode/issues/328
  ;;        - Which, last update is [2018-07-16], so.
  (define-advice markdown-match-generic-metadata (:override (&rest _) mantle:advice:yaml-fontification/disable)
    (ignore (goto-char (point-max)))))


;;------------------------------------------------------------------------------
;; Evil Markdown - Markdown, but with a gotee?
;;------------------------------------------------------------------------------

(imp:use-package evil-markdown
  :when (imp:feature? 'evil)

  ;;------------------------------
  :hook
  ;;------------------------------
  ((markdown-mode-hook      . evil-markdown-mode)
   (evil-markdown-mode-hook . evil-normalize-keymaps))

  ;;------------------------------
  :general
  ;;------------------------------
  ;; TODO: is this emacs or evil? ...or both?
  (:prefix  (keybind:prefix :local)
   :states  'normal
   :keymaps (list 'evil-markdown-mode-map keybind:leader/local:keymaps)
   "TAB"     #'markdown-cycle
   [backtab] #'markdown-shifttab
   "M-r"     #'browse-url-of-file)

  ;; TODO: Add back in when I finally learn the secrets of VIM movement?
  ;; (:prefix  (keybind:prefix :local)
  ;;  :states  'insert
  ;;  :keymaps (list 'evil-markdown-mode-map keybind:leader/local:keymaps)
  ;;  "M-*" #'markdown-insert-list-item
  ;;  "M-b" #'markdown-insert-bold
  ;;  "M-i" #'markdown-insert-italic
  ;;  "M-`" #'+markdown/insert-del
  ;;  "M--" #'markdown-insert-hr)

  (:prefix  (keybind:prefix :local)
   :states  'motion
   :keymaps (list 'evil-markdown-mode-map keybind:leader/local:keymaps)
   "] h"  #'markdown-next-visible-heading
   "[ h"  #'markdown-previous-visible-heading
   "[ p"  #'markdown-promote
   "] p"  #'markdown-demote
   "[ l"  #'markdown-previous-link
   "] l"  #'markdown-next-link))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'config 'user 'dev-env 'markdown)

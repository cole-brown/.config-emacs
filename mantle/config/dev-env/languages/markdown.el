;;; mantle/config/dev-env/markdown.el --- Markup with Markdown! -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2023-01-03
;; Modified:   2023-01-04
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


(imp:provide :innit 'vars)


;;------------------------------------------------------------------------------
;; Keybinds: General Infixes
;;------------------------------------------------------------------------------
;; Define the infix with the title here so we don't have to worry about what's
;; defined in what order since you can only define the title once or else things
;; overwrite each other?
;;
;; TODO: Make sure that's a correct assumption. Currently only 87% sure.

(keybind:leader/local:def
 :keymaps 'markdown-mode-map
 :infix   "i"                      ; insert
 "" '(nil :which-key "insert...")) ; infix's title

(keybind:leader/local:def
 :keymaps 'markdown-mode-map
 :infix "p"                         ; preview
 "" '(nil :which-key "preview...")) ; infix's title



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
  (markdown-open-command (cond (innit:os:mac?   "open")
                               (innit:os:linux? "xdg-open")))

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


;;------------------------------
;; Keybinds : Meow
;;------------------------------

(imp:use-package markdown-mode
  :when  (imp:flag? :keybinds +meow)
  :after meow

  ;;------------------------------
  :config
  ;;------------------------------
  ;;---
  ;; Misc.
  ;;---
  (mantle:meow:leader/local:keys 'markdown-mode-map
                                 "'" #'markdown-edit-code-block
                                 "e" #'markdown-export
                                 ;; "i" == insert menu
                                 "o" #'markdown-open)

  ;;---
  ;; Preview...
  ;;---
  ;; `impatient-mode' will add more to this keymap
  (mantle:meow:keymap
      mantle:meow/keymap/local:markdown/preview
      "Preview Markdown"
    ("p" #'markdown-preview))

  (mantle:meow:leader/local:key 'markdown-mode-map
                                "p" 'mantle:meow/keymap/local:markdown/preview)

  ;;---
  ;; Insert...
  ;;---
  (mantle:meow:keymap
      mantle:meow/keymap/local:markdown/insert
      "`markdown-mode' \"insert\" keybinds"
    ("i" #'markdown-insert-image)          ; Image
    ("l" #'markdown-insert-link)           ; Link
    ("-" #'markdown-insert-hr)             ; <hr>
    ("1" #'markdown-insert-header-atx-1)   ; Heading 1
    ("2" #'markdown-insert-header-atx-2)   ; Heading 2
    ("3" #'markdown-insert-header-atx-3)   ; Heading 3
    ("4" #'markdown-insert-header-atx-4)   ; Heading 4
    ("5" #'markdown-insert-header-atx-5)   ; Heading 5
    ("6" #'markdown-insert-header-atx-6)   ; Heading 6
    ("C" #'markdown-insert-gfm-code-block) ; Code Block
    ("P" #'markdown-pre-region)            ; Pre Region
    ("Q" #'markdown-blockquote-region)     ; Blockquote Region
    ("[" #'markdown-insert-gfm-checkbox)   ; Checkbox
    ("b" #'markdown-insert-bold)           ; Bold
    ("c" #'markdown-insert-code)           ; Inline Code
    ("e" #'markdown-insert-italic)         ; Italic
    ("f" #'markdown-insert-footnote)       ; Footnote
    ("h" #'markdown-insert-header-dwim)    ; Header DWIM
    ("i" #'markdown-insert-italic)         ; Italic
    ("k" #'markdown-insert-kbd)            ; kbd
    ("l" #'markdown-insert-link)           ; Link
    ("p" #'markdown-insert-pre)            ; Pre
    ("q" #'markdown-insert-blockquote)     ; New Blockquote
    ("s" #'markdown-insert-strike-through) ; Strike Through
    ("t" #'markdown-insert-table)          ; Table
    ("w" #'markdown-insert-wiki-link))     ; Wiki Link

  (mantle:meow:leader/local:key 'markdown-mode-map
                                "i" 'mantle:meow/keymap/local:markdown/insert))


;;------------------------------
;; Keybinds : Evil
;;------------------------------

(imp:use-package markdown-mode
  :when  (imp:flag? :keybinds +evil)
  :after (:and evil evil-collection)

  ;;------------------------------
  :general ; evil
  ;;------------------------------
  ;;---
  ;; Misc.
  ;;---
  ;; TODO-meow: is this emacs or evil? ...or both?
  (:prefix  (keybind:prefix :local)
   :states  keybind:leader/local:states
   :keymaps 'markdown-mode-map
   "'" #'markdown-edit-code-block
   "e" #'markdown-export
   ;; "i" == insert menu
   "o" #'markdown-open)

  ;;---
  ;; Preview...
  ;;---
  (:prefix  (keybind:prefix :local "p")
   :states  keybind:leader/local:states
   :keymaps 'markdown-mode-map
   "p" #'markdown-preview)

  ;;---
  ;; Insert...
  ;;---
  ;; TODO-meow: is this emacs or evil? ...or both?
  (:prefix  (keybind:prefix :local "i")
   :states  keybind:leader/local:states
   :keymaps 'markdown-mode-map
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
   "w" (list #'markdown-insert-wiki-link      :which-key "Wiki Link")))


;;------------------------------------------------------------------------------
;; Markdown Table of Contents
;;------------------------------------------------------------------------------

;; https://github.com/ardumont/markdown-toc
(imp:use-package markdown-toc
  :after markdown-mode)


;;------------------------------
;; Keybinds : Meow
;;------------------------------

(imp:use-package markdown-toc
  :when  (imp:flag? :keybinds +meow)
  :after (:and markdown-mode meow)

  ;;------------------------------
  :bind ; meow
  ;;------------------------------
  ;; Add to "Insert" keybinds...
  (:map mantle:meow/keymap/local:markdown/insert
   ("T" . #'markdown-toc-generate-toc))) ; Table Of Content


;;------------------------------
;; Keybinds : Evil
;;------------------------------

(imp:use-package markdown-toc
  :when  (imp:flag? :keybinds +evil)
  :after (:and markdown-mode evil evil-collection)

  ;;------------------------------
  :general ; evil
  ;;------------------------------
  ;; TODO-meow: is this emacs or evil? ...or both?
  (:prefix  (keybind:prefix :local "i") ;; Insert...
   :states  keybind:leader/local:states
   :keymaps 'markdown-mode-map
   "T" (list #'markdown-toc-generate-toc :which-key "Table Of Content")))


;;------------------------------------------------------------------------------
;; Evil Markdown - Markdown, but with a gotee?
;;------------------------------------------------------------------------------

(imp:use-package evil-markdown
  :when (imp:feature? 'evil)
  :after (:and markdown-mode evil)
  ;; Not on a package repository so get it from GitHub:
  :straight (:type git
             :host github
             :repo "Somelauw/evil-markdown")

  ;;------------------------------
  :hook
  ;;------------------------------
  ((markdown-mode-hook      . evil-markdown-mode)
   (evil-markdown-mode-hook . evil-normalize-keymaps))

  ;;------------------------------
  :general ; evil
  ;;------------------------------
  ;; TODO: is this emacs or evil? ...or both?
  (:prefix  (keybind:prefix :local)
   :states  'normal
   :keymaps 'evil-markdown-mode-map
   "TAB"     #'markdown-cycle
   [backtab] #'markdown-shifttab
   "M-r"     #'browse-url-of-file)

  ;; TODO: Add back in when I finally learn the secrets of VIM movement?
  ;; (:prefix  (keybind:prefix :local)
  ;;  :states  'insert
  ;;  :keymaps 'evil-markdown-mode-map
  ;;  "M-*" #'markdown-insert-list-item
  ;;  "M-b" #'markdown-insert-bold
  ;;  "M-i" #'markdown-insert-italic
  ;;  "M-`" #'+markdown/insert-del
  ;;  "M--" #'markdown-insert-hr)

  (:prefix  (keybind:prefix :local)
   :states  'motion
   :keymaps 'evil-markdown-mode-map
   "] h"  #'markdown-next-visible-heading
   "[ h"  #'markdown-previous-visible-heading
   "[ p"  #'markdown-promote
   "] p"  #'markdown-demote
   "[ l"  #'markdown-previous-link
   "] l"  #'markdown-next-link))


;;------------------------------------------------------------------------------
;; Live Preview
;;------------------------------------------------------------------------------
;; NOTE: There are several alternatives to `impatient-mode' for live previews,
;; but `impatient-mode' has the distinction of only relying on Emacs packages.
;; See: https://wikemacs.org/wiki/Markdown#Live_preview_as_you_type

;; TODO: `impatient-mode' is mainly for HTML... move to HTML (or web or
;; whatever) file when/if it gets one? Or... Keep this one for markdown and make
;; another for HTML if needed?

;; To get a live preview of a Markdown buffer:
;;   1. Enable the web server provided by simple-httpd:
;;      'M-x httpd-start'
;;
;;   2. Publish buffers by enabling the minor mode impatient-mode.
;;      'M-x impatient-mode'
;;
;;   3. In a browser:
;;      1. Go to http://localhost:8080/imp/
;;      2. Select the Markdown buffer.
;;      3. Watch your changes appear as you type!
;;
;; https://github.com/skeeto/impatient-mode
(imp:use-package impatient-mode
  :after markdown-mode

  ;;------------------------------
  :init
  ;;------------------------------

  (defun mantle:user:markdown->html (buffer)
    "Convert markdown BUFFER to html string.

Should be provided to `impatient-mode' using `imp-set-user-filter'.
Can be removed via `imp-remove-user-filter'.

https://wikemacs.org/wiki/Markdown#Impatient-mode"
    (let ((func/name "mantle:user:markdown->html"))
      (if (featurep 'markdown-mode)
          ;; Works with `markdown-mode'.
          (princ
           (with-temp-buffer
             (let ((tmpname (buffer-name)))
               (set-buffer buffer)
               (set-buffer (markdown tmpname)) ; `markdown' is from `markdown-mode.el'.
               (buffer-string)))
           (current-buffer))

        ;; Non-`markdown-mode' version...
        (princ
         (with-current-buffer buffer
           (mapconcat #'identity
                      (list "<!DOCTYPE html><html><title>Impatient Markdown</title>"
                            "<xmp theme=\"united\" style=\"display:none;\">"
                            "\n"
                            (format "<!-- %s START -->" "mantle:user:markdown->html")
                            "\n"
                            (buffer-substring-no-properties (point-min) (point-max))
                            "\n"
                            (format "<!-- %s END -->" "mantle:user:markdown->html")
                            "\n"
                            "</xmp><script src=\"http://ndossougbe.github.io/strapdown/dist/strapdown.js\"></script></html>")
                      "\n")
           (current-buffer))))))


  (defun mantle:cmd:markdown:preview-live/start ()
    "View live markdown preview of the current buffer.

This will:
  1. Enable the web server provided by simple-httpd:
     'M-x httpd-start'

  2. Publish buffers by enabling the minor mode `impatient-mode'.
     'M-x impatient-mode'

  3. Open your browser to http://localhost:8080/imp/

You can then:
  1. Select the buffer in the browser.
  2. Watch your changes appear as you type!"
    (interactive)
    (if (not markdown-mode)
        (message "%s is not a `markdown-mode' buffer!"
                 (buffer-name))

      (funcall-interactively #'httpd-start)
      (impatient-mode +1)
      (imp-set-user-filter #'mantle:user:markdown->html)
      (browse-url "http://localhost:8080/imp/")
      (message "Select this buffer in the browser to see changes live!")))


  (defun mantle:cmd:markdown:preview-live/stop ()
      "Stop/kill/whatever the live markdown preview of the current buffer."
      (interactive)
      (cond ((not markdown-mode)
             (message "%s is not a `markdown-mode' buffer!"
                      (buffer-name)))
            ((not impatient-mode)
             (message "%s is not an `impatient-mode' (live preview) buffer!"
                      (buffer-name)))
            (t
             (imp-remove-user-filter)
             (impatient-mode -1)
             (funcall-interactively #'httpd-stop)))))


;;------------------------------
;; Keybinds : Meow
;;------------------------------

(imp:use-package impatient-mode
  :when  (imp:flag? :keybinds +meow)
  :after (:and markdown-mode meow)

  ;;------------------------------
  :bind ; meow
  ;;------------------------------
  (:map mantle:meow/keymap/local:markdown/preview
   ;; `markdown-mode' package puts `markdown-preview' as "p".
   ;; Steal "p", but add `markdown-preview' back somewhere else.
   ("p" . #'mantle:cmd:markdown:preview-live/start)  ; Preview (Live)
   ("P" . #'markdown-preview)                        ; Preview (Static)
   ("s" . #'mantle:cmd:markdown:preview-live/stop))) ; Stop Preview (Live)


;;------------------------------
;; Keybinds : Evil
;;------------------------------

(imp:use-package impatient-mode
  :when  (imp:flag? :keybinds +evil)
  :after (:and markdown-mode evil evil-collection)

  ;;------------------------------
  :general ; evil
  ;;------------------------------
  ;; TODO: is this emacs or evil? ...or both?
  (:prefix  (keybind:prefix :local "p")
   :states  keybind:leader/local:states
   :keymaps 'markdown-mode-map
   ;; `markdown-mode' puts `markdown-preview' as "p".
   ;; Steal "p", but add `markdown-preview' back somewhere else.
   "p" (list #'mantle:cmd:markdown:preview-live/start :which-key "Preview (Live)")
   "P" (list #'markdown-preview                       :which-key "Preview (Static)")
   "s" (list #'mantle:cmd:markdown:preview-live/stop  :which-key "Stop Preview (Live)")))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'config 'user 'dev-env 'languages 'markdown)

;;; mantle/config/dev-env/css.el --- Cascading Style Sheets -*- lexical-binding: t; -*-
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
;;  Cascading Style Sheets
;;
;;; Code:


;;------------------------------------------------------------------------------
;; CSS
;;------------------------------------------------------------------------------

(imp:use-package css-mode ; `scss-mode' also?
  :ensure nil ; This is an Emacs built-in feature.

  ;;------------------------------
  :init
  ;;------------------------------

  (defun mantle:css:comment-indent-new-line (&optional _)
    "Continues the comment in an indented new line.

Meant for `comment-line-break-function' in `css-mode' and `scss-mode'.

From Doom's `+css/comment-indent-new-line' in \"modules/lang/web/autoload/css.el\"."
    (interactive)
    (cond ((or (not (buffer:point:inside-comment?))
               (and comment-use-syntax
                    (not (save-excursion (comment-beginning)))))
           (let (comment-line-break-function)
             (newline-and-indent)))

          ((save-match-data
             (let ((at-end (looking-at-p ".+\\*/"))
                   (indent-char (if indent-tabs-mode ?\t ?\s))
                   (post-indent (save-excursion
                                  (move-to-column (1+ (current-indentation)))
                                  (skip-chars-forward " \t" (line-end-position))))
                   (pre-indent (current-indentation))
                   opener)
               (save-excursion
                 (if comment-use-syntax
                     (goto-char (comment-beginning))
                   (goto-char (line-beginning-position))
                   (when (re-search-forward comment-start-skip (line-end-position) t)
                     (goto-char (or (match-end 1)
                                    (match-beginning 0)))))
                 (buffer-substring-no-properties (point) (line-end-position))
                 (when (looking-at "\\(//\\|/?\\*\\**/?\\)\\(?:[^/]\\)")
                   (list (match-string-no-properties 1)
                         (- (match-beginning 1) (line-beginning-position))))
                 (if (looking-at "\\(//\\|/?\\*\\**/?\\)\\(?:[^/]\\)")
                     (setq opener (match-string-no-properties 1)
                           pre-indent (- (match-beginning 1) (line-beginning-position)))
                   (setq opener ""
                         pre-indent 0)))
               (insert-and-inherit
                "\n" (make-string pre-indent indent-char)
                (if (string-prefix-p "/*" opener)
                    (if (or (eq +web-continue-block-comments t)
                            (string= "/**" opener))
                        " *"
                      "")
                  opener)
                (make-string post-indent indent-char))
               (when at-end
                 (save-excursion
                   (just-one-space)
                   (insert "\n" (make-string pre-indent indent-char)))))))))


  (defun mantle:css:fill/adaptive ()
    "An `adaptive-fill-function' that conjoins SCSS line comments correctly.

From Doom's `+css-adaptive-fill-fn' in \"modules/lang/web/autoload/css.el\"."
    (when (looking-at "[ \t]*/[/*][ \t]*")
      (let ((str (match-string 0)))
        (when (string-match "/[/*]" str)
          (replace-match (if (string= (match-string 0 str) "/*")
                             " *"
                           "//")
                         t t str)))))

  ;; Switch between files with the same basename and different extensions.
  (imp:eval:after projectile
   (dolist (other-files '(("css"  "scss" "sass" "less" "styl")
                          ("scss" "css")
                          ("sass" "css")
                          ("less" "css")
                          ("styl" "css")))
     (unless (seq-contains-p projectile-other-file-alist other-files)
       (push other-files projectile-other-file-alist))))


  (innit:hook:defun
     (:name   'css:settings
      :docstr "Settings for CSS mode. Non-LSP stuff.")
   ;; Correctly continue /* and // comments on newline-and-indent
   (setq comment-line-break-function #'mantle:css:comment-indent-new-line)
   ;; Fix `fill-paragraph' not conjoining line comments in CSS modes correctly.
   (setq adaptive-fill-function #'mantle:css:fill/adaptive)
   ;; Fix filled lines not being auto-prefixed with a * when needed.
   (setq adaptive-fill-first-line-regexp "\\'[ \t]*\\(?:\\* *\\)?\\'")

   ;; Separate camel-case into separate words?
   ;; (subword-mode t)
   )


  ;;------------------------------
  :hook
  ;;------------------------------
  (css-mode-hook . mantle:hook:css:settings)


  ;;------------------------------
  :config
  ;;------------------------------

  ;;---
  ;; SmartParens & CSS-Mode
  ;;---
  (imp:eval:after smartparens
    (defun mantle:user:css:is-auto-close-style-3 (_id action _context)
      (and (eq action 'insert)
           (eq css-mode-auto-close-style 3)))
    (sp-local-pair 'css-mode
                   "<" ">"
                   :unless '(:add mantle:user:css:is-auto-close-style-3))

    ;; let smartparens handle these
    (innit:customize-set-variable css-mode-enable-auto-quoting nil)
    (innit:customize-set-variable css-mode-enable-auto-pairing t)

    ;; 1. Remove css-mode auto pairs whose end pair starts with a latter
    ;;    (truncated autopairs like <?p and hp ?>). Smartparens handles these
    ;;    better.
    ;; 2. Strips out extra closing pairs to prevent redundant characters
    ;;    inserted by smartparens.
    (dolist (alist css-mode-engines-auto-pairs)
      (setcdr alist
              (cl-loop for pair in (cdr alist)
                       unless (string-match-p "^[a-z-]" (cdr pair))
                       collect (cons (car pair)
                                     (string-trim-right (cdr pair)
                                                        "\\(?:>\\|]\\|}\\)+\\'")))))
    (setq css-mode-engines-auto-pairs (delq nil css-mode-engines-auto-pairs)))

  ;;---
  ;; CSS-Mode Config
  ;;---
  (add-to-list 'css-mode-engines-alist '("elixir" . "\\.eex\\'"))
  (add-to-list 'css-mode-engines-alist '("phoenix" . "\\.[lh]eex\\'"))

  ;; Use // instead of /* as the default comment delimited in JS
  (setf (alist-get "javascript" css-mode-comment-formats nil nil #'equal)
        "//"))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'config 'user 'dev-env 'languages 'css)

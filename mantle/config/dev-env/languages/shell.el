;;; mantle/config/dev-env/shell.el --- Bash, Fish, NuShell, PowerShell... -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2023-01-05
;; Timestamp:  2023-06-28
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Bash, Fish, NuShell, PowerShell...
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Shell Mode
;;------------------------------------------------------------------------------

(imp:use-package sh-script
  :ensure nil ; This is an Emacs built-in feature.


  ;;------------------------------
  :mode
  ;;------------------------------
  ("\\.env\\'" . sh-mode)


  ;;------------------------------
  :init
  ;;------------------------------

  (defvar mantle:user:shell:keywords/add
    '("cat" "cd" "chmod" "chown" "cp" "curl" "date" "echo" "find" "git" "grep"
      "kill" "less" "ln" "ls" "make" "mkdir" "mv" "pgrep" "pkill" "pwd" "rm"
      "sleep" "sudo" "touch")
    "A list of common shell commands to be fontified especially in `sh-mode'.")


  (defun mantle:user:shell:match-vars-in-quotes (limit)
    "Search for variables in double-quoted strings bounded by LIMIT.

Originally from Doom's `+sh--match-variables-in-quotes'
in \"modules/lang/sh/autoload.el\"."
    (with-syntax-table sh-mode-syntax-table
      (let (res)
        (while
            (and (setq res
                       (re-search-forward
                        "[^\\]\\(\\$\\)\\({.+?}\\|\\<[a-zA-Z0-9_]+\\|[@*#!]\\)"
                        limit t))
                 (not (eq (nth 3 (syntax-ppss)) ?\"))))
        res)))


  (defun mantle:user:shell:match-command-subst-in-quotes (limit)
    "Search for variables in double-quoted strings bounded by LIMIT.

Originally from Doom's `+sh--match-command-subst-in-quotes'
in \"modules/lang/sh/autoload.el\"."
    (with-syntax-table sh-mode-syntax-table
      (let (res)
        (while
            (and (setq res
                       (re-search-forward "[^\\]\\(\\$(.+?)\\|`.+?`\\)"
                                          limit t))
                 (not (eq (nth 3 (syntax-ppss)) ?\"))))
        res)))


  (innit:hook:defun
     (:name   'shell:keywords/add
      :docstr (mapconcat #'identity
                         '("Add the extra syntax hilighting to the mode."
                          ""
                          "1. Fontify variables in double quotes."
                          "2. Fontify command substitution in double quotes."
                          "3. Fontify built-in/common commands (see `mantle:user:shell:keywords/add').")
                         "\n")
    (font-lock-add-keywords nil ; Add to this buffer's mode.
                            `((mantle:user:shell:match-vars-in-quotes
                               (1 'font-lock-constant-face prepend)
                               (2 'font-lock-variable-name-face prepend))
                              (mantle:user:shell:match-command-subst-in-quotes
                               (1 'sh-quoted-exec prepend))
                              (,(regexp-opt mantle:user:shell:keywords/add 'symbols)
                               (0 'font-lock-type-face append))))))


  (innit:hook:defun
     (:name   'shell:settings
      :docstr "Settings for SHELL mode. Non-LSP stuff.")

   (setq fill-column (jerky:get 'fill-column 'wide)
         tab-width   (jerky:get 'code 'tab 'standard))

   ;; NOTE: If `sh-mode' indention isn't working as desired, try changing this.
   ;; Doom had it as `always'; see its docstr for what that does.
   ;; (setq sh-indent-after-continuation 'always)

   ;; "Shell-script" is a bit long-winded...
   (setq mode-name "sh"))


  ;;------------------------------
  :hook
  ;;------------------------------
  ((sh-mode-hook . mantle:hook:shell:keywords/add)
   (sh-mode-hook . mantle:hook:shell:settings)
   (sh-mode-hook . rainbow-delimiters-mode))


  ;;------------------------------
  :config
  ;;------------------------------

  ;; Recognize function names with dashes in them.
  (add-to-list 'sh-imenu-generic-expression
               '(sh (nil "^\\s-*function\\s-+\\([[:alpha:]_-][[:alnum:]_-]*\\)\\s-*\\(?:()\\)?" 1)
                    (nil "^\\s-*\\([[:alpha:]_-][[:alnum:]_-]*\\)\\s-*()" 1)))

  ;; May have already done this advice in core init? Don't double up.
  (unless (advice-member-p #'innit:advice:squelch #'sh-set-shell)
    (advice-add #'sh-set-shell :around #'innit:advice:squelch))

  ;; Add backtick as a parenthesis type if using smartparens.
  (imp:eval:after smartparens
                  (sp-local-pair 'sh-mode
                                 "`" "`"
                                 :unless '(:add sp-point-before-word-p sp-point-before-same-p))))


;; NOTE: If using Company, add a use-package for `company-shell'?
;;   - See Doom:
;;     - modules/lang/sh/config.el
;;     - https://github.com/doomemacs/doomemacs/blob/e96624926d724aff98e862221422cd7124a99c19/modules/lang/sh/config.el#L78


;;------------------------------------------------------------------------------
;; PowerShell
;;------------------------------------------------------------------------------

(imp:use-package powershell
  ;;------------------------------
  :init
  ;;------------------------------

  (innit:hook:defun
      (:name   'powershell:settings
       :docstr "Settings for PowerShell mode. Non-LSP stuff.")

    (setq fill-column (jerky:get 'fill-column 'wide))
          tab-width   (jerky:get 'code 'tab 'standard))


  ;;------------------------------
  :hook
  ;;------------------------------
  ((powershell-mode-hook . mantle:hook:powershell:settings)
   (powershell-mode-hook . rainbow-delimiters-mode))


  ;;------------------------------
  :custom
  ;;------------------------------

  (powershell-indent (jerky:get 'code 'tab 'standard))
  (powershell-continuation-indent (jerky:get 'code 'tab 'short)))


;;------------------------------------------------------------------------------
;; Fish
;;------------------------------------------------------------------------------
;; NOTE: Not using Fish currently, so no reason to bother with any of this:
;;
;; Briefly tried to use Fish as my Daily Shell. Is a nice shell, with useful
;; features for a Daily Shell... But is not quite useful enough to double up on
;; shells since I still need Bash for the general, day-to-day, work type stuff.
;;
;; ;; https://github.com/wwwjfy/emacs-fish
;; (imp:use-package fish-mode
;;   ;;------------------------------
;;   :init
;;   ;;------------------------------
;;   (innit:hook:defun
;;      (:name   'fish:settings
;;       :docstr "Settings for Fish Shell mode. Non-LSP stuff.")
;;
;;    ;; 'wide' is a decent default, probably?
;;    (setq fill-column (jerky:get 'fill-column 'wide)))
;;
;;
;;   ;;------------------------------
;;   :hook
;;   ;;------------------------------
;;   ((fish-mode-hook  . mantle:hook:fish:settings)
;;    (before-save-hook . fish_indent-before-save) ; Auto-format: Run `fish_indent' before saving.
;;    (fish-mode-hook . rainbow-delimiters-mode))
;;
;;
;;   ;;------------------------------
;;   :custom
;;   ;;------------------------------
;;   (fish-indent-offset (jerky:get 'code 'tab 'standard)))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'config 'user 'dev-env 'languages 'shell)

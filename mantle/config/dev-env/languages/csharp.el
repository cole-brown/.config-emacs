;;; mantle/config/dev-env/languages/csharp.el --- Config the Sharp C. -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2021-09-27
;; Modified:   2022-12-12
;; URL:        https://github.com/cole-brown/.config-emacs
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;  Config the Sharp C.
;;
;;; Code:


(imp:require :keybind)
(imp:require :jerky)


;;------------------------------------------------------------------------------
;; C# Mode
;;------------------------------------------------------------------------------
;; https://github.com/emacs-csharp/csharp-mode
;;
;; Apparently `csharp-mode' is getting pulled into Emacs proper for Emacs 29.
;; Until then, it's in "no longer maintained" mode?
;;   - https://github.com/emacs-csharp/csharp-mode#obsoletion-warning
(when (> emacs-major-version 28)
  (nub:warning
     :innit
     (imp:path:current:file/relative :mantle)
   '(:line:each
     "`csharp-mode' in Emacs 29 _should_ be a built-in mode."
     "TODO: Add `:ensure nil' or something to the `use-package'?"
     "See: https://github.com/emacs-csharp/csharp-mode")))


(imp:use-package csharp-mode
  ;;--------------------
  :init
  ;;--------------------

  (innit:hook:defun
     (:name   'csharp:settings
      :file   macro<imp>:path/file
      :docstr (concat "Settings for C# mode. Non-LSP stuff."
                      "\n"
                      "Note that `csharp-mode' is not derived from `cc-mode' (though it does "
                      "require/use it), so do All The Things!"))

     ;; Use BSD in C, C++. Used to use it in C#.
     ;; But now C# shares a style with Java, so see if that's better.
     ;; Or go back to BSD if I hate it.
     ;; (c-set-style "bsd") ; See c-style-alist for supported types
     ;; Trial [2022-12-12]
     ;; TODO: yea or nay?
     (c-set-style "csharp") ; See c-style-alist for supported types
     (message "TODO: yea or nay?\n  (c-set-style \"csharp\")\nTry \"bsd\" as well? Customize to make work correctest?")


     (setq c-basic-offset (jerky:get 'code 'tab 'normal))
     (c-set-offset 'innamespace 0) ; Don't indent namespace - waste of indent level
     ;; Set by "csharp" style.
     ;; (c-set-offset 'case-label '+) ; indent case labels by c-indent-level, too

     ;; 'wide' is a decent default, probably?
     (setq fill-column (jerky:set 'fill-column 'wide))

     ;; TODO: is this true?
     ;; electric-indent-mode is true and might take care of this?
     ;; (local-set-key [return] 'newline-and-indent)

     ;; §-TODO-§ [2019-10-11]: I probably want most or all of these, or some
     ;;   competing package/feature?
     ;; (paredit-mode 1)
     ;; (my/disable-paredit-spaces-before-paren)
     ;; (company-mode 1)
     ;; (flycheck-mode 1)

     ;; §-TODO-§ [2019-10-11]: check out these ideas from:
     ;;   https://github.com/dholm/dotemacs/blob/master/.emacs.d/lisp/modes/c-c%2B%2B.el
     ;; §-TODO-§ [2019-10-11]: enable/disable in c-common-hook as well.

     ;; Separate camel-case into separate words?
     ;; (subword-mode t)
     )

  ;;--------------------
  :hook
  ;;--------------------
  (csharp-mode-hook . mantle:hook:csharp:settings)

  ;;--------------------
  :config
  ;;--------------------

  )


;;------------------------------------------------------------------------------
;; .NET CLI Transient Menu
;;------------------------------------------------------------------------------
;; https://github.com/sebasmonia/sharper


(imp:use-package sharper
  ;;--------------------
  :general
  ;;--------------------
  (:prefix  (keybind:prefix :local) ;; "compile"?
   :states  keybind:leader/local:states
   :keymaps keybind:leader/local:keymaps

   ;; `sharper-main-transient' is the entrypoint.
   "c" '(sharper-main-transient :which-key "`dotnet' CLI..."))

  ;; TODO: Doom also mapped these; do we want any?
  ;; (map! (:map sharper--solution-management-mode-map
  ;;        :nv "RET" #'sharper-transient-solution
  ;;        :nv "gr" #'sharper--solution-management-refresh)
  ;;       (:map sharper--project-references-mode-map
  ;;        :nv "RET" #'sharper-transient-project-references
  ;;        :nv "gr" #'sharper--project-references-refresh)
  ;;       (:map sharper--project-packages-mode-map
  ;;        :nv "RET" #'sharper-transient-project-packages
  ;;        :nv "gr" #'sharper--project-packages-refresh)
  ;;       (:map sharper--nuget-results-mode-map
  ;;        :nv "RET" #'sharper--nuget-search-install))
  )




;;------------------------------------------------------------------------------
;; `.csproj' Mode
;;------------------------------------------------------------------------------
;; https://github.com/omajid/csproj-mode

;; Nothing to do... super simple mode.
(imp:use-package csproj-mode

  )


;;------------------------------------------------------------------------------
;; `.sln' Mode
;;------------------------------------------------------------------------------
;; https://github.com/sensorflo/sln-mode

(imp:use-package! sln-mode
  ;; Not on a package repository so get it from GitHub:
  :straight (:type git
             :host github
             :repo "sensorflo/sln-mode")

  ;; This is all the set-up that Doom does:
  :mode "\\.sln\\'")


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :dot-emacs 'config 'code '+c-sharp)

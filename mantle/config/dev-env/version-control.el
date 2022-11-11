;;; mantle/config/version-control.el --- Git et al -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2022-07-21
;; Modified:   2022-07-21
;; URL:        https://github.com/cole-brown/.config-emacs
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;  Git et al
;;
;;; Code:


(imp:require :keybind)


;;------------------------------------------------------------------------------
;; Magit & Friends
;;------------------------------------------------------------------------------
;; The best Git Porcelain:
;;   https://magit.vc/
;;   https://github.com/magit/magit
;;
;; Plus some other version control things.
;; Whatever.
;; The important thing is: Magit.


;;------------------------------------------------------------------------------
;; Magit: Git Front-End (Porcelain)
;;------------------------------------------------------------------------------
;; The best Git Porcelain.
;;   https://magit.vc/
;;   https://github.com/magit/magit

(imp:use-package magit

  ;;--------------------
  :init
  ;;--------------------

  (defun mantle:user:magit:buffer:kill ()
    "Kill all magit buffers."
    (interactive)
    (message "[%s] Kill 'magit' buffers..."
             (datetime:string/get 'rfc-3339 'datetime))
    (buffer:kill:matching ".*magit.*"
                          :internal
                          :modified
                          :process))


  ;;--------------------
  :general
  ;;--------------------
  ;; Put Magit Leader under the Global Leader
  ;;---
  (:prefix  (keybind:prefix :global "g")
   :states  keybind:leader/global:states
   :keymaps keybind:leader/global:keymaps
   ;; Title
   "" '(nil :which-key "Magit / Version Control")

   ;;---
   ;; Magit Keybinds
   ;;---
   "g" (list #'magit-status                           :which-key "Status")
   "q" (list #'mantle:user:deadgrep:default-directory :which-key "Kill All 'deadgrep' Buffers")

   ;; TODO: More keybinds!
   ;; TODO: And use `:repeat' / `:jump'?
   ;;   https://github.com/noctuid/general.el#evil-command-properties
   ;;   https://github.com/noctuid/evil-guide#command-properties
   ;;
   ;; (general-define-key
   ;;  :keymaps 'normal
   ;;  :prefix "SPC"
   ;;  "gj" '(git-gutter:next-hunk :properties (:repeat t :jump t))
   ;;  "gk" '(git-gutter:previous-hunk :repeat t :jump t))
   ;;
   ;; ;; they also work globally
   ;; (general-define-key
   ;;  :keymaps 'normal
   ;;  :prefix "SPC"
   ;;  :properties '(:repeat t :jump t)
   ;;  ;; or
   ;;  :repeat t
   ;;  :jump t
   ;;  "gj" 'git-gutter:next-hunk
   ;;  "gk" 'git-gutter:previous-hunk)
   ))


;;------------------------------
;; Magit Forge (GitHub, et al)
;;------------------------------
;; "Work with Git forges, such as Github and Gitlab, from the comfort of Magit
;; and the rest of Emacs."
;; https://github.com/magit/forge
;; https://magit.vc/manual/forge/Loading-Forge.html#Loading-Forge

(imp:use-package forge
  :after magit)


;;------------------------------
;; Magit Todos
;;------------------------------
;; https://github.com/alphapapa/magit-todos

;;---
;; External Tool Prereqs:
;;---
;; > One of the following external scanners is required:
;; >   - 'ripgrep'
;; >   - 'git grep' (built with PCRE support)
;; >   - 'GNU grep' (built with PCRE support)
;; >
;; > Most Linux systems should have the latter two by default, but some
;; > non-standard systems may not. For example, on MacOS you may use Homebrew to
;; > install ripgrep, or git with PCRE support, like: brew reinstall --with-pcre2
;; > git.
;;
;; I have installed 'ripgrep', so we're triple good on Linux.
;;---

;; Lodge a complaint if 'ripgrep' isn't installed on the system. But don't skip
;; the `use-package', since it can use 'git grep' or 'GNU grep'.
(unless (executable-find "rg")
  (nub:warning
      :innit
      (imp:path:join (imp:path:current:dir/relative :mantle)
                     (imp:path:current:file))
    '("Could not find 'ripgrep' (`rg') executable. Is it installed? "
      "`magit-todos' wants it.")))


(imp:use-package magit-todos
  :after magit

  ;;--------------------
  :general
  ;;--------------------
  (:prefix  (keybind:prefix :global "g")
   :states  keybind:leader/global:states
   :keymaps keybind:leader/global:keymaps

   ;;---
   ;; Magit-Todos Keybinds
   ;;---
   "t" '(magit-todos-list :which-key "Magit TODOs list buffer"))


  ;;--------------------
  :config
  ;;--------------------
  (magit-todos-mode +1))


;;------------------------------------------------------------------------------
;; Git Gutter
;;------------------------------------------------------------------------------
;; https://github.com/emacsorphanage/git-gutter-fringe

(imp:use-package git-gutter-fringe
  ;; NOTE: `git-gutter-fringe' does not work in the terminal. Use `git-gutter' if
  ;; needed there.
  :when (display-graphic-p)

  ;;--------------------
  :config
  ;;--------------------
  (global-git-gutter-mode +1))


;;------------------------------------------------------------------------------
;; Git File Modes
;;------------------------------------------------------------------------------

;; .gitignore, .gitattributes, and .gitconfig
(imp:use-package git-modes
  :defer t

  ;;--------------------
  :config
  ;;--------------------

  ;; Docker's ignore file is basically the same format as .gitignore, so use the
  ;; `gitignore-mode' for it.
  (add-to-list 'auto-mode-alist
             (cons "/.dockerignore\\'" 'gitignore-mode)))


;;------------------------------------------------------------------------------
;; Autogit
;;------------------------------------------------------------------------------

;; TODO: uncomment autogit use-package.
;; Autogit is configured in secrets repo and it looks something like this:
;;    (imp:use-package autogit
;;    ;; This is my own package, so...
;;    ;;   1) Don't try to install.
;;    :ensure nil
;;    ;;   2) Here's where it is; add this dir to the `load-path'.
;;    :load-path (path:join innit:path:packages:user "autogit")
;;
;;    ;;--------------------
;;    :custom
;;    ;;--------------------
;;
;;    ;; Repos: Autocommit:
;;    (autogit:repos:path/commit (list
;;                            ;; Org-Mode Files.
;;                            "/path/to/dir/org"
;;
;;                            ;; Any other just-auto-commit-'em repos?
;;                            ;;   - Personal docs dirs.
;;                            ;;   - "Backup" git repos.
;;                            ;;   - etc.
;;                            ))
;;
;;    ;; Repos: Gather Status:
;;    (autogit:repos:path/watch (list
;;                            ;;------------------------------
;;                            ;; Auto-commit repos to also watch:
;;                            ;;------------------------------
;;                            ;; Org-Mode Files.
;;                            "/path/to/dir/org"
;;
;;                            ;;------------------------------
;;                            ;; Only watch repos:
;;                            ;;------------------------------
;;
;;                            ;;---
;;                            ;; Personal
;;                            ;;---
;;                            ;; Your .emacs repo?
;;                            (path:abs:dir "~" ".config" "emacs")
;;                            ;; or...
;;                            (path:abs:dir "~" ".emacs.d")
;;
;;                            ;;---
;;                            ;; Work: all repos in directory
;;                            ;;--
;;                            ;; Delay until after 'autogit' is loaded so we can
;;                            ;; use `autogit:repos:list' to load all repos in a
;;                            ;; directory.
;;                            )))
;;
;;    ;; Can't use `autogit:repos:list' to set up `autogit:repos:path/watch' until
;;    ;; after it's loaded, so... use it after 'autogit' is loaded.
;;    (imp:eval:after autogit
;;    ;; Add more repos to `autogit:repos:path/watch' list.
;;    (customize-set-variable 'autogit:repos:path/watch
;;                            (append autogit:repos:path/watch ;; We want to add, not replace, so append to it.
;;                                    ;;---
;;                                    ;; Work: all repos in directory
;;                                    ;;---
;;                                    (autogit:repos:list "~/path/to/repositories"))))


;;------------------------------
;; Autogit Requirement: Deferred
;;------------------------------
;; This is in Autogit's package requirements, so I'm not sure why it isn't loaded.
;;   - Is it because `:ensure' is nil for autogit so no dependencies are ensured
;;     either?
;;   - Is it because my first package (`autogit') has something wrong with it?
;;   - Something else?
;;   - None of the above?
;;   - All of the above?
;;
;; ...Just make sure it's installed...

(imp:use-package deferred)


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'config 'user 'dev-env 'version-control)

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


(require 'cl-lib)

(imp:require :alist 'generic)


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
;; Keybinds: General Infixes
;;------------------------------------------------------------------------------
;; Define the infix with the title here so we don't have to worry about what's
;; defined in what order since you can only define the title once or else things
;; overwrite each other?
;;
;; TODO: Make sure that's a correct assumption. Currently only 87% sure.

(keybind:leader/global:def
 :infix  "g"
 "" '(nil :which-key "Magit / Version Control"))


;;------------------------------------------------------------------------------
;; Magit: Git Front-End (Porcelain)
;;------------------------------------------------------------------------------
;; The best Git Porcelain.
;;   https://magit.vc/
;;   https://github.com/magit/magit

(imp:use-package magit

  ;;------------------------------
  :init
  ;;------------------------------

  (defun mantle:user:magit:buffer:kill ()
    "Kill all magit buffers."
    (interactive)
    (message "[%s] Kill all 'magit' buffers..."
             (datetime:string/get 'rfc-3339 'datetime))
    (buffer:kill:matching ".*magit.*"
                          :internal
                          :modified
                          :process)))


;;------------------------------
;; Magit Keybinds: Meow
;;------------------------------

(imp:use-package magit
  :when  (imp:flag? :keybinds +meow)
  :after meow

  ;;------------------------------
  :config
  ;;------------------------------

  (transient-define-prefix mantle:meow/transient:dev-env:version-control ()
    "Notes commands like org-mode links, org-journal entries, etc."
    ["Version Control..."
     ["Git"
      ("d" "magit: status" magit)
      ("K" "magit: Kill all `magit' buffers" mantle:user:magit:buffer:kill)]])
  ;; (mantle:meow/transient:notes)

  ;; TODO: Make a `mantle:meow/transient:dev-env' transient for dev-env stuff in general?
  (meow-normal-define-key '("d" . mantle:meow/transient:dev-env:version-control)))


;;------------------------------
;; Magit Keybinds: evil
;;------------------------------

(imp:use-package magit
  :when  (imp:flag? :keybinds +evil)
  :after (:and evil evil-collection)

  ;;------------------------------
  :general ; evil
  ;;------------------------------
  ;; Put Magit Leader under the Global Leader
  ;;---
  (keybind:leader/global:def
   :infix  "g"

   ;;---
   ;; Magit Keybinds
   ;;---
   "g" (list #'magit-status                  :which-key "Status")
   "q" (list #'mantle:user:magit:buffer:kill :which-key "Kill All 'magit' Buffers")

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

  ;;------------------------------
  :custom
  ;;------------------------------

  ;; Global glob patterns to ignore:
  (magit-todos-exclude-globs '(".git/"
                               ;; Web: Ignore source maps and minified Javascript & CSS.
                               "*.js.map"
                               "*.css.map"
                               "*.min.js"
                               "*.min.css"
                               ))
  ;; https://github.com/alphapapa/magit-todos/#tips
  ;; They suggest to use `magit-todos-exclude-globs' as a dir local var for
  ;; per-repository settings, but let's ignore some globally too.


  ;;------------------------------
  :config
  ;;------------------------------
  (magit-todos-mode +1))


;;------------------------------
;; Magit-Todos Keybinds: meow
;;------------------------------

(imp:use-package magit-todos
  :when  (imp:flag? :keybinds +meow)
  :after (:and magit meow)

  ;;------------------------------
  :bind ; meow
  ;;------------------------------
  (:map mantle:meow/keymap/global:version-control
   ("t" . magit-todos-list)))


;;------------------------------
;; Magit-Todos Keybinds: evil
;;------------------------------

(imp:use-package magit-todos
  :when  (imp:flag? :keybinds +evil)
  :after (:and magit evil evil-collection)

  ;;------------------------------
  :general ; evil
  ;;------------------------------
  (keybind:leader/global:def
   :infix  "g"
   ;;---
   ;; Magit-Todos Keybinds
   ;;---
   "t" '(magit-todos-list :which-key "Magit TODOs list buffer")))


;;------------------------------------------------------------------------------
;; Git Gutter
;;------------------------------------------------------------------------------
;; https://github.com/emacsorphanage/git-gutter-fringe

(imp:use-package git-gutter-fringe
  ;; NOTE: `git-gutter-fringe' does not work in the terminal. Use `git-gutter' if
  ;; needed there.
  :when (display-graphic-p)

  ;;------------------------------
  :config
  ;;------------------------------
  (global-git-gutter-mode +1))


;;------------------------------
;; Magit Keybinds: Meow
;;------------------------------

(imp:use-package git-gutter-fringe
  :when  (imp:flag? :keybinds +meow)
  :after meow

  ;;------------------------------
  :config
  ;;------------------------------

  (transient-append-suffix 'mantle:meow/transient:dev-env:version-control
    '(0 -1) ; Append after last group/suffix in the first group.
     ["Git Hunks"
      ("." "Hunk: Previous" git-gutter:previous-hunk)
      ("e" "Hunk: Next"     git-gutter:next-hunk)])
  ;; (mantle:meow/transient:dev-env:version-control)
 )


;;------------------------------------------------------------------------------
;; Git File Modes
;;------------------------------------------------------------------------------

;; .gitignore, .gitattributes, and .gitconfig
(imp:use-package git-modes
  :defer t

  ;;------------------------------
  :config
  ;;------------------------------

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
;;    ;;------------------------------
;;    :custom
;;    ;;------------------------------
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
;;    (innit:customize-set-variable autogit:repos:path/watch
;;                                  (append autogit:repos:path/watch ;; We want to add, not replace, so append to it.
;;                                          ;;---
;;                                          ;; Work: all repos in directory
;;                                          ;;---
;;                                          (autogit:repos:list "~/path/to/repositories"))))


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


;; TODO-meow: more keybinds?
;; ;;------------------------------------------------------------------------------
;; ;; Keybinds
;; ;;------------------------------------------------------------------------------
;;
;; ;; A lot of the evil-collection binds get nuked by my binds, so remake them?
;; (imp:eval:after (:and magit evil-collection-magit)
;;   ;;------------------------------
;;   ;; Keybind Definer
;;   ;;------------------------------
;;   ;; Create a general keybind definer...
;;   (general-create-definer keybind:leader/magit:def
;;     ;; TODO: shorter or no prefix?
;;     ;; TODO: like... idk... "g" is already taken by a lot of stuff but it's also our magit menu in all modes?
;;     :prefix  (keybind:prefix :local)
;;     ;; :states  evil-collection-magit-state
;;     ;; :keymaps magit-mode-map-or-something ; keybind:leader/local:keymaps
;;     )
;;
;;   ;;------------------------------
;;   ;; Keybinds
;;   ;;------------------------------
;;
;;  (let (binds:evil/general:by-keymap-state
;;        set/keymaps)
;;
;;    ;; Translate from `evil-collection' to `general'.
;;    (dolist (binding evil-collection-magit-mode-map-bindings)
;;      ;; Binding will be e.g.:
;;      ;;   ((normal visual) magit-mode-map "g")
;;      ;;   ((normal visual) magit-mode-map "\C-j" magit-section-forward "n")
;;      (let* ((bind/state (nth 0 binding))
;;             (bind/map   (nth 1 binding))
;;             (bind/key   (nth 2 binding))
;;             (bind/func  (nth 3 binding))
;;             (alist/key  (cons bind/map bind/state))
;;             (binds  (alist:generic:get/value alist/key binds:evil/general:by-keymap-state :op/equal)))
;;        (cl-pushnew bind/map set/keymaps)
;;        (push bind/key binds)
;;        (push `(function ,bind/func) binds)
;;        (alist:generic:update alist/key binds binds:evil/general:by-keymap-state :op/equal)))
;;
;;    ;; Create in `general'.
;;    (dolist (map/binds binds:evil/general:by-keymap-state)
;;      (let ((map   (caar map/binds))
;;            (state (cdar map/binds))
;;            (binds (nreverse (cdr map/binds))))
;;
;;        ;; Give it its title?
;;        (when (memq map set/keymaps)
;;          (eval
;;           `(keybind:leader/magit:def
;;              :states  ',state
;;              :keymaps ',map
;;              "" '(nil :which-key "Magit...")))
;;          (setq set/keymaps (remove map set/keymaps)))
;;
;;        ;; Make the bindings.
;;        (eval
;;         `(keybind:leader/magit:def
;;            :states  ',state
;;            :keymaps ',map
;;            ,@binds)))))
;;   )


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'config 'user 'dev-env 'version-control)

;;; package.el --- Now we're the Postal Service -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2022-04-26
;; Modified:   2022-04-26
;; URL:        https://github.com/cole-brown/.config-emacs
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;  Now we're the Postal Service
;;
;;; Code:


(require 'package)
(require 'url)

(imp:require :innit 'debug)
(imp:require :innit 'vars)
(imp:require :nub)


;;------------------------------------------------------------------------------
;; Package Archives
;;------------------------------------------------------------------------------

;; Save common package archives so we don't have to go scrounging them up again.
(defvar innit:package:archives:common
  '(;; Milkypostman's Emacs Lisp Package Archive
    (:melpa . ("melpa" . "https://melpa.org/packages/"))
    ;; For more & newer Org-Mode packages.
    (:org . ("org"   . "https://orgmode.org/elpa/"))

    ;;---
    ;; The Default
    ;;---
    ;; Emacs Lisp Package Archive
    (:default . ("gnu"   . "https://elpa.gnu.org/packages/")))
  "Alist of Package Archive keyword to cons cells  for `package-archives' alist.")


(defcustom innit:package:archives:enabled
  '(:melpa :org :default)
  "Ordered list of package archives to enable in `innit:package:init'."
  :group 'innit:group
  :type  '(restricted-sexp :match-alternatives (keywordp)))


;;------------------------------------------------------------------------------
;; Package Path
;;------------------------------------------------------------------------------
;; Modify `package' path: want a top-level dir that can hold the "elpa"
;; `package' dir, any local packages, `straight.el' repos, etc...

;; TODO: Here or move to a 'no-littering' file?
(defcustom innit:path:var (path:join user-emacs-directory "var")
  "Directory for persistent data files."
  :group 'innit:group
  :type  'string)


;; TODO: Here or move to a 'no-littering' file?
(defcustom innit:path:etc (path:join user-emacs-directory "config")
  "Directory for persistent config files."
  :group 'innit:group
  :type  'string)


(defcustom innit:path:packages (path:join user-emacs-directory "packages")
  "Top-level directory for packages. ELPA, others will have subdirectories."
  :group 'innit:group
  :type  'string)


(defcustom innit:path:packages:elpa (path:join innit:path:packages "elpa")
  "Directory for ELPA/packages.el packages."
  :group 'innit:group
  :type  'string)


(defcustom innit:path:packages:straight (path:join innit:path:packages "straight")
  "Directory for `straight.el' packages.

`straight-base-dir' is actually \"dir in which `straight' will make its
'straight' base dir\"... so... just use `innit:path:packages' for that."
  :group 'innit:group
  :type 'string)


(defcustom innit:path:packages:gpg (path:join innit:path:var "packages" "gpg")
  "Directory for gpg keys for e.g. `package-gnupghome-dir'."
  :group 'innit:group
  :type  'string)


(defcustom innit:path:packages:user (path:join innit:path:packages "user")
  "Directory for user's personal/custom packages."
  :group 'innit:group
  :type  'string)


;;------------------------------------------------------------------------------
;; Straight
;;------------------------------------------------------------------------------

(defcustom innit:version-control:required
  '("git"    ; Git
    ;; "hg"     ; Mercurial
    ;; "bzr"    ; Bazaar
    ;; "fossil" ; Fossil
    ;; "darcs"  ; Darcs
    ;; "pijul"  ; Pijul
    )
  "All executables listed are checked for during `innit:package:init/standard'."
  :group 'innit:group
  :type  '(repeat string))


(defcustom innit:straight:use-package-default nil
  "Should `straight' be the default used by `use-package' to obtain packages?"
  :group 'innit:group
  :type  '(choice (const t)
                  (const nil)))


;;------------------------------------------------------------------------------
;; Package Early-Init
;;------------------------------------------------------------------------------

(defun innit:package:archive (keyword-or-cons)
  "Return a package name/URL cons.

If KEYWORD-OR-CONS is a keyword, returns value from
`innit:package:archives:common'.

If KEYWORD-OR-CONS is a string/string cons, returns it as-is.

Else error."
  (cond ((keywordp keyword-or-cons)
         (alist-get keyword-or-cons innit:package:archives:common))
        ((and (consp keyword-or-cons)
              (stringp (car keyword-or-cons))
              (stringp (cdr keyword-or-cons)))
         keyword-or-cons)
        (t
         (error "innit:package:archive: Expected keyword or cons, got: %S"
                keyword-or-cons))))


(defun innit:package:init/paths ()
  "Set 'package.el' paths.

Some (`package-user-dir') get overwritten when 'package.el' loads, so this must
be called after 'package.el' loads.

Can be called earlier too, if you want..."
  ;;---
  ;; Set path packages are saved to.
  ;;---
  (when innit:path:packages:elpa
    ;; `package-user-dir' doesn't get set via `customize-set-variable'?!
    ;;   (innit:customize-set-variable package-user-dir innit:path:packages:elpa)
    ;; So use `setq' instead:
    (setq package-user-dir innit:path:packages:elpa))
  ;; NOTE: Also, this gets wiped out when 'package.el' is loaded? So... Set it
  ;; as much as needed to force Emacs to behave?

  (when innit:path:packages:gpg
    (innit:customize-set-variable package-gnupghome-dir innit:path:packages:gpg)))


(defun innit:package:init/early ()
  "Prepare Emacs 'package.el' for the `package-initialize' step.

`package-initialize' is called between \"early-init.el\" and \"init.el\", so
this needs to be called during \"early-init.el\"."
  (nub:debug
      :innit
      "innit:package:init/early"
      '(:innit :package :early)
    "'package.el' early init...")

  ;;---
  ;; Set enabled package archives.
  ;;---
  (when innit:package:archives:enabled
    (innit:customize-set-variable package-archives
                                  (mapcar #'innit:package:archive
                                          innit:package:archives:enabled)))

  ;;---
  ;; Set path packages are saved to.
  ;;---
  (innit:package:init/paths))


(defun innit:package:init/standard ()
  "Initialize Emacs 'package.el' after the `package-initialize' step.

1. Download package metadata if needed.
2. Install `use-package' if needed.
3. Require `use-package'.

`package-initialize' is called between \"early-init.el\" and \"init.el\", so
this needs to be called during \"init.el\"."
  (let ((func/name "innit:package:init/standard")
        (func/tags '(:innit :package :standard)))
    (nub:debug
        :innit
        func/name
        func/tags
      "'package.el' standard init...")

    ;;------------------------------
    ;; `package.el'
    ;;------------------------------
    ;; Set 'package.el' paths again as 'package.el' overwrites them when it loads...
    (innit:package:init/paths)

    ;; Update packages list if we are on a new install.
    (unless (or (package-installed-p 'use-package)
                package-archive-contents)
      (nub:info
          :innit
          func/name
        "Update packages list...")
      (package-refresh-contents))

    ;;------------------------------
    ;; `straight'
    ;;------------------------------
    ;; https://github.com/radian-software/straight.el#getting-started
    ;; https://github.com/radian-software/straight.el#the-recipe-format

    ;; Make sure we can even get `straight' to function; it needs `git' et al
    ;; already installed on the system.
    (dolist (exe innit:version-control:required)
      (unless (executable-find exe)
        (nub:error
           :innit
           func/name
         "Required version control system executable `%s' cannot be found by Emacs!"
         exe)))

    ;;---
    ;; Pre-Bootstrap
    ;;---
    ;; These must happen _before_ the bootstrap code in order to affect `straight'.

    ;; NOTE: Give these all docstrings as we're defining them before they would be defined otherwise.

    (innit:customize-set-variable straight-base-dir innit:path:packages
                                  '("straight's main directory, containing it's build files and package repos"
                                    ""
                                    "Directory in which the \"straight/\" subdirectory is created. (default: `user-emacs-directory')"
                                    "So just use `innit:path:packages' instead of `innit:path:packages:straight'"))

    (innit:customize-set-variable straight-build-dir (format "build-%s" emacs-version)
                                  '("The directory in which `straight' packages are built."
                                    ""
                                    "It is located at \"`straight-base-dir'/build\". Changing this variable will"
                                    "change the name of that directory and the name of the build cache file (unless"
                                    "`straight-build-cache-fixed-name' is non-nil)."
                                    ""
                                    "Since byte-code is rarely compatible across different versions of"
                                    "Emacs, it's best we build them in separate directories, per emacs"
                                    "version."))

    (innit:customize-set-variable straight-check-for-modifications nil
                                  '("When to check for package modifications."
                                    ""
                                    "Make start-up a few seconds faster by skipping the modification checks, so..."
                                    "Don't modify any packages in place and it won't be needed."))

    (innit:customize-set-variable straight-vc-git-default-clone-depth '(1 single-branch)
                                  '("The default value for `:depth' when `:type' is the symbol `git'."
                                    ""
                                    "Don't need full history for package repos, so save some time/bandwidth/space by"
                                    "shallow cloning the repos."
                                    ""
                                    "NOTE: Some packages may break when shallow cloned?! Doom mentions `magit' and"
                                    "`org', so don't use `straight' for those maybe?"))

    (innit:customize-set-variable straight-use-package-by-default innit:straight:use-package-default
                                  '("Non-nil means install packages by default in `use-package' forms."
                                    ""
                                    "If you use `use-package', then this makes each `use-package' form invoke"
                                    "`straight' to install the package, unless otherwise specified"
                                    "(via `:stright nil')."))

    (innit:customize-set-variable straight-enable-package-integration t
                                  '("Whether to enable \"integration\" with package.el."
                                    ""
                                    "This means that `package-enable-at-startup' is disabled, and"
                                    "advices are put on `package--ensure-init-file' and"
                                    "`package--save-selected-packages' to prevent package.el from"
                                    "modifying the init-file."))

    ;; Alist mapping forge `:host' symbols to username strings. Used to compute
    ;; the repo URL when the `:fork' keyword is used in a recipe.
    ;;   https://github.com/radian-software/straight.el#but-what-about-my-fork-of-obscure-el-package
    (innit:customize-set-variable straight-host-usernames '((github . "cole-brown")
                                                            ;; (gitlab . "cole-brown")
                                                            ;; (codeberg . "cole-brown")
                                                            ;; (sourcehut . "cole-brown")
                                                            ;; (bitbucket . "cole-brown")
                                                            ))

    ;;---
    ;; Bootstrap Code (copy/pasted from README's "Getting Started")
    ;;---
    (defvar bootstrap-version) ;; TODO: Does this have to be def'd as a non-namespaced variable?
    (let ((bootstrap-file (path:join innit:path:packages:straight "repos/straight.el/bootstrap.el"))
          (bootstrap-version 6))
      (unless (file-exists-p bootstrap-file)
        (with-current-buffer
            (url-retrieve-synchronously
             "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
             'silent 'inhibit-cookies)
          (goto-char (point-max))
          (eval-print-last-sexp)))
      (load bootstrap-file nil 'nomessage))

    ;;------------------------------
    ;; `use-package'
    ;;------------------------------
    ;;---
    ;; Install:
    ;;---
    (unless (package-installed-p 'use-package)
      (nub:debug
          :innit
          func/name
          func/tags
        "Install `use-package'...")
      (package-install 'use-package))

    (require 'use-package)

    ;;---
    ;; Global Settings:
    ;;---
    ;; Automatically install package if not found.
    ;;   https://github.com/jwiegley/use-package#package-installation
    ;; NOTE: Does not keep anything up-to-date. For that you would use package
    ;; `auto-package-update' or something similar.
    (innit:customize-set-variable use-package-always-ensure t)

    ;; 'When using :hook omit the "-hook" suffix if you specify the hook
    ;; explicitly, as this is appended by default.
    ;;
    ;; [...]
    ;;
    ;; If you do not like this behaviour, set use-package-hook-name-suffix to
    ;; nil. By default the value of this variable is "-hook".'
    ;;   - https://github.com/jwiegley/use-package#hooks
    ;;
    ;; Need to override this to allow naming hooks something other than
    ;; `*-hook'.
    (innit:customize-set-variable use-package-hook-name-suffix nil)

    ;;---
    ;; Debugging Settings:
    ;;---
    (setq use-package-compute-statistics    innit:debug?
          use-package-verbose               innit:debug?
          use-package-minimum-reported-time (if innit:debug? 0 0.1)
          use-package-expand-minimally      innit:interactive?)

    ;;---
    ;; `auto-minor-mode' for `use-package' keyword `:minor'
    ;;---
    ;; https://github.com/joewreschnig/auto-minor-mode
    (imp:use-package auto-minor-mode)))


;;------------------------------------------------------------------------------
;; Package Upgrade
;;------------------------------------------------------------------------------

;;------------------------------
;; NOTE: Update/Upgrade Packages Process
;;------------------------------
;;
;; If you get an error like this on installing a new package:
;;   Error (use-package): Failed to install magit: http://melpa.org/packages/magit-20190113.1949.tar: Not found
;;   Error (use-package): Cannot load magit
;; Or this:
;;   Debugger entered--Lisp error: (file-error "http://melpa.org/packages/markdown-mode-20190305.319.el" "Not found")
;; Just evalulate this again: (package-refresh-contents)

;;------------------------------
;; NOTE: Manual Upgrade Process
;;------------------------------
;;   M-x list-packages
;;     1. 'U' to mark upgrades
;;     2. 'x' to upgrade?
;; After it's all done, maybe run:
;;   M-x package-autoremove
;;     - May need `M-x list-packages' first if custom-file doesn't exist right now.


;;------------------------------
;; Package Upgrade
;;------------------------------

(defcustom innit:package:upgrade:pretty-print-packages 10
  "Pretty print packages-to-upgrade list if greater than or equal to this amount.

`innit:cmd:package:upgrade' will pretty print the info buffer, and truncate the
prompt."
  :group 'innit:group
  :type  '(natnum)) ; non-negative integer


(defun int<innit>:package:version (package-name package-description-alist)
  "Get version for PACKAGE-NAME from PACKAGE-DESCRIPTION-ALIST.

Return nil if PACKAGE-NAME is not in PACKAGE-DESCRIPTION-ALIST."
  (let ((package-desc (cadr (assq package-name package-description-alist))))
    (when package-desc
      (package-desc-version package-desc))))


;; TODO [2020-03-11]:
;;   - Instead of a max number, just do X at a time with some
;;     (yasnippet-snippets) on their own, because they can cause too many files
;;     to open and that'll fail the package upgrade.

;; Less manual. Upgrade all packages without showing *Packages* buffer.
;; https://emacs.stackexchange.com/questions/16398/noninteractively-upgrade-all-packages
(defun innit:cmd:package:upgrade (&optional max)
  "Upgrade all packages automatically without showing the '*Packages*' buffer.

First updates package metadata, then upgrades all packages that have newer
versions.

Will only upgrade the first MAX if MAX is numberp or can be converted by
`string-to-number', and number to upgrade is greater than MAX."
  (interactive "P")

  ;; Interactive code "P" (raw prefix argument) gives us nil or a list with the
  ;; prefix in it. So if we get the list, just use the first element as the MAX.
  (when (listp max)
    (setq max (nth 0 max)))

  (let* ((func/name "innit:package:upgrade")
         ; (func/tags '(:innit :package))
         (packages/max (cond ((null max)
                              nil)
                             ((and (stringp max)
                                   (string= "" max))
                              nil)
                             ((stringp max)
                              (string-to-number max))
                             ((numberp max)
                              max)
                             (t
                              (nub:error
                                 :innit
                                 func/name
                               "Cannot convert MAX to number: %S"
                               max))))
         ;; Sanity:
         ;;   - 0 lurking around as a valid string-to-number for "IDK" is annoying.
         ;;   - Negative or 0 from weird user inputs should be handled...
         ;;   - Also a good spot to convert 'nil' to 'I don't care'.
         (packages/max (if (or (null packages/max)
                               (< packages/max 1))
                      most-positive-fixnum
                    packages/max))
         upgrades/all
         upgrades/subset
         length/all
         length/subset)

    ;;------------------------------
    ;; Error Checks
    ;;------------------------------
    (when (and (not (null max))
               (null packages/max))
      (nub:error
         :innit
         func/name
       "Don't know what to do with intended MAX number: %S"
       max))

    ;;------------------------------
    ;; Package Metadata
    ;;------------------------------
    (nub:info
        :innit
        func/name
      "Refreshing package metadata...")

    ;; Update the package metadata from the package repos.
    (package-refresh-contents)

    ;;------------------------------
    ;; Find Packages to Upgrades
    ;;------------------------------
    (nub:info
        :innit
        func/name
      "Checking for packages to upgrade...")

    (dolist (package (mapcar #'car package-alist))
      (let ((in-archive (int<innit>:package:version package package-archive-contents)))
        (when (and in-archive
                   (version-list-< (int<innit>:package:version package package-alist)
                                   in-archive))
          (push (cadr (assq package package-archive-contents))
                upgrades/all))))
    (setq length/all (length upgrades/all))

    ;;------------------------------
    ;; Show Packages to Upgrades
    ;;------------------------------
    (if (null upgrades/all)
        ;;---
        ;; Nothing - done!
        ;;---
        (nub:info
           :innit
           func/name
         "All packages are up to date.")

      ;;---
      ;; Have upgrades - now we need to whittle down to `packages/max' if necessary.
      ;;---
      (if (and (not (null packages/max))
               (> (length upgrades/all) packages/max))
          ;; Chop upgrades down to the maximum allowed right now.
          (setq upgrades/subset (seq-take upgrades/all packages/max))
        ;; just take 'em all
        (setq upgrades/subset upgrades/all))
      (setq length/subset (length upgrades/subset))

      ;;------------------------------
      ;; Update Packages
      ;;------------------------------
      (let ((packages/prompt (format "Upgrade %s package%s (%s)? "
                                     (if (> length/all length/subset)
                                         (format "%d/%d" length/subset length/all)
                                       (format "%d" length/subset))
                                     (if (= length/subset 1) "" "s")
                                     (mapconcat #'package-desc-full-name
                                                upgrades/subset ", "))))
        ;;  If more than a few packages, format for innit info buffer differently.
        (if (< length/subset innit:package:upgrade:pretty-print-packages)
            ;; One-line version:
            (nub:info
               :innit
               func/name
             packages/prompt)

          ;; One-per-line version:
          (nub:info
             :innit
             func/name
           "Upgrade %s packages?"
           (if (> length/all length/subset)
               (format "%d/%d" length/subset length/all)
             (format "%d" length/subset)))
          (let ((digits/subset (1+ (floor (log10 length/subset))))) ; Number of digits to format for display.
            (dotimes (i length/subset)
              (nub:info
                 :innit
                 func/name
               (message (concat "  " ; list indent
                                "%"  ; "%0" if you want "002" instead of "  2".
                                (number-to-string digits/subset)
                                "d. %s")
                        i
                        (package-desc-full-name (nth i upgrades/subset)))))))

        ;;---
        ;; Update packages or cancel?
        ;;---
        (if (yes-or-no-p packages/prompt)
            ;;---
            ;; Update
            ;;---
            (progn
              (nub:info
                 :innit
                 func/name
               "Updating packages...")
              (save-window-excursion
                (dolist (package-desc upgrades/subset)
                  (let ((old-package (cadr (assq (package-desc-name
                                                  package-desc)
                                                 package-alist))))
                    (nub:info
                       :innit
                       func/name
                     "  Updating `%s'..."
                     (package-desc-full-name package-desc))
                    (package-install package-desc)

                    (nub:info
                       :innit
                       func/name
                     "  Cleaning `%s'..."
                     (package-desc-full-name package-desc))
                    (package-delete old-package)))))

          ;;---
          ;; Cancel
          ;;---
          (nub:info
             :innit
             func/name
           "Package update declined."))))))

;; http://nhoffman.github.io/.emacs.d/#orgf46780c
;; Some useful ELPA variables and functions:
;;   M-x package-list-packages  - open list of packages
;;   package-activated-list     - variable containing list of the names of currently activated packages
;;   package-install            - install a package
;;   package-installed-p        - true if package is installed


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :innit 'package)

;;; mantle/config/project.el --- Project Stuff like Projectile -*- lexical-binding: t; -*-
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2022-11-21
;; Modified:   2022-11-21
;; URL:        https://github.com/cole-brown/.config-emacs
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;  Project Stuff like Projectile
;;
;;; Code:


(imp:require :innit)


;;------------------------------------------------------------------------------
;; Projectile
;;------------------------------------------------------------------------------
;; https://github.com/bbatsov/projectile
;; https://melpa.org/#/projectile

(imp:use-package projectile

  ;; TODO: keybinds!
  ;; TODO: Doom made it lazy/auto-load based on these commands. Do I want that or to let use-package do whatever it does normally?
  ;; ;;--------------------
  ;; :commands
  ;; ;;--------------------
  ;;
  ;; (projectile-project-root
  ;;  projectile-project-name
  ;;  projectile-project-p
  ;;  projectile-locate-dominating-file
  ;;  projectile-relevant-known-projects)


  ;;--------------------
  :init
  ;;--------------------

  (defun mantle:user:projectile:ignore? (project-root)
    "Return non-nil if temporary file or a `no-littering' directory."
    ;; If a remote file, don't ignore.
    (unless (file-remote-p project-root)
      ;; Non-remote file; ignore if:
      (or
      ;; - a temp file
       (file-in-directory-p project-root temporary-file-directory)
       ;; - in one of the `no-littering' dirs
       (and (bound-and-true-p no-littering-etc-directory)
            (file-in-directory-p project-root no-littering-etc-directory))
       (and (bound-and-true-p no-littering-var-directory)
            (file-in-directory-p project-root no-littering-var-directory)))))


  ;;--------------------
  :custom
  ;;--------------------

  ;; `no-littering' puts these into "`user-emacs-directory'/var/..." for us.
  ;;   - `projectile-cache-file'
  ;;   - `projectile-known-projects-file'

  (projectile-enable-caching innit:interactive?)

  ;; Auto-discovery is slow to do by default. Better to update the list
  ;; when you need to (`projectile-discover-projects-in-search-path').
  (projectile-auto-discover nil)

  ;; Always ignore:
  (projectile-globally-ignored-files         '(".DS_Store" "TAGS"))
  (projectile-globally-ignored-file-suffixes '(".elc" ".pyc" ".o"))

  ;; Options:
  ;;   - `kill-all':        kill each buffer
  ;;   - `kill-only-files': kill only the buffer associated to a file
  ;;   - a predicate:       Otherwise, it should be a predicate that takes one argument: the buffer to be killed.
  ;; NOTE [2022-11-21]: Doom set this to `kill-only-files', but... Try default of `kill-all'?
  ;; (projectile-kill-buffers-filter 'kill-only-files)

  (projectile-ignored-projects '("~/"))
  (projectile-ignored-project-function #'mantle:user:projectile:ignore?)

  ;; The original `projectile-default-mode-line' can be expensive over
  ;; TRAMP, so we gimp it in remote buffers.
  (projectile-mode-line-function (lambda ()
                                   (if (file-remote-p default-directory) ""
                                     (projectile-default-mode-line))))

  ;; Per-project compilation buffers
  (compilation-buffer-name-function   #'projectile-compilation-buffer-name)
  (compilation-save-buffers-predicate #'projectile-current-project-buffer-p)


  ;; ;;--------------------
  ;; :general ;; TODO: Or maybe put in `:config'? Or both?
  ;; ;;--------------------

  ;; TODO: keybinds!
  ;; ;; Recommended keymap prefix on Windows/Linux
  ;; ;; (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

  ;; (global-set-key [remap evil-jump-to-tag] #'projectile-find-tag)
  ;; (global-set-key [remap find-tag]         #'projectile-find-tag)


  ;;--------------------
  :config
  ;;--------------------

  (innit:hook:defun-and-add
      projectile-relevant-known-projects
      (:name    "projectile:known-projects"
       :file    macro<imp>:path/file
       :docstr  (concat "Auto-discovery on `projectile-mode' is slow and premature. "
                        "Let's defer it until it's actually needed. "
                        "Also clean up non-existing projects too!")
       ;; :squelch t
       :quiet   t)
    (projectile-cleanup-known-projects)
    (projectile-discover-projects-in-search-path))

  ;; Projectile runs four functions to determine the root (in this order):
  ;;  - `projectile-root-local' -> checks the `projectile-project-root' variable
  ;;     for an explicit path.
  ;;  - `projectile-root-bottom-up' -> searches from / to your current directory
  ;;    for the paths listed in `projectile-project-root-files-bottom-up'. This
  ;;    includes .git and .project
  ;;  - `projectile-root-top-down' -> searches from the current directory down to
  ;;    / the paths listed in `projectile-root-files', like package.json,
  ;;    setup.py, or Cargo.toml
  ;;  - `projectile-root-top-down-recurring' -> searches from the current
  ;;    directory down to / for a directory that has one of
  ;;    `projectile-project-root-files-top-down-recurring' but doesn't have a
  ;;    parent directory with the same file.
  ;;
  ;; In the interest of performance, we reduce the number of project root marker
  ;; files/directories projectile searches for when resolving the project root.
  (setq projectile-project-root-files-bottom-up
        (append
         ;;---
         ;; Always:
         ;;---
         '(".projectile"  ; projectile's root marker
           ".project"     ; doom project marker
           ".git")        ; Git VCS root dir
         ;;---
         ;; If Installed
         ;;---
         (when (executable-find "hg") ; Mercurial VCS dir(s), if installed.
           '(".hg"))      ; Mercurial VCS root dir
         (when (executable-find "bzr") ; Bazaar VCS dir(s), if installed
           '(".bzr"))     ; Bazaar VCS root dir
         (when (executable-find "fossil") ; Fossil VCS dir(s), if installed
           '(".fslckout"  ; Fossil VCS root dir
             "_FOSSIL_")) ; Fossil VCS root DB on Windows
         (when (executable-find "darcs") ; Darcs VCS dir(s), if installed
           '("_darcs"))   ; Darcs VCS root dir
         (when (executable-find "pijul") ; Pijul VCS dir(s), if installed
           '(".pijul")))) ; Pijul VCS root dir

  ;; TODO: If slow, implement this from Doom (see "core-projects.el" or `rg' the var):
  ;; ;; This will be filled by other modules. We build this list manually so
  ;; ;; projectile doesn't perform so many file checks every time it resolves
  ;; ;; a project's root -- particularly when a file has no project.
  ;; (setq projectile-project-root-files '())

  ;; I don't need SVN or CVS. Those are hopefully ancient history.
  (setq projectile-project-root-files-top-down-recurring
        (remove "CVS"
                (remove ".svn" projectile-project-root-files-top-down-recurring)))

  ;; Enable!
  (projectile-mode +1))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'config 'user 'project)

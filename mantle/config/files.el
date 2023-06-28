;;; mantle/config/files.el --- File & Directory Settings -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2022-07-29
;; Timestamp:  2023-06-28
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;  File & Directory Settings
;;
;;; Code:


(imp:require :datetime)
(imp:require :buffer)


;;------------------------------------------------------------------------------
;; recentf for recent files
;;------------------------------------------------------------------------------

;; http://pages.sachachua.com/.emacs.d/Sacha.html#org0676afd
(imp:use-package recentf
  :demand t

  ;; TODO: Do I want this functionality from Doom or no? It has errors, currently, if not '--debug-init'.
  ;; ;;------------------------------
  ;; :init
  ;; ;;------------------------------
  ;;
  ;; (defun mantle:recentf:file/truename (file)
  ;;   "Proudly nicked from Doom's 'core/core-editor.el'."
  ;;   (if (or (not (file-remote-p file))
  ;;           (equal "sudo" (file-remote-p file 'method)))
  ;;       (abbreviate-file-name (file-truename (tramp-file-name-localname tfile)))
  ;;     file))
  ;;
  ;; ;; REVIEW: Use this in lieu of `mantle:recentf:file/truename' when we drop
  ;; ;;   Emacs 28 support. See emacs-mirror/emacs@32906819addd.
  ;; ;; (setq recentf-show-abbreviated t)
  ;;
  ;; (innit:hook:defun
  ;;     (:name    "recentf:touch/buffer"
  ;;      :docstr  "Bump file in recent file list when it is switched or written to."
  ;;      :squelch t)
  ;;   (when buffer-file-name
  ;;     (recentf-add-file buffer-file-name))
  ;;   ;; Return nil for `write-file-functions'
  ;;   nil)
  ;;
  ;; (innit:hook:defun
  ;;     (:name    "recentf:touch/dired"
  ;;      :docstr  "Add dired directories to recentf file list."
  ;;      :squelch t)
  ;;   (recentf-add-file default-directory))
  ;;
  ;;
  ;; ;;------------------------------
  ;; :hook
  ;; ;;------------------------------
  ;;
  ;; (((window-selection-change-functions
  ;;    write-file-functions)
  ;;   . mantle:hook:recentf:touch/buffer) ;; (innit:hook:func/name:symbol "recentf:touch/buffer" nil)
  ;;  (dired-mode-hook . mantle:hook:recentf:touch/dired)) ;; (innit:hook:func/name:symbol "recentf:touch/dired" nil)


  ;;------------------------------
  :custom
  ;;------------------------------

  ;; Clean up the recent list when Emacs has been idle for over 30 seconds.
  (recentf-auto-cleanup    30)

  ;; Default is 20. Doom sets to 200 and that was occasionally too low.
  (recentf-max-saved-items 1000)

  ;; How many saved items to /show/.
  (recentf-max-menu-items  20)


  ;;------------------------------
  :config
  ;;------------------------------

  ;; Resolve symlinks, strip out the /sudo:X@ prefix in local tramp paths, and
  ;; abbreviate $HOME -> ~ in filepaths (more portable, more readable, & saves
  ;; space)
  (add-to-list 'recentf-filename-handlers #'mantle:recentf:file/truename)

  ;; Text properties inflate the size of recentf's files, and there is
  ;; no purpose in persisting them.
  ;; NOTE: Must be first in the list!
  (add-to-list 'recentf-filename-handlers #'substring-no-properties)

  ;; Periodically save the list of recent files: https://www.emacswiki.org/emacs/RecentFiles#toc1
  ;; Otherwise they're only saved during a graceful shutdown.
  (run-with-timer (* 30 60) ;; Wait 30 mins to run.
                  (* 30 60) ;; Repeat every 30 mins.
                  'recentf-save-list)

  ;; Recentf and TRAMP need some peace-keeping to get along.
  ;; https://lists.gnu.org/archive/html/bug-gnu-emacs/2007-07/msg00019.html
  (add-to-list 'recentf-keep 'file-remote-p)

  ;; Don't want the minibuffer to always say
  ;;   "Cleaning up the recentf list...done (NN removed)"
  ;; when Emacs has been idle a while and `recentf' has run the auto-cleanup.
  ;;
  ;; NOTE: `recentf-auto-cleanup' just sets up a timer to call
  ;; `recentf-cleanup', which I do want to output messages when called
  ;; interactively... so only squelch if it's _not_ called interactively.
  (define-advice recentf-cleanup (:around (fn &rest args) mantle:user:squelch)
    "`recentf-auto-cleanup' should not allow `recentf-cleanup' to be chatty."
    (innit:squelch/unless :interactive? t
                          (apply fn args)))

  ;; Don't want a bunch of `load-file' messages when `recentf-load-list' runs.
  (define-advice recentf-load-list (:around (fn &rest args) mantle:user:squelch)
    "Don't want a bunch of `load-file' messages when `recentf-load-list' runs."
    (innit:squelch/unless :interactive? t
                          (apply fn args)))

  ;; Excluded Files/Dirs:
  ;;---
  ;; NOTE: `no-littering' has the set-up for adding its dirs to `recentf-exclude'.
  ;;   See:    core/boot/10-init/00-bootstrap.el
  ;;   Search: recentf-exclude
  ;;---
  ;;
  ;; No others to exclude that I know of, currently.
  ;; (add-to-list 'recentf-exclude <path>)

  ;; Enable!
  (recentf-mode +1))


;;------------------------------------------------------------------------------
;; Search: Deadgrep (uses Ripgrep)
;;------------------------------------------------------------------------------

;;------------------------------
;; ripgrep
;;------------------------------
;; https://github.com/BurntSushi/ripgrep
;;
;; This needs installed on the computer separately from Emacs.
;;
;;   "ripgrep is a line-oriented search tool that recursively searches your
;; current directory for a regex pattern. By default, ripgrep will respect your
;; .gitignore and automatically skip hidden files/directories and binary files.
;; ripgrep has first class support on Windows, macOS and Linux, with binary
;; downloads available for every release. ripgrep is similar to other popular
;; search tools like The Silver Searcher, ack and grep."


;;------------------------------
;; deadgrep
;;------------------------------
;; https://github.com/Wilfred/deadgrep
;; "Deadgrep is the fast, beautiful text search that your Emacs deserves."
(imp:use-package deadgrep
  :demand t

  ;;------------------------------
  :init
  ;;------------------------------

  (defun mantle:user:deadgrep:default-directory (search-term)
    "Search for SEARCH-TERM with `deadgrep' at `default-directory'."
    (interactive (list (deadgrep--read-search-term)))
    (call-interactively #'deadgrep
                        search-term
                        default-directory))


  (defun mantle:user:deadgrep:buffer:kill ()
    "Kill all deadgrep buffers."
    (interactive)
    (message "[%s] Kill 'deadgrep' buffers..."
             (datetime:string/get 'rfc-3339 'datetime))
    (buffer:kill:matching ".*deadgrep.*"
                              :internal
                              :modified
                              :process))

  ;; ;;------------------------------
  ;; :config
  ;; ;;------------------------------
  ;;
  ;; ;;---
  ;; ;; Project Root Overrides
  ;; ;;---
  ;; ;; TODO: Search per-comp configs for `deadgrep-project-root' to find what's set?
  ;; ;;   - `deadgrep-project-root-overrides'
  ;; ;;   - `deadgrep-project-root-function'
  )


;;------------------------------
;; Keybinds : Meow
;;------------------------------

(imp:use-package deadgrep
  :when  (imp:flag? :keybinds +meow)
  :after meow

  ;;------------------------------
  :bind ; meow
  ;;------------------------------

  (:map deadgrep-mode-map ; TODO: Rebind (more) keybinds from this map?
   ("k" . window:kill-or-quit) ; 'kill-or-quit' instead of 'quit-or-kill'
   ("/" . deadgrep-restart)    ; "↺ Refresh" / rerun search
   ;; Normal Enter/Return is "visit in other window".
   ("RET"   . deadgrep-visit-result-other-window)
   ;; BUG?: "S-RET" doesn't bind to "shift-enter"?! Why not? What does?
   ;; > ("S-RET"   . deadgrep-visit-result)
   ;;   - "S-<return>" works.
   ;;   - "C-S-m" might works too?
   ;;   - "C-S-j" might work too?
   ;; But none of them are exactly the same return as the 'logical key' "RET"...
   ;; ...I don't think?
   ("S-<return>" . deadgrep-visit-result))


  ;;------------------------------
  :config
  ;;------------------------------

  ;;------------------------------
  ;; `General'
  ;;------------------------------

  (defun mantle:meow/keybind/general:search ()
    "Create the \"Search...\" keybinds in `general' for `meow'."
    (keybind:leader/global:def
      :infix (keybind:infix "/")      ; search
      "" '(nil :which-key "Search...")) ; infix title

    ;;---
    ;; Global Keybinds
    ;;---
    (keybind:global:def
      :infix "/"
      "/" (list #'deadgrep                                  :which-key "`rg' @ project root")
      "." (list #'mantle:user:deadgrep:default-directory    :which-key "`rg' @ default-directory")
      ;; TODO: A deadgrep search that lets me choose the starting dir?
      ;; "?" (list #'mantle:user:deadgrep:default-directory :which-key "`rg' @...")

      "k" (list #'mantle:user:deadgrep:buffer:kill          :which-key "Kill All 'deadgrep' Buffers")

      "s" (list #'isearch-forward                           :which-key "Search Forward")
      "r" (list #'isearch-backward                          :which-key "Search Backward")))


  ;;------------------------------
  ;; `Transient'
  ;;------------------------------

  (defun mantle:meow/keybind/transient:search ()
    "Create the \"Search...\" keybinds in `transient' for `meow'."
    ;;---
    ;; Global Keybinds
    ;;---

    (transient-define-prefix mantle:meow/transient:search ()
      "Search commands that should be available globally."
      ["Search..."
       ["Project:"
        ("/" "`rg' @ project root"         deadgrep)
        ("." "`rg' @ default-directory"    mantle:user:deadgrep:default-directory)
        ;; TODO-meow: A deadgrep search that lets me choose the starting dir?
        ;; ("?" "`rg' @ ..."                  mantle:user:deadgrep:default-directory)
        ("k" "Kill All 'deadgrep' Buffers" mantle:user:deadgrep:buffer:kill)

        ("?" "`consult-ripgrep'" consult-ripgrep)]

       ["Buffer:"
        ("s" "Search Forward"  isearch-forward)
        ("r" "Search Backward" isearch-backward)]])

    ;;---
    ;; Entrypoint
    ;;---

    (meow-normal-define-key
     '("/" . mantle:meow/transient:search))) ; :which-key "Search..."


  ;;------------------------------
  ;; Actually Create Keybinds:
  ;;------------------------------

  (if (imp:provided? :keybinds 'user 'general 'meow)
      (mantle:meow/keybind/general:search)
    (mantle:meow/keybind/transient:search)))


;;------------------------------
;; Keybinds : Evil
;;------------------------------

(imp:use-package deadgrep
  :when  (imp:flag? :keybinds +evil)
  :after (:and evil evil-collection)

  ;;------------------------------
  :general ; evil
  ;;------------------------------

  ;;---
  ;; Global Keybinds
  ;;---
  (keybind:global:def
    :states  'normal
    :infix "/"
    ;; Title
    "" '(nil :which-key "Search...")

    "/" (list #'deadgrep                               :which-key "`rg' @ project root")
    "." (list #'mantle:user:deadgrep:default-directory :which-key "`rg' @ default-directory")
    ;; TODO: A deadgrep search that lets me choose the starting dir?
    ;; "?" (list #'mantle:user:deadgrep:default-directory :which-key "`rg' @...")

    "q" (list #'mantle:user:deadgrep:buffer:kill       :which-key "Kill All 'deadgrep' Buffers"))

  ;;---
  ;; `deadgrep-mode-map' Keybinds
  ;;---
  (:states  'normal
   :keymaps 'deadgrep-mode-map ; TODO: Rebind (more) keybinds from this map!

   ;; 'kill-or-quit' instead of 'quit-or-kill'
   "q" (list #'window:kill-or-quit :which-key "Quit or Kill 'deadgrep' Window")

   ;; Swap these two around.
   "RET"   (list #'deadgrep-visit-result-other-window :which-key "Visit Result")
   "S-RET" (list #'deadgrep-visit-result              :which-key "Visit Result (w/ This Window)")

   ;; Bind for EEEVIL!
   "g r" (list #'deadgrep-restart :which-key "↺ Refresh")))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'config 'user 'files)

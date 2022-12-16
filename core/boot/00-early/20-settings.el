;;; 20-init.el --- Early Init -*- lexical-binding: t; -*-
;;
;; Author: Cole Brown <code@brown.dev>
;; URL:    https://github.com/cole-brown/.config-emacs
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;; Code:


;;------------------------------------------------------------------------------
;; Fast, innit?
;;------------------------------------------------------------------------------

;; Always set up our Emacs start-up timing benchmark.
(innit:time:init)


;;==============================================================================
;;                       ------------------------------
;;                                   -----
;;                           The Speeeeeeeeeeed!!!
;;                                   -----
;;                       ------------------------------
;;==============================================================================

;; A lot of these are about speeding up start-up a bit.


;;------------------------------------------------------------------------------
;; Mute Emacs Info Stuff
;;------------------------------------------------------------------------------

;; Reduce *Message* noise at startup. An empty scratch buffer (or whatever)
;; is more than enough.
(setq inhibit-startup-screen            t                ; Don't make/display the "*GNU Emacs*" buffer.
      inhibit-startup-echo-area-message user-login-name) ; See var docstring for why user login name.


;;------------------------------------------------------------------------------
;; Interactive vs Non-interactive
;;------------------------------------------------------------------------------

;; TODO: Safe to do this or not? We're not pre-compiling everything like Doom is.
;;
;; ;; In noninteractive sessions, prioritize non-byte-compiled source files to
;; ;; prevent the use of stale byte-code. Otherwise, it saves us a little IO time
;; ;; to skip the mtime checks on every *.elc file.
;; (setq load-prefer-newer noninteractive)

;;------------------------------
;; Interactive && NOT Debugging
;;------------------------------
;; Run these unless Emacs is in:
;;   - daemon (service) mode
;;   - noninteractive (batch/script) mode
;;   - Any sort of debug mode.
(when (innit:optimize?)
  ;; Save & empty `file-name-handler-alist' to speed up opening files (e.g. for
  ;; loading elisp) during start-up. Reverts by merging original
  ;; `file-name-handler-alist' with any additions to the empty list that
  ;; happened during start-up.
  (innit:optimize:file-name-handler-alist:inhibit)


  ;; Speed up start-up a bit by inhibiting redrawing display, writing messages.
  (innit:optimize:display:inhibit)
  (innit:optimize:load-message:inhibit)


  ;; Disable warnings from legacy advice system. They aren't useful, and what can
  ;; we do about them, besides changing packages upstream?
  (setq ad-redefinition-action 'accept)


  ;; There can also be a "default.el" init file, found via the standard search
  ;; path for libraries. Generally used for site-lisp, it seems? I've never had
  ;; one, so don't bother looking in every `load-path' directory for it - just
  ;; skip it.
  (setq inhibit-default-init t)


  ;; Shave seconds off startup time by starting the scratch buffer in
  ;; `fundamental-mode', rather than, say, `org-mode' or `text-mode', which
  ;; pull in a ton of packages.
  (setq initial-major-mode 'fundamental-mode
        initial-scratch-message nil)
  ;; TODO: Add a hook here for first visit of the scratch buffer or something
  ;; (idle timer?). I want to put it into `emacs-lisp' mode eventually.
  )


;;------------------------------
;; NOT a Daemon Start-Up
;;------------------------------
(unless (daemonp)
  ;; Get rid of "For information about GNU Emacs..." message at startup, unless
  ;; we're in a daemon session where it'll say "Starting Emacs daemon." instead,
  ;; which isn't so bad.
  (advice-add #'display-startup-echo-area-message :override #'ignore)


  ;; HACK `tty-run-terminal-initialization' is *tremendously* slow for some
  ;;      reason; inexplicably doubling startup time for terminal Emacs. Keeping
  ;;      it disabled will have nasty side-effects, so we simply delay it instead,
  ;;      and invoke it later, at which point it runs quickly; how mysterious!
  (advice-add #'tty-run-terminal-initialization :override #'ignore)
  (defun innit:hook:tty-run-terminal-initialization ()
    "Remove the `tty-run-terminal-initialization' override advice.

Advice just ignores calls to the fuction to speed up terminal Emacs init.

Remove the advice, allowing original func to run, and then run original func."
    (advice-remove #'tty-run-terminal-initialization #'ignore)
    (tty-run-terminal-initialization (selected-frame) nil t))
  (add-hook 'window-setup-hook #'innit:hook:tty-run-terminal-initialization))


;;------------------------------------------------------------------------------
;; Display
;;------------------------------------------------------------------------------

;; Emacs "updates" its ui more often than it needs to, so slow it down slightly
(setq idle-update-delay 1.0)  ; default is 0.5


;;------------------------------------------------------------------------------
;; Processes
;;------------------------------------------------------------------------------

;; Increase how much is read from processes in a single chunk .
;; This is further increased elsewhere, where needed (like our LSP module).
;; TODO: Think that's a Doom comment so, do I need to increase that when I do LSP or...
(setq read-process-output-max (max read-process-output-max ; default:  4kb
                                   (* 64 1024)))           ; minimum: 64kb


;;------------------------------------------------------------------------------
;; Security
;;------------------------------------------------------------------------------

;;------------------------------
;; `authinfo'
;;------------------------------

;; Emacs defaults to storing `authinfo' in $HOME and in plain-text by default.
;; This file stores usernames, passwords, and other such treasures for the
;; aspiring malicious third party.
;;
;; NOTE: EPA/EPG needs to be set up for Emacs to be able to automatically open
;; auth sources with filenames ending in ".gpg".
(innit:customize-set-variable auth-sources (list (path:join innit:path:etc "authinfo.gpg")
                                            "~/.config/authinfo.gpg"
                                            "~/.authinfo.gpg"))
;; Default is:
;;   auth-sources
;;     -> ("~/.authinfo" "~/.authinfo.gpg" "~/.netrc")


;;------------------------------------------------------------------------------
;; Files
;;------------------------------------------------------------------------------

;; Don't want to rely on case insensitivity for file names; the
;; `auto-mode-alist' entry should be fixed if needed instead.
(setq auto-mode-case-fold nil)


;; Don't ping things that look like domain names.
(setq ffap-machine-p-known 'reject)


;;------------------------------------------------------------------------------
;; Text
;;------------------------------------------------------------------------------

;;------------------------------
;; UTF-8
;;------------------------------

;; Contrary to what many Emacs users have in their configs, you don't need
;; more than this to make UTF-8 the default coding system:
(set-language-environment "UTF-8")

;; set-language-enviornment sets default-input-method, which is unwanted
(setq default-input-method nil)


;;------------------------------
;; Font
;;------------------------------

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we halve startup times, particularly when we use
;; fonts that are larger than the system default (which would resize the frame).
(setq frame-inhibit-implied-resize t)

;; Font compacting can be terribly expensive, especially for rendering icon
;; fonts on Windows. Whether disabling it has a notable affect on Linux and Mac
;; hasn't been determined, but do it there anyway, just in case. This increases
;; memory usage, however!
(setq inhibit-compacting-font-caches t)


;;------------------------------
;; Bidirectional Text
;;------------------------------
;; Disable bidirectional text scanning for a modest performance boost. I've set
;; this to `nil' in the past, but the `bidi-display-reordering's docs say that
;; is an undefined state and suggest this to be just as good:
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)

;; Disabling the Bidirectional Parenthesis Algorithm makes redisplay faster, but might produce incorrect display
;; reordering of bidirectional text with embedded parentheses and other bracket
;; characters whose 'paired-bracket' Unicode property is non-nil.
(setq bidi-inhibit-bpa t)  ; Emacs 27+


;;------------------------------------------------------------------------------
;; Cursor
;;------------------------------------------------------------------------------

;; Reduce rendering/line scan work for Emacs by not rendering cursors or regions
;; in non-focused windows.
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)


;;------------------------------------------------------------------------------
;; Scrolling
;;------------------------------------------------------------------------------

;; ;; More performant rapid scrolling over unfontified regions. May cause brief
;; ;; spells of inaccurate syntax highlighting right after scrolling, which should
;; ;; quickly self-correct.
;; (setq fast-but-imprecise-scrolling t)
;; TODO: Set or no? Does not setting help some modes that are having trouble (e.g. csharp-mode)?


;; Introduced in Emacs HEAD (b2f8c9f), this inhibits fontification while
;; receiving input, which should help a little with scrolling performance.
;; See help for the variable for more info.
(setq redisplay-skip-fontification-on-input t)


;;------------------------------------------------------------------------------
;; [OS]: Windows
;;------------------------------------------------------------------------------
;; Windows is... special.

;;------------------------------
;; Is Windows?
;;------------------------------

;; Generally, use `innit:os:windows?'... but in this case we have a specific
;; variable to check the existance of.
;;
;; Performance on Windows is considerably worse than elsewhere. We'll need
;; everything we can get.
(when (boundp 'w32-get-true-file-attributes)
  (setq w32-get-true-file-attributes nil   ; decrease file IO workload
        w32-pipe-read-delay 0              ; faster IPC
        w32-pipe-buffer-size (* 64 1024))  ; read more at a time (was 4K)

  ;; The clipboard on Windows could be in another encoding (likely utf-16), so
  ;; let Emacs/the OS decide what to use there.
  (setq selection-coding-system 'utf-8))


;;------------------------------------------------------------------------------
;; [OS]: Linux
;;------------------------------------------------------------------------------

;;------------------------------
;; _-NOT-_ Linux?
;;------------------------------

;; Remove command line options that aren't relevant to our current OS; means
;; slightly less to process at startup.
(unless innit:os:linux?
  (setq command-line-x-option-alist nil))


;;------------------------------------------------------------------------------
;; [OS]: MacOS
;;------------------------------------------------------------------------------

;;------------------------------
;; _-NOT-_ MacOS?
;;------------------------------

;; Remove command line options that aren't relevant to our current OS; means
;; slightly less to process at startup.
(unless innit:os:mac?
  (setq command-line-ns-option-alist nil))


;;------------------------------------------------------------------------------
;; The End
;;------------------------------------------------------------------------------
(imp:provide :core 'boot 'early 'settings)

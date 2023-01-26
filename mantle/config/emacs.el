;;; mantle/config/emacs.el --- Configure Emacs -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2023-01-24
;; Modified:   2023-01-24
;; URL:        https://github.com/cole-brown/.config-emacs
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Emacs Settings and Configuration
;;
;; For Emacs settings that have to be in place super early, see:
;;   $(git rev-parse --show-toplevel)/settings.el
;;
;;; Code:


(imp:require :datetime 'format)
(imp:require :buffer 'type)
(imp:provide :innit 'vars)


;;------------------------------------------------------------------------------
;; Emacs Frame Settings
;;------------------------------------------------------------------------------

(imp:use-package emacs
  :ensure nil ; This is an Emacs built-in feature.

  ;;------------------------------
  :custom
  ;;------------------------------

  ;;------------------------------
  ;; Large Monitors / Window Splitting
  ;;------------------------------

  ;; For `split-window-sensibly'.
  ;; To split "vertically" (as in a horizontal split so the new and current are
  ;; now stacked vertically), must have at least this many lines visible RIGHT
  ;; NOW (not after split). Or set to nil to disable.
  ;;
  ;; Annoying right now since my work monitors aren't big enough to bother getting
  ;; used to more than 2 side-by-sides.
  (split-height-threshold nil)


  ;; For `split-window-sensibly'.
  ;; To split "horizontally" (as in a vertical split so the new and current are
  ;; now stacked side-by-side), must have at least this many columns visible RIGHT
  ;; NOW (not after split). Or set to nil to disable.
  ;;
  ;; Annoying right now since my work monitors aren't big enough to bother getting
  ;; used to more than 2 side-by-sides. 160 is wide enough that it /should/ work
  ;; for most situations.
  (split-width-threshold 160)

  ;; ;;------------------------------
  ;; ;; Files / Buffers
  ;; ;;------------------------------
  ;;
  ;; ;; Don't prompt for confirmation when we create a new file or buffer (assume the
  ;; ;; user knows what they're doing).
  ;; ;; Default (`after-completion') is to confirm only after auto-completion.
  ;; (innit:customize-set-variable confirm-nonexistent-file-or-buffer nil)

  ;;------------------------------
  ;; Bell
  ;;------------------------------

  ;; Visual-Only Bell
  (ring-bell-function nil)
  (visible-bell       t)

  ;; ;; Standard Bell that goes "Donk!"
  ;; (ring-bell-function nil)
  ;; (visible-bell       nil)

  ;; ;; Disable Bell
  ;; (ring-bell-function #'ignore)
  ;; (visible-bell       nil)

  ;;------------------------------
  ;; Frame Title / Icon
  ;;------------------------------

  ;; This variable has the same structure as `mode-line-format', except that
  ;; the %c, %C, and %l constructs are ignored.  It is used only on frames for
  ;; which no explicit name has been set (see `modify-frame-parameters').
  (frame-title-format '("｢%b｣" ; buffer name
                        " @" ; system/host name
                        (:eval (or (file-remote-p default-directory 'host) system-name))
                        " — Emacs " ; And I guess we should say what we are...
                        emacs-version))

  ;; This is for the Emacs icon thingy in the OS app list/taskbar.
  ;; Just have it be the same as the `frame-title-format'.
  (icon-title-format frame-title-format)

  ;;------------------------------
  ;; Window Border / Divider
  ;;------------------------------

  ;; The native border "consumes" a pixel of the fringe on righter-most splits,
  ;; `window-divider' does not. Available since Emacs 25.1.
  (window-divider-default-places t)
  (window-divider-default-bottom-width 1)
  (window-divider-default-right-width 1)

  ;;------------------------------
  ;; Dialogs & Tooltips
  ;;------------------------------

  ;; Disable system's dialog boxes for mouse commands; have Emacs prompt instead.
  ;; More consistent interaction paradigm? Forces you back to the keyboard? Etc?
  (use-dialog-box nil)

  ;; NOTE: Doom guarded with equivalent of this, but do we actually need to?:
  ;;   (when innit:os:linux?
  (x-gtk-use-system-tooltips nil)


  ;;------------------------------
  :config
  ;;------------------------------

  ; Window Border / Divider
  (window-divider-mode +1))


;;--------------------------------------------------------------------------------
;; Misc Emacs Config
;;--------------------------------------------------------------------------------

(imp:use-package emacs
  :ensure nil ; This is an Emacs built-in feature.

  ;;------------------------------
  :custom
  ;;------------------------------

  ;;------------------------------
  ;; File Time Formats
  ;;------------------------------

  ;; Set time format for e.g. dired to a better format:
  ;;   RFC-3339 (human-readable ISO-8601)
  ;; Original value:
  ;;   '(\"%d.%m.%Y %H:%M\" \"%d.%m.%Y %H:%M\"))
  (ls-lisp-format-time-list (list
                             ;; Recent Time Format:
                             (datetime:format/get 'rfc-3339 'datetime)
                             ;; Not-Recent Time Format:
                             (datetime:format/get 'rfc-3339 'datetime)))

  ;; Force use of `ls-lisp-format-time-list' regardless of locale.
  (ls-lisp-use-localized-time-format t)

  ;;------------------------------
  ;; Cursor / Point
  ;;------------------------------

  (blink-cursor-interval 0.75) ; default is 0.5 seconds
  (blink-matching-paren  nil)

  ;; Don't stretch the cursor to fit wide characters; it is disorienting,
  ;; especially for tabs.
  (x-stretch-cursor nil)

  ;; middle-click paste at point, not at click
  (mouse-yank-at-point t)

  ;;------------------------------
  ;; Lines
  ;;------------------------------

  ;;This determines the style of line numbers in effect. If set to `nil', line
  ;; numbers are disabled. For relative line numbers, set this to `relative'.
  (display-line-numbers-type t)

  ;;------------------------------
  ;; Scrolling
  ;;------------------------------

  ;; Smooth horizontal scrolling for long lines?
  (hscroll-margin 2)
  (hscroll-step   1)

  ;; Emacs spends too much effort recentering the screen if you scroll the
  ;; cursor more than N lines past window edges (where N is the settings of
  ;; `scroll-conservatively'). This is especially slow in larger files
  ;; during large-scale scrolling commands. If kept over 100, the window is
  ;; never automatically recentered.
  (scroll-conservatively 101)
  (scroll-margin         0)

  ;; Keep the point on the screen when scrolling.
  (scroll-preserve-screen-position t)

  ;; Reduce cursor lag by a tiny bit by not auto-adjusting `window-vscroll'
  ;; for tall lines.
  (auto-window-vscroll nil)

  ;; Mouse Wheel Scroll
  (mouse-wheel-scroll-amount '(2                     ; Scroll 2 lines normally.
                               ((shift) . hscroll))) ; Scroll horizontally when shift is held.
  (mouse-wheel-scroll-amount-horizontal 2)           ; Scroll 2 columns when hscrolling.

  ;;------------------------------
  ;; Fringe
  ;;------------------------------

  ;; TODO: Do the buffer boundaries interfere with `git-gutter', `flycheck', etc?
  (indicate-buffer-boundaries nil)

  ;; Just puts a bunch of "~" in the fringe for lines _after_ the EOF.
  ;; Does nothing for in-the-file empty lines.
  (indicate-empty-lines nil)


  ;;------------------------------
  :config
  ;;------------------------------

  ;; Make `next-buffer', `other-buffer', etc. ignore unreal buffers.
  (buffer:next/other:ignore :unreal)

  (blink-cursor-mode +1))


;;------------------------------------------------------------------------------
;; Emacs Profiler
;;------------------------------------------------------------------------------

(imp:use-package profiler
  :ensure nil ; This is an Emacs built-in feature.

  ;;------------------------------
  :init
  ;;------------------------------

  ;; Larger column width for function name in profiler reports
  (setf (caar profiler-report-cpu-line-format)    80
        (caar profiler-report-memory-line-format) 80)


  (defvar mantle:profiler:enabled? nil
    "Boolean guess for if Emacs profiler is enabled.

Only accurate when using `mantle:cmd:profiler:toggle'.")


  (defun mantle:cmd:profiler:toggle ()
    "Toggle the Emacs profiler. Display the profiling report when toggling off."
    (interactive)
    (if (not mantle:profiler:enabled?)
        (profiler-start 'cpu+mem)
      (profiler-report)
      (profiler-stop))
    (setq mantle:profiler:enabled? (not mantle:profiler:enabled?))))


;;------------------------------------------------------------------------------
;; ANSI Color
;;------------------------------------------------------------------------------

(imp:use-package ansi-color
  :ensure nil ; This is an Emacs built-in feature.

  ;;------------------------------
  :custom
  ;;------------------------------

  ;; Translate ANSI SGR control sequences into text properties.
  ;; https://en.wikipedia.org/wiki/ANSI_escape_code#SGR
  (ansi-color-for-comint-mode t))


;;------------------------------------------------------------------------------
;; Minibuffer Tweaks
;;------------------------------------------------------------------------------
(imp:use-package emacs
  :after vertico
  :ensure nil ; This is an Emacs built-in feature.

  ;;------------------------------
  :init
  ;;------------------------------

  ;; Typing yes/no is obnoxious when y/n will do
  (advice-add #'yes-or-no-p :override #'y-or-n-p)

  ;; Try to keep the cursor out of the read-only portions of the minibuffer.
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  (define-advice completing-read-multiple (:filter-args (fn &rest args) mantle:minibuffer:completing-read-multiple:indicator)
    "Add a prompt indicator to `completing-read-multiple'.

We display a prefix with the `crm-separator' in the prompt.
Assuming:
  1. The separator is a comma.
  2. The prompt is \"prompt❯ \"
Example:
  ├CRM:,┤ prompt❯ _"
    ;; Add prefix to PROMPT in ARGS.
    (cons (format "├CRM:%s┤ %s" ; Unicode?
                  ;; "[CRM:%s] %s" ; ASCII only
                  (replace-regexp-in-string "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                                            crm-separator)
                  (car args))
          (cdr args)))


  ;;------------------------------
  :custom
  ;;------------------------------

  ;; Allow for minibuffer-ception. Sometimes we need another minibuffer command
  ;; while we're in the minibuffer.
  (enable-recursive-minibuffers t)

  ;; Show current key-sequence in minibuffer ala 'set showcmd' in vim. Any
  ;; feedback after typing is better UX than no feedback at all.
  (echo-keystrokes 0.02)

  ;; Expand the minibuffer to fit multi-line text displayed in the echo-area. This
  ;; doesn't look too great with direnv, however...
  (resize-mini-windows 'grow-only)

  ;; Try to keep the cursor out of the read-only portions of the minibuffer.
  (minibuffer-prompt-properties '(read-only         t
                                  intangible        t
                                  cursor-intangible t
                                  face              minibuffer-prompt))

  ;; TODO: Do I want to try this?
  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (read-extended-command-predicate #'command-completion-default-include-p)
  )


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'config 'user 'emacs)

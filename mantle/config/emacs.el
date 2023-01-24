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
  (icon-title-format frame-title-format))


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
  ;; Cursor
  ;;------------------------------

  (blink-cursor-interval 0.75) ; default is 0.5 seconds


  ;;------------------------------
  ;; Lines
  ;;------------------------------

  ;;This determines the style of line numbers in effect. If set to `nil', line
  ;; numbers are disabled. For relative line numbers, set this to `relative'.
  (display-line-numbers-type t)


  ;;------------------------------
  :config
  ;;------------------------------

  (blink-cursor-mode +1))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'config 'user 'emacs)

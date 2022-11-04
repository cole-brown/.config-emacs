;;; ui.el --- User Interface & Experience -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <code@brown.dev>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2022-11-02
;; Modified:   2022-11-02
;;
;;; Commentary:
;;
;; User Interface & Experience
;;
;; Frame & Window Tweaks?
;;
;;; Code:


(imp:require :buffer 'name)
(imp:require :datetime 'format)


;;------------------------------------------------------------------------------
;; File Time Formats
;;------------------------------------------------------------------------------

;; Set time format for e.g. dired to a better format:
;;   RFC-3339 (human-readable ISO-8601)
;; Original value:
;;   '(\"%d.%m.%Y %H:%M\" \"%d.%m.%Y %H:%M\"))
(customize-set-variable 'ls-lisp-format-time-list
                        (list
                         ;; Recent Time Format:
                         (datetime:format/get 'rfc-3339 'datetime)
                         ;; Not-Recent Time Format:
                         (datetime:format/get 'rfc-3339 'datetime)))

;; Force use of `ls-lisp-format-time-list' regardless of locale.
(customize-set-variable 'ls-lisp-use-localized-time-format t)


;;------------------------------------------------------------------------------
;; Buffer Names
;;------------------------------------------------------------------------------

(imp:use-package uniquify
  :ensure nil ;; This is part of emacs; don't install.

  ;;--------------------
  :custom
  ;;--------------------

  ;;---
  ;; Buffer Name
  ;;---
  ;; Set uniquify buffer/path separator to e.g. "file.txt:path/to"
  (uniquify-buffer-name-style 'post-forward)

  ;; I think ':' is better than '|' for `post-forward' style.
  (uniquify-separator ":")

  ;; Have at least this many dirs in buffer names.
  (uniquify-min-dir-content 1)

  ;; Add path separator to dired buffer names.
  (uniquify-trailing-separator-p t)

  ;;---
  ;; Other uniquify Settings
  ;;---
  ;; Rename buffers after killing a buffer. E.g. de-uniquify others as possible.
  (uniquify-after-kill-buffer-p t)

  ;; Ignored for uniquifying.
  ;; Don't muck with my or Emacs' special buffers.
  (uniquify-ignore-buffers-re buffer:regex:specials))



;;------------------------------------------------------------------------------
;; Modeline
;;------------------------------------------------------------------------------

;; Doom has a really nice looking modeline, so... use it?
(imp:use-package doom-modeline
  ;; Has optional support for `all-the-icons'. So... opt in to that.
  :after (all-the-icons evil)

  ;;--------------------
  :custom
  ;;--------------------
  ;; https://github.com/seagle0128/doom-modeline#customize

  ;;---
  ;; Modal Editting (Evil, etc)
  ;;---
  (doom-modeline-modal-icon t)

  ;;---
  ;; Buffer Name
  ;;---
  ;; Given ~/Projects/FOSS/emacs/lisp/comint.el
  ;;   auto => emacs/l/comint.el (in a project) or comint.el
  ;;   truncate-upto-project => ~/P/F/emacs/lisp/comint.el
  ;;   truncate-from-project => ~/Projects/FOSS/emacs/l/comint.el
  ;;   truncate-with-project => emacs/l/comint.el
  ;;   truncate-except-project => ~/P/F/emacs/l/comint.el
  ;;   truncate-upto-root => ~/P/F/e/lisp/comint.el
  ;;   truncate-all => ~/P/F/e/l/comint.el
  ;;   truncate-nil => ~/Projects/FOSS/emacs/lisp/comint.el
  ;;   relative-from-project => emacs/lisp/comint.el
  ;;   relative-to-project => lisp/comint.el
  ;;   file-name => comint.el
  ;;   buffer-name => comint.el<2> (uniquify buffer name)
  ;;---
  ;; NOTE: `relative-from-project' is good enough for the modeline, but the
  ;; buffer names themselves /need/ to be uniquify'd. And having different
  ;; actual buffer name and displayed buffer names is just confusing.
  (doom-modeline-buffer-file-name-style 'buffer-name)

  ;;---
  ;; Icons
  ;;---
  ;; Display icons on modeline?
  (doom-modeline-icon t)
  (doom-modeline-major-mode-icon t) ;; TODO: t or nil? Doom sets to nil.
  (doom-modeline-major-mode-color-icon t)
  (doom-modeline-buffer-state-icon t)
  (doom-modeline-buffer-modification-icon t)
  (doom-modeline-time-icon t) ;; TODO: t or nil?

  ;; Use unicode or ASCII for fallback when not using icons.
  (doom-modeline-unicode-fallback t)

  ;;---
  ;; Modes
  ;;---
  (doom-modeline-minor-modes nil)

  ;;---
  ;; Error Checker (LSP, Flymake)
  ;;---
  (doom-modeline-checker-simple-format t)
  (doom-modeline-lsp t)

  ;;---
  ;; Misc
  ;;---
  (doom-modeline-enable-word-count nil)
  (doom-modeline-buffer-encoding 'nondefault)
  (doom-modeline-indent-info nil)

  ;; Disabled stuff.
  (doom-modeline-mu4e nil)
  (doom-modeline-gnus nil)
  (doom-modeline-irc  nil)
  (doom-modeline-time nil)

  ;;--------------------
  :config
  ;;--------------------
 (doom-modeline-mode 1))


;;------------------------------------------------------------------------------
;; Title
;;------------------------------------------------------------------------------

;; This variable has the same structure as `mode-line-format', except that
;; the %c, %C, and %l constructs are ignored.  It is used only on frames for
;; which no explicit name has been set (see `modify-frame-parameters').
(customize-set-variable 'frame-title-format
                        ;; buffer name
                        '("｢%b｣"
                          ;; system/host name
                          " @"
                          (:eval (or (file-remote-p default-directory 'host) system-name))
                          ;; And I guess we should say what we are...
                          " — Emacs "
                          emacs-version))

;; This is for the Emacs icon thingy in the OS app list/taskbar.
;; Just have it be the same as the `frame-title-format'.
(customize-set-variable 'icon-title-format frame-title-format)



;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'config 'user 'ui)

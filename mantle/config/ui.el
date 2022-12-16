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
(innit:customize-set-variable ls-lisp-format-time-list
                        (list
                         ;; Recent Time Format:
                         (datetime:format/get 'rfc-3339 'datetime)
                         ;; Not-Recent Time Format:
                         (datetime:format/get 'rfc-3339 'datetime)))

;; Force use of `ls-lisp-format-time-list' regardless of locale.
(innit:customize-set-variable ls-lisp-use-localized-time-format t)


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

  ;;------------------------------
  :init
  ;;------------------------------

  (defun mantle:advice:doom-modeline:env:update/all-windows (&rest _)
    "Update version strings in all buffers."
    (dolist (window (window-list))
      (with-selected-window window
        (when (fboundp 'doom-modeline-update-env)
          (doom-modeline-update-env))
        (force-mode-line-update))))

  (defun mantle:advice:doom-modeline:env:clear/all-windows  (&rest _)
    "Blank out version strings in all buffers."
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (setq doom-modeline-env--version
              (bound-and-true-p doom-modeline-load-string))))
    (force-mode-line-update t))

  ;;---
  ;; Use in other modes in `:config'.
  ;;---
  ;; As advice:
  ;;   (when (functionp #'mantle:advice:doom-modeline:env:update/all-windows)
  ;;     (advice-add #'pythonic-activate :after-while #'mantle:advice:doom-modeline:env:update/all-windows))
  ;;   (when (functionp #'mantle:advice:doom-modeline:env:clear/all-windows)
  ;;     (advice-add #'pythonic-deactivate :after #'mantle:advice:doom-modeline:env:clear/all-windows))
  ;;
  ;; Or with hooks:
  ;;   (when (functionp #'mantle:advice:doom-modeline:env:update/all-windows)
  ;;     (add-hook 'pyvenv-post-activate-hooks  #'mantle:advice:doom-modeline:env:update/all-windows))
  ;;   (when (functionp #'mantle:advice:doom-modeline:env:clear/all-windows)
  ;;     (add-hook 'pyvenv-pre-deactivate-hooks #'mantle:advice:doom-modeline:env:clear/all-windows))


  ;;------------------------------
  :custom
  ;;------------------------------
  ;; https://github.com/seagle0128/doom-modeline#customize

  ;;---
  ;; Modal Editting (Evil, etc)
  ;;---
  (doom-modeline-modal-icon t)

  ;;---
  ;; Perspective/Workspace
  ;;---
  ;; Add the workspace/perspective name to the modeline.
  (doom-modeline-persp-name t)

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

  ;;------------------------------
  :config
  ;;------------------------------

 (doom-modeline-mode 1))


;;------------------------------------------------------------------------------
;; Title
;;------------------------------------------------------------------------------

;; This variable has the same structure as `mode-line-format', except that
;; the %c, %C, and %l constructs are ignored.  It is used only on frames for
;; which no explicit name has been set (see `modify-frame-parameters').
(innit:customize-set-variable frame-title-format
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
(innit:customize-set-variable icon-title-format frame-title-format)


;;------------------------------------------------------------------------------
;; Cursor
;;------------------------------------------------------------------------------

(blink-cursor-mode 1)
(setq blink-cursor-interval 0.75) ; default is 0.5 seconds


;;------------------------------------------------------------------------------
;; Lines
;;------------------------------------------------------------------------------

;;This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;;------------------------------------------------------------------------------
;; Indentation Guide
;;------------------------------------------------------------------------------

(imp:use-package highlight-indent-guides

  ;;------------------------------
  :init
  ;;------------------------------
  (innit:hook:defun
      (:name    'highlight-indent-guides:faces/init
       :file    macro<imp>:path/file
       :docstr  (mapconcat #'identity
                           '("Set indent faces if possible."
                             ""
                             "HACK: `highlight-indent-guides' calculates its faces from the current theme, but"
                             "is unable to do so properly in terminal Emacs, where it only has access to 256"
                             "colors. So if the user uses a daemon we must wait for the first graphical frame"
                             "to be available to do.")
                           "\n")
       :squelch t)
      (when (display-graphic-p)
        (highlight-indent-guides-auto-set-faces)))

  (innit:hook:defun
      (:name    'highlight-indent-guides:disable-maybe
       :file    macro<imp>:path/file
       :docstr  (mapconcat #'identity
                           '("Disable `highlight-indent-guides' if `org-indent-mode'."
                             ""
                             "`highlight-indent-guides' breaks when `org-indent-mode' is active.")
                           "\n")
       :squelch t)
      (and highlight-indent-guides-mode
           (bound-and-true-p org-indent-mode)
           (highlight-indent-guides-mode -1)))


  ;;------------------------------
  :hook
  ;;------------------------------
  (((prog-mode-hook text-mode-hook conf-mode-hook) . highlight-indent-guides-mode)
   (innit:theme:load:hook . mantle:hook:highlight-indent-guides:faces/init)
   (org-mode-local-vars-hook . mantle:hook:highlight-indent-guides:disable-maybe))


  ;;------------------------------
  :custom
  ;;------------------------------

  (highlight-indent-guides-suppress-auto-error t)

  ;; How the indent guides look.
  ;; https://github.com/DarthFennec/highlight-indent-guides/tree/cf352c85cd15dd18aa096ba9d9ab9b7ab493e8f6#screenshots
  ;; Doom used `character'; `bitmap' looks neater.
  ;; ...but `fill' might be best for avoiding the buggy looking indents that `character' gets into.
  (highlight-indent-guides-method 'fill) ;; 'bitmap)

  (highlight-indent-guides-auto-even-face-perc      4) ;; default: 10
  (highlight-indent-guides-auto-odd-face-perc       2) ;; default:  5
  (highlight-indent-guides-auto-character-face-perc 5) ;; default: 10


  ;;------------------------------
  :config
  ;;------------------------------

  ;; If theme is already loaded, run the hook.
  (when innit:theme:loaded
    (mantle:hook:highlight-indent-guides:faces/init)))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'config 'user 'ui)

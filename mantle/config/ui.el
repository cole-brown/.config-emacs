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


(imp:require :path)
(imp:require :buffer 'name)
(imp:require :datetime 'format)


;;------------------------------------------------------------------------------
;; Buffer Names
;;------------------------------------------------------------------------------

;;------------------------------
;; `uniquify'
;;------------------------------
;; (imp:use-package uniquify
;;   :ensure nil ; This is an Emacs built-in feature.
;;
;;   ;;------------------------------
;;   :custom
;;   ;;------------------------------
;;
;;   ;;---
;;   ;; Buffer Name
;;   ;;---
;;   ;; Set uniquify style to e.g. "file.txt:path/to"
;;   (uniquify-buffer-name-style 'post-forward)
;;   ;; Set uniquify style to a custom thing:
;;   ;;   (uniquify-buffer-name-style #'path:project:uniquify)
;;
;;   ;; I think ':' is better than '|' for `post-forward' style.
;;   (uniquify-separator ":")
;;
;;   ;; Have at least this many dirs in buffer names.
;;   (uniquify-min-dir-content 3)
;;
;;   ;; Add path separator to dired buffer names.
;;   (uniquify-trailing-separator-p t)
;;
;;   ;;---
;;   ;; Other uniquify Settings
;;   ;;---
;;   ;; Rename buffers after killing a buffer. E.g. de-uniquify others as possible.
;;   (uniquify-after-kill-buffer-p t)
;;
;;   ;; Ignored for uniquifying.
;;   ;; Don't muck with my or Emacs' special buffers.
;;   (uniquify-ignore-buffers-re (buffer:regex:specials)))


;;------------------------------
;; `path:uniquify'
;;------------------------------
;; NOTE: Not (currently) intended to be used in combination with `uniquify'.

;; For debugging when `uniquify' may be in the mix...
(when (bound-and-true-p uniquify-buffer-name-style)
  (innit:customize-set-variable uniquify-buffer-name-style nil)
  (uniquify-unload-function))

(imp:require :path '+uniquify)

;; Add our special buffers regex to the ignored-by-name regex.
(innit:customize-set-variable path:uniquify:ignore/buffer:name/rx
                              (list 'and
                                    'string-start
                                    (list 'or
                                          ;; The defaults...
                                          path:uniquify:ignore/buffer:name/rx:defaults ; Be more idempotent; use the original value.
                                          ;; Or my special buffers.
                                          buffer:regex:bookend)
                                    'string-end))
;; path:uniquify:ignore/buffer:name/rx
;; (pp path:uniquify:ignore/buffer:name/rx)
;; (path:uniquify:ignore/buffer:name/rx)


(path:uniquify:set-up)
;; (path:uniquify:tear-down)


;;------------------------------------------------------------------------------
;; Modeline
;;------------------------------------------------------------------------------
;; Doom has a really nice looking modeline, so... use it?

(imp:use-package doom-modeline
  ;; Has optional support for `all-the-icons'. So... opt in to that.
  :after all-the-icons

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
  ;; TODO-meow: Does this work with `meow' or do I gotta hack in some stuff?
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
  ;;   (doom-modeline-buffer-file-name-style 'buffer-name)
  ;; NOTE [2023-04-21]: Testing out a custom buffer naming style?
  (doom-modeline-buffer-file-name-style 'mantle:doom-modeline:project/truncate-path)
  ;; (setq doom-modeline-buffer-file-name-style 'mantle:doom-modeline:project/truncate-path)
  ;; (setq doom-modeline-buffer-file-name-style 'buffer-name)


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

  (define-advice doom-modeline-buffer-file-name (:around (fn &rest args) mantle:advice/override)
    "Propertize file name based on `doom-modeline-buffer-file-name-style'.

Check `doom-modeline-buffer-file-name-style' for custom styles; pass on to
original function if not custom."
    (pcase doom-modeline-buffer-file-name-style
      ;;------------------------------
      ;; Custom Style?
      ;;------------------------------
      ('mantle:doom-modeline:project/truncate-path
       (let* ((buffer        (current-buffer))
              (project/alist (if (path:uniquify:buffer/managed? buffer)
                                 ;; Use the pre-existing data for this buffer.
                                 (path:uniquify:settings/get :project buffer)
                               ;; We don't have anything saved for this buffer; we'll have to figure it out.
                               (path:project:current/alist (path:buffer buffer)))))
         (path:project:buffer/name:propertize :project/name (list (alist-get :project/name project/alist)
                                                                  'face 'underline)
                                              :project/path (list (alist-get :path project/alist))
                                              :truncate 'path
                                              :modeline? t)))
      ;;------------------------------
      ;; Standard Style?
      ;;------------------------------
       ;; Otherwise dunno what that style is; pass through to original function.
       (_
        (apply fn args))))


  ;;------------------------------
  ;; Enable
  ;;------------------------------
  (doom-modeline-mode +1))


;;------------------------------------------------------------------------------
;; Highlight: Indentation Guide
;;------------------------------------------------------------------------------

(imp:use-package highlight-indent-guides

  ;;------------------------------
  :init
  ;;------------------------------
  (innit:hook:defun
      (:name    'highlight-indent-guides:faces/init
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
;; Highlight: Current Line
;;------------------------------------------------------------------------------

(imp:use-package hl-line
  :ensure nil ; This is an Emacs built-in feature.

  ;;------------------------------
  :init
  ;;------------------------------

  ;; Temporarily disable `hl-line' when selection is active, since it doesn't
  ;; serve much purpose when the selection is so much more visible.
  (defvar mantle:user:ui:hl-line:enabled?/local-cache nil)

  ;; TODO: Do we need this hook? It would hook into `hl-line-mode-hook'.
  ;; See Doom's "core/core-ui.el".
  ;; (innit:hook:defun
  ;;     (:name    'hl-line:disable/local
  ;;      :docstr  "Disable `hl-line' by setting our local var."
  ;;      :squelch t)
  ;;  (unless hl-line-mode
  ;;    (setq-local mantle:user:ui:hl-line:enabled/local? nil)))

  (innit:hook:defun
      (:name    'hl-line:enable/global
       :docstr  "Disable `hl-line' when starting to select text.")
  (global-hl-line-mode +1))

  (innit:hook:defun
      (:name    'hl-line:select-text/enter
       :docstr  "Disable `hl-line' when starting to select text."
       :squelch t)
    (when (bound-and-true-p hl-line-mode)
      (setq-local mantle:user:ui:hl-line:enabled?/local-cache t)
      (hl-line-mode -1)))

  (innit:hook:defun
      (:name    'hl-line:select-text/exit
       :docstr  "Enable `hl-line' when done selecting text."
       :squelch t)
   (when mantle:user:ui:hl-line:enabled?/local-cache
     (hl-line-mode +1)))


  ;;------------------------------
  :hook
  ;;------------------------------

  (;(innit:theme:load:hook . global-hl-line-mode)
   (emacs-startup-hook . mantle:hook:hl-line:enable/global)
   ((evil-visual-state-entry-hook activate-mark-hook)  . mantle:hook:hl-line:select-text/enter)
   ((evil-visual-state-exit-hook deactivate-mark-hook) . mantle:hook:hl-line:select-text/exit))


  ;; ;;------------------------------
  ;; :config
  ;; ;;------------------------------
  ;;
  ;; TODO: Do I needthis hack from Doom? Don't think I want/need a hl-line mode white/blacklist?
  ;;
  ;; (defvar global-hl-line-modes
  ;;   '(prog-mode text-mode conf-mode special-mode
  ;;     org-agenda-mode dired-mode)
  ;;   "What modes to enable `hl-line-mode' in.")
  ;;
  ;; ;; HACK I reimplement `global-hl-line-mode' so we can white/blacklist modes in
  ;; ;;      `global-hl-line-modes' _and_ so we can use `global-hl-line-mode',
  ;; ;;      which users expect to control hl-line in Emacs.
  ;; (define-globalized-minor-mode global-hl-line-mode hl-line-mode
  ;;   (lambda ()
  ;;     (and (cond (hl-line-mode nil)
  ;;                ((null global-hl-line-modes) nil)
  ;;                ((eq global-hl-line-modes t))
  ;;                ((eq (car global-hl-line-modes) 'not)
  ;;                 (not (derived-mode-p global-hl-line-modes)))
  ;;                ((apply #'derived-mode-p global-hl-line-modes)))
  ;;          (hl-line-mode +1))))
  )


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'config 'user 'ui)

;;; mantle/config/common.el --- General Development Environment Stuff -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2022-08-05
;; Modified:   2022-12-09
;; URL:        https://github.com/cole-brown/.config-emacs
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;  General Development Environment Stuff
;;
;;; Code:


(imp:require :jerky)


;;------------------------------------------------------------------------------
;; Tabs
;;------------------------------------------------------------------------------

(jerky:set 'tab 'width 'code 'normal
           :namespace :default
           :value 4
           :docstr "Default/normal tab width is 4 spaces.")

(jerky:set 'tab 'width 'code 'short
           :namespace :default
           :value 2
           :docstr "Short tab width is 2 spaces.")


;; Use spaces instead of tabs.
(customize-set-variable 'indent-tabs-mode nil)


;;------------------------------------------------------------------------------
;; Color Codes
;;------------------------------------------------------------------------------

;; Set the background color of color strings to the color they represent.
;; e.g.:
;;   - #ff1100
;;   - #abc
;;   - rgb(100, 100, 100)
;;   - YellowGreen
;;   - etc
;; https://github.com/emacsmirror/rainbow-mode/blob/master/rainbow-mode.el
;; https://elpa.gnu.org/packages/rainbow-mode.html
(imp:use-package rainbow-mode

  ;;--------------------
  :init
  ;;--------------------

  ;;---
  ;; Create a hook for enabling this minor mode.
  ;;---
  ;; Creates a func called `mantle:hook:rainbow-mode/enable'.
  ;;   (innit:hook:func/name:symbol "rainbow-mode/enable" nil)
  (innit:hook:defun
      (:name "rainbow-mode/enable"
       :file macro<imp>:path/file
       :docstr "Enable `rainbow-mode' (colorize color codes) for this buffer."
       :quiet t)
    (rainbow-mode +1))

  ;;--------------------
  :hook
  ;;--------------------

  ((org-mode-hook    . mantle:hook:rainbow-mode/enable)
   (csharp-mode-hook . mantle:hook:rainbow-mode/enable)
   (css-mode-hook    . mantle:hook:rainbow-mode/enable)
   (php-mode-hook    . mantle:hook:rainbow-mode/enable)
   (html-mode-hook   . mantle:hook:rainbow-mode/enable)))


;;------------------------------------------------------------------------------
;; Diff
;;------------------------------------------------------------------------------

;;------------------------------
;; Ediff
;;------------------------------

(imp:use-package ediff

  ;;--------------------
  :init
  ;;--------------------

  (defvar mantle:user:ediff:window/cache nil
    "Cache of window config so it can be restored after quitting `ediff'.")

  (innit:hook:defun
      (:name   "ediff:window/save"
       :file   macro<imp>:path/file
       :docstr "Save window config so it can be restored after quitting `ediff'."
       :quiet  t)
    (setq mantle:user:ediff:window/cache (current-window-configuration)))

  ;; NOTE: Cannot make the `ediff:window/restore' hook here as we need the
  ;; `:depth' argument and `use-package' does not support it in the `:hook'
  ;; section. So that one is in the `:config'.


  ;;--------------------
  :hook
  ;;--------------------
  ;; Note: Use `innit:cmd:hook:func/name' to insert the func names created via the `innit:hook:defun' `:name' field.
  ((ediff-before-setup-hook . mantle:hook:ediff:window/save))


  ;;--------------------
  :custom
  ;;--------------------

  ;; Ignore whitespace.
  (ediff-diff-options "-w")

  ;; 'Unified'(?) instead of 'copied' context? IDK...
  (ediff-custom-diff-options "-u")

  ;; Always one frame.
  (ediff-window-setup-function 'ediff-setup-windows-plain)

  ;; Side-by-side instead of default top/bottom split.
  (ediff-split-window-function 'split-window-horizontally)


  ;;--------------------
  :config
  ;;--------------------

  (innit:hook:defun-and-add
      (ediff-quit-hook ediff-suspend-hook)
      (:name   "ediff:window/restore"
       :file   macro<imp>:path/file
       :docstr "Restore a saved window config after quitting `ediff'."
       :depth  'append
       :quiet  t)
    (when (window-configuration-p mantle:user:ediff:window/cache)
      (set-window-configuration mantle:user:ediff:window/cache))
    (setq mantle:user:ediff:window/cache nil))
  )


;;------------------------------------------------------------------------------
;; Comments
;;------------------------------------------------------------------------------

;;------------------------------
;; Block Commenting Style
;;------------------------------

;; Only use for languages that have ending comments, like C's "/* */".
(innit:hook:defun
    (:name   "comments:block/align"
     :file   macro<imp>:path/file
     :docstr "Set comment style for start/end line comment languages (C, HTML, ...)."
     :quiet  t)
  ;; `aligned' is like (default) `indent' but also aligns end-of-line comment chars if the language has them.
  (setq 'comment-style 'aligned))
;; NOTE: Need to add this hook (`mantle:hook:comments:block/align') to whatever modes want it.

;;---
;; Add hook to languages that need it.
;;---

;; TODO: Move to "language/html.el"?
;; `html-mode-hook' (HTML mode) or `mhtml-mode-hook' (HTML+ mode (derived from HTML mode))?
(add-hook 'html-mode-hook 'mantle:hook:comments:block/align)

;; C-mode hook added (or not) in "language/c-and-cpp.el".


;;------------------------------------------------------------------------------
;; Defaults for All Programming Modes
;;------------------------------------------------------------------------------
;; TODO: move to languages dir?

(spy:hook/defun-and-hooker prog-mode-hook
    '(:name "prog-mode/settings"
      :file ".doom.d/config/code.el"
      :docstr "Settings for all prog-mode derived modes. Non-LSP stuff."
      :quiet t)

    ;; My 2560x1440 monitor can display around 152 columns when fullscreen with
    ;; two vertically-split windows, so we'll do 140 as our fill column?
    (setq fill-column 140))


;;------------------------------------------------------------------------------
;; Metasyntatic Variables
;;------------------------------------------------------------------------------

(defcustom mantle:user:code:variables/metasyntatic
  '((:metasyntactic
     (foo (bar (baz (qux (quux (quuux (quuuux (quuuuux))))))
               (thud (grunt))
               (bletch)
               (fum)
               (bongo)
               (zot)))
     (bazola (ztesch))
     (fred (jim (sheila (barney))))
     (corge (grault (flarp)))
     (zxc (spqr (wombat)))
     (shme)
     (spam (eggs))
     (snork)
     (blarg (wibble))
     (toto (titi (tata (tutu))))
     (pippo (pluto (paperino)))
     (aap (noot (mies)))
     (oogle (foogle (boogle (zork (gork (bork)))))))

    (:pinky (narf (zort (poit (egad (troz (fiddely-posh))))))))
  "Alist of meaningless/placeholder variable name progressions.

See: http://www.catb.org/jargon/html/M/metasyntactic-variable.html")


;; (defun mantle:user:code:variable/metasyntatic (&optional type)
;;   "Insert a metasyntatic variable at point.
;;
;; If TYPE is a keyword, use that list in variable
;; `mantle:user:code:variable/metasyntatic'."
;;   (interactive)
;;   (let ((tree-of-meta (alist-get (or nil :metasyntactic) mantle:user:code:variables/metasyntatic)))
;;     ;; TODO: Would be nice to progress in the metavars usage but idk how to, easily.
;;     ;; So just... chose a random?
;;     ;; TODO: finish this?
;;     ))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'config 'user 'dev-env 'common)

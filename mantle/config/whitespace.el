;;; whitespace.el --- Configure the Unseen -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2022-08-05
;; Modified:   2022-08-05
;; URL:        https://github.com/cole-brown/.config-emacs
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;  Configure the Unseen
;;
;;; Code:


(imp:require :jerky)


;;------------------------------------------------------------------------------
;; Tabs
;;------------------------------------------------------------------------------
;; Save these in Jerky so various mode hooks can get at them.

;;------------------------------
;; Widths: Documents
;;------------------------------

(jerky:set 'docs 'tab 'short
           :namespace :default
           :value 2
           :docstr "Short tab width is 2 spaces.")

(jerky:set 'docs 'tab 'standard
           :namespace :default
           :value 4
           :docstr "Standard tab width is 4 spaces.")

;; Long would be... 8, I think? But that's ridiculous and I don't like it so
;; it's not here until it's needed.


;;------------------------------
;; Indents: Code
;;------------------------------

(jerky:set 'code 'tab 'short
           :namespace :default
           :value 2
           :docstr "Short tab width is 2 spaces.")

(jerky:set 'code 'tab 'standard
           :namespace :default
           :value 4
           :docstr "Standard tab width is 4 spaces.")

;; Set Emacs' standard to our's.
(innit:customize-set-variable standard-indent (jerky:get 'code 'tab 'standard))

;; Long would be... 8, I think? But that's ridiculous and I don't like it so
;; it's not here until it's needed.


;;------------------------------
;; Tab Settings
;;------------------------------
;; https://www.emacswiki.org/emacs/NoTabs
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Just-Spaces.html
;; https://www.emacswiki.org/emacs/TabsAreEvil
;; https://www.emacswiki.org/emacs/SmartTabs

;; Always use spaces; never use tabs.
(innit:customize-set-variable indent-tabs-mode nil)

;; Set default tab width for all buffers.
(innit:customize-set-variable tab-width (jerky:get 'code 'tab 'standard))

;; Make sure this is at it's default of nil, because:
;;   "A value of nil means a tab stop every `tab-width' columns."
(innit:customize-set-variable tab-stop-list nil)

;; NOTE: M-x tabify and M-x untabify exist and work on regions.


;;------------------------------------------------------------------------------
;; Clean Whitespace
;;------------------------------------------------------------------------------

;; Whitespace Butler: Only removes whitespace from regions you've changed.
;;   https://melpa.org/#/ws-butler
;;
;; NOTE: Use `ws-butler' instead of `whitespace-cleanup' so as to avoid noisy
;; commits. Can still manually call `whitespace-cleanup' on files that bug you
;; too much.
(imp:use-package ws-butler
  :demand t

  ;;------------------------------
  :custom
  ;;------------------------------

  (ws-butler-convert-leading-tabs-or-spaces t)

  ;; NOTE: If modes need to be excluded, customize the variable `ws-butler-global-exempt-modes'.


  ;;------------------------------
  :config
  ;;------------------------------

  ;; HACK: Bug fix via advice. See for more info:
  ;;   emacs-sn004:/docs/issues/2019-08-26_whitespace-mode-and-move-to-column.org
  (define-advice move-to-column (:filter-args (args) ws-butler/whitespace-mode:fix/force-off-by-one)
    "Un-lose the one single space that's being lost sometimes.

Bug Conditions:
  1. `whitespace-mode' is on
  2. `move-to-column' is called with 'force' set true (`ws-butler' does this).
  3. `ws-butler-keep-whitespace-before-point' is on
Number 3 isn't actually necessary but it's the only time I've
noticed this bug (aside from contriving it in bug hunts/repros).

Repro:
  1. Start emacs without user init: `emacs -q`
  2. Paste the following code into the scratch buffer.
  3. Run `M-x eval-buffer`
  4. Run `M-x bug:whitespace:repro`

(defun bug:whitespace:move-30-force ()
  (interactive)
  (move-to-column 30 t))

(defun bug:whitespace:repro ()
  (interactive)
  ;; Set Up
  (let ((buffer-name \"bug:whitespace:repro\"))
    ;; Get a fresh buffer for each test.
    (when-let ((buffer (get-buffer buffer-name)))
      (kill-buffer buffer-name))
    (pop-to-buffer (get-buffer-create buffer-name)))
  ;; Make sure we don't have our bugfix advice running when we're trying to prove the bug.
  (require 'nadvice)
  (advice-mapc (lambda (advice _props) (advice-remove sym advice)) 'move-to-column)
  ;; Don't want tabs when we're trying to count spaces.
  (setq indent-tabs-mode nil)
  ;; Not needed for bug; for seeing column/line on modeline.
  (column-number-mode t)
  (line-number-mode t)

  ;;------------------------------
  ;; Reproduce the bug!
  ;;------------------------------
  ;; The bug: `whitespace-mode' and `move-to-column' (only when FORCE is non-nil
  ;; (and it actually has to force)) interact with each other to produce an
  ;; off-by-one error. For this example, we'll do \"Move 1\" without
  ;; `whitespace-mode', to see what's expected of `(move-to-column 30 t)'.
  ;;
  ;; Legend:
  ;;   - '█' : our text cursor
  ;;   - '-' : space
  ;;     - '·' : space

  ;;---
  ;; [OK] Move 1: Unbugged Move:
  ;;---
  ;; Move 1: Expect:
  ;; move-1:······················█
  ;; Move 1: Get:
  ;; move-1:······················█
  (insert \"move-1:[OK]:\")
  (move-to-column 30 t)
  (insert (format \"column: %2d\" (current-column)))

  ;;---
  ;; Bug set up:
  ;;---
  ;; 1. Whitespace mode must be enabled!
  (whitespace-mode 'enable)
  ;; 2. We must not be on the final line of the buffer.
  (insert \"\n\n\")
  (forward-line -1)

  ;;---
  ;; [ERROR] Move 2: Bugged Move:
  ;;---
  ;; Move 2: Expect:
  ;; move-2:······················█
  ;; Move 2: Get:
  ;; move-2:·····················█
  (insert \"move-2:[ERROR]:\")
  (move-to-column 30 t)
  (insert (format \"column: %2d\" (current-column)))

  ;;---
  ;; [OK] Move 3: Final line of buffer is immune to bug:
  ;;---
  ;; Move 3: Expect:
  ;; move-3:······················█
  ;; Move 3: Get:
  ;; move-3:······················█
  (goto-char (point-max))
  (insert \"move-3:[OK]:\")
  (move-to-column 30 t)
  (insert (format \"column: %2d\" (current-column))))"
    (let ((column (nth 0 args))
          (force (nth 1 args)))
      ;; Bug Conditions:
      ;;   1. `whitespace-mode' is on
      ;;   2. `move-to-column' is called with 'force' set true.
      ;;   3. `ws-butler-keep-whitespace-before-point' is on
      ;; Number 3 isn't actually necessary but it's the only time I've
      ;; noticed this bug (aside from contriving it in bug hunts/repros).
      (when (and (or (bound-and-true-p global-whitespace-mode)
                     (bound-and-true-p whitespace-mode))
                 force
                 ;; Not needed but ws-butler is what triggers this all the time
                 ;; so I'll contain my brute force fix to only work if ws-butler
                 ;; is setup to expect move-to-column to restore point.
                 (bound-and-true-p ws-butler-keep-whitespace-before-point))
        ;; Possibly a bugged move-to-column... Let's figure out how far we
        ;; have to go.
        (save-excursion
          (let ((at-last-line (> (forward-line 1) 0)))
            (unless at-last-line (forward-line -1))
            (move-end-of-line nil)
            (when (and (> column (current-column))
                       (not at-last-line))
              ;; We're in bug territory, and we want past current EOL, and this
              ;; line has a '\n' in it, so I think we have a bugged
              ;; move-to-column case. Up by one to offset for move-to-column's
              ;; off-by-one-in-this-instance bug.
              (setq column (1+ column))))))
      ;; return list of (fixed or ignored) inputs
      (list column force)))
  ;; (advice-remove 'move-to-column #'move-to-column@ws-butler/whitespace-mode:fix/force-off-by-one)

  ;; Turn on ws-butler globally.
  (ws-butler-global-mode 1))


;;------------------------------------------------------------------------------
;; View Whitespace
;;------------------------------------------------------------------------------

(imp:use-package whitespace
  :ensure nil ; This is an Emacs built-in feature.
  :demand t

  ;;------------------------------
  :init
  ;;------------------------------

  (defun mantle:user:whitespace:frame/childless? ()
    "Return non-nil if current buffer should obey `global-whitespace-mode'.

In this case, buffers that have no `parent-frame' should show whitespace.
Otherwise `whitespace-mode' inundates child frames with whitespace markers, so
this will disable it in child frames to fix all that visual noise.

Add to `whitespace-enable-predicate' via `add-function'."
    (null (frame-parameter nil 'parent-frame)))

  (innit:hook:defun
      (:name    "org/whitespace:mode/enter"
       :docstr  "I like some whitespace-mode stuff in org-mode, but want less than other modes."
       :squelch t)
    ;; Make a local copy of `whitespace-style' we can modify and...
    (set (make-local-variable 'whitespace-style)
         ;; ...set it as old one with removed 'too-long line' highlighting.
         (remove 'lines-tail whitespace-style)))

  (innit:hook:defun
      (:name    "org/whitespace:save/pre"
       :docstr  "Invoke `whitespace-cleanup' if the `ws-butler' feature is not present."
       :squelch t)
   (unless (featurep 'ws-butler)
     (whitespace-cleanup)))


  ;;------------------------------
  :hook
  ;;------------------------------
  ((org-mode-hook    . mantle:hook:org/whitespace:mode/enter) ;; (innit:hook:func/name:symbol "org/whitespace:mode/enter" nil)
   (before-save-hook . mantle:hook:org/whitespace:save/pre))  ;; (innit:hook:func/name:symbol "org/whitespace:save/pre" nil)


  ;;------------------------------
  :custom
  ;;------------------------------

  ;; Set 'lines-tail/'lines in `whitespace-style' to base off of `fill-column' instead of just hard-coded `80'.
  (whitespace-line-column nil)

  ;; Enable specific styles:
  (whitespace-style
   (quote
    ;;---
    ;; visualization via faces (see set-face-attribute below)
    ;;---
    (face

     ;;---
     ;; general/normal whitespace
     ;;---
     tabs spaces newline

     ;;---
     ;; the bad kind
     ;;---
     trailing space-before-tab space-after-tab

     ;; `empty' lines were annoying as emacs or whitespace is bad at cleaning up
     ;; the visualization when the line is no longer matching this whitespace
     ;; warning type.
     ;;empty       ;; ...lines (...at beginning/end of buffer)

     lines-tail  ;; `lines' would be whole line...
     ;; lines-tail is just whatever's past fill-column

     ;;---
     ;; not sure if want or bad or what.
     ;;---
     indentation

     ;;---
     ;; visualize these whitespaces with non-whitespace chars via display-table
     ;;---
     space-mark tab-mark newline-mark)))


  ;;------------------------------
  :config
  ;;------------------------------

  ;; NOTE: See thime's tweaks (e.g. "mantle/theme/zenburn/whitespace.el") for
  ;; changes to face attributes.

  ;; `whitespace-mode' inundates child frames with whitespace markers; this
  ;; fixes all that visual noise.
  (add-function :before-while whitespace-enable-predicate #'mantle:user:whitespace:frame/childless?)

  ;; Enable whitespace-mode everywhere.
  (global-whitespace-mode +1))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'config 'user 'whitespace)

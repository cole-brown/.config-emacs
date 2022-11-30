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
;; Widths: Code
;;------------------------------

(jerky:set 'code 'tab 'short
           :namespace :default
           :value 2
           :docstr "Short tab width is 2 spaces.")

(jerky:set 'code 'tab 'standard
           :namespace :default
           :value 4
           :docstr "Standard tab width is 4 spaces.")

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
(customize-set-variable 'indent-tabs-mode nil)

;; Set default tab width for all buffers.
(customize-set-variable 'tab-width (jerky:get 'code 'tab 'standard))

;; Make sure this is at it's default of nil, because:
;;   "A value of nil means a tab stop every `tab-width' columns."
(customize-set-variable 'tab-stop-list nil)

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

  ;;--------------------
  :custom
  ;;--------------------

  (ws-butler-convert-leading-tabs-or-spaces t)

  ;; NOTE: If modes need to be excluded, customize the variable `ws-butler-global-exempt-modes'.


  ;;--------------------
  :config
  ;;--------------------

  ;;------------------------------
  ;; TODO: Does this bug still exist? This was from a few years ago in sn-002 config.
  ;;------------------------------
  ;;     ;; For a bug. See for more info:
  ;;     ;;  sn-002 docs: "whitespace-and-butler/move-to-column.org"
  ;;     (defun sn-002/advice/move-to-column/force-fix (args)
  ;;       "Un-lose the one single space that's being lost sometimes."
  ;;       (let ((column (nth 0 args))
  ;;             (force (nth 1 args)))
  ;;
  ;;         ;; bug conditions:
  ;;         ;;   1. whitespace-mode is on
  ;;         ;;   2. move-to-column is called with 'force' set true.
  ;;         ;;   3. ws-butler-keep-whitespace-before-point is on
  ;;         ;; Number 3 isn't actually necessary but it's the only time I've
  ;;         ;; noticed this bug (aside from contriving it in bug hunts/repros).
  ;;         (when (and (or global-whitespace-mode whitespace-mode)
  ;;                    force
  ;;                    ;; Not needed but ws-butler is what triggers this all the time
  ;;                    ;; so I'll contain my brute force fix to only work if ws-butler
  ;;                    ;; is setup to expect move-to-column to restore point.
  ;;                    ws-butler-keep-whitespace-before-point)
  ;;           ;; Possibly a bugged move-to-column... Let's figure out how far we
  ;;           ;; have to go.
  ;;           (save-excursion
  ;;             (let ((at-last-line (> (forward-line 1) 0)))
  ;;               (unless at-last-line (forward-line -1))
  ;;               (move-end-of-line nil)
  ;;               (when (and (> column (current-column))
  ;;                          (not at-last-line))
  ;;                 ;; We're in bug territory, and we want past current EOL, and this
  ;;                 ;; line has a '\n' in it, so I think we have a bugged
  ;;                 ;; move-to-column case. Up by one to offset for move-to-column's
  ;;                 ;; off-by-one-in-this-instance bug.
  ;;                 (setq column (1+ column))))))
  ;;         ;; return list of (fixed or ignored) inputs
  ;;         (list column force)))
  ;;     ;; And now add our shenanigan to the function...
  ;;     (advice-add 'move-to-column
  ;;                 :filter-args #'sn-002/advice/move-to-column/force-fix)
  ;;     ;;(advice-remove 'move-to-column #'sn-002/advice/move-to-column/force-fix)
  ;;------------------------------

  ;; Turn on ws-butler globally.
  (ws-butler-global-mode 1))


;;------------------------------------------------------------------------------
;; View Whitespace
;;------------------------------------------------------------------------------

(imp:use-package whitespace
  :ensure nil
  :demand t

  ;;--------------------
  :init
  ;;--------------------
  (innit:hook:defun
      (list :name    "org/whitespace:mode/enter"
            :file    (path:current:file)
            :docstr  "I like some whitespace-mode stuff in org-mode, but want less than other modes."
            :squelch t
            :quiet   t)
    ;; Make a local copy of `whitespace-style' we can modify and...
    (set (make-local-variable 'whitespace-style)
         ;; ...set it as old one with removed 'too-long line' highlighting.
         (remove 'lines-tail whitespace-style)))

  (innit:hook:defun
     (list :name    "org/whitespace:save/pre"
           :file    (path:current:file)
           :docstr  "Invoke `whitespace-cleanup' if the `ws-butler' feature is not present."
           :squelch t
           :quiet   t)
   (unless (featurep 'ws-butler)
     (whitespace-cleanup)))


  ;;--------------------
  :hook
  ;;--------------------
  ((org-mode-hook    . mantle:hook:org/whitespace:mode/enter) ;; (innit:hook:func/name:symbol "org/whitespace:mode/enter" nil)
   (before-save-hook . mantle:hook:org/whitespace:save/pre))  ;; (innit:hook:func/name:symbol "org/whitespace:save/pre" nil)


  ;;--------------------
  :custom
  ;;--------------------

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


  ;;--------------------
  :config
  ;;--------------------

  ;; NOTE: See thime's tweaks (e.g. "mantle/theme/zenburn/whitespace.el") for
  ;; changes to face attributes.

  ;; Enable whitespace-mode everywhere.
  (global-whitespace-mode +1))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'config 'user 'whitespace)

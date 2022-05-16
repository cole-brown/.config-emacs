;;; org-mode.el --- Zenburn & Org-Mode Customizations -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Cole Brown
;;
;; Author: Cole Brown <code@brown.dev>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created: May 12, 2022
;; Modified: May 12, 2022
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/work/org-mode
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Zenburn & Org-Mode Customizations
;;
;;; Code:

;;; zenburn.el --- Low-Contrast Theme -*- lexical-binding: t; -*-
;;
;; Author: Cole Brown <code@brown.dev>
;; URL:    https://github.com/cole-brown/.config-emacs
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;  Low-Contrast Theme
;;
;;; Code:


(imp:require :innit 'theme)


;;------------------------------------------------------------------------------
;; Zenburn Theme Tweaks for Org-Mode
;;------------------------------------------------------------------------------

;; Run this after both `zenburn' and `org-mode' have loaded.
;; Need elisp from both.
(imp:eval:after '(zenburn org-mode)

  ;; Set up keys as variables from :
  ;;   - `zenburn-default-colors-alist'
  ;;   - `zenburn-override-colors-alist'
  (zenburn-with-color-variables

   ;;---------------------------------------------------------------------------
   ;; Faces
   ;;---------------------------------------------------------------------------
   ;; I need to customize some org-mode faces - they're not the greatest (IMO)
   ;; in Zenburn. Could maybe move these out of Zenburn into general theme stuff
   ;; if ever other themes want to adjust the same faces? In that case, split
   ;; like:
   ;;
   ;; "some-theme-file.el":
   ;;    (defface mantle:theme:face:org.todo.keyword:todo
   ;;      (list (cons
   ;;             ;; display type
   ;;             t
   ;;             ;; default attributes
   ;;             (list
   ;;              :weight 'bold
   ;;              :inherit 'warning)))
   ;;      "Face for todo keyword in todo sequence."
   ;;      :group 'innit:group)
   ;;
   ;; "zenburn/whatever.el" or "theme-too.el":
   ;;   (face-spec-set 'mantle:theme:face:org.todo.keyword:todo
   ;;                  (list (cons
   ;;                         ;; display type
   ;;                         t
   ;;                         ;; attributes
   ;;                         (list [...]))))
   ;;------------------------------

   ;; HACK: Face specs fed directly to `org-todo-keyword-faces' don't respect
   ;;       underlying faces like the `org-todo' face does, so we define our own
   ;;       intermediary faces that extend from org-todo.

   (defface mantle:theme:face:org.todo.keyword:active
     (list (cons
            ;; display type
            t
            ;; attributes
            '(:inherit (bold font-lock-constant-face org-todo))))
     "Face spec for 'active' todo sequence keywords."
     :group 'innit:group:theme)


   (defface mantle:theme:face:org.todo.keyword:project
     (list (cons
            ;; display type
            t
            ;; attributes
            '(:inherit (bold font-lock-doc-face org-todo))))
     "Face spec for 'project' todo sequence keyword(s)."
     :group 'innit:group:theme)


   (defface mantle:theme:face:org.todo.keyword:holding
     (list (cons
            ;; display type
            t
            ;; attributes
            '(:inherit (bold warning org-todo))))
     "Face spec for 'on-hold' todo sequence keyword(s)."
     :group 'innit:group:theme)


   ;; TODO: Delete?
   ;; (defface mantle:theme:face:org.todo.keyword:cancel
   ;;   (list (cons
   ;;          ;; display type
   ;;          t
   ;;          ;; attributes
   ;;          '(:inherit (bold error org-todo))))
   ;;   "Face spec for 'cancelled'/'killed' todo sequence keyword(s)."
   ;;   :group 'innit:group:theme)


   ;; TODO: Delete?
   ;; (defface mantle:theme:face:org.todo.keyword:background
   ;;   (list (cons
   ;;          ;; display type
   ;;          t
   ;;          ;; attributes
   ;;          ;; nil
   ;;          (list :background zenburn-bg-05)))
   ;;   "Face spec for the background of todo sequence keywords."
   ;;   :group 'innit:group:theme)


   (defface mantle:theme:face:org.todo.keyword:todo
     (list (cons
            ;; display type
            t
            ;; attributes
            ;; (list
            ;;  :weight 'bold
            ;;  :inherit 'warning)
            (list :foreground zenburn-magenta-01
                  :background zenburn-bg-05
                  :weight 'bold)))
     "Face spec for 'todo' keyword in todo sequence."
     :group 'innit:group:theme)


   (defface mantle:theme:face:org.todo.keyword:done/good
     (list (cons
            ;; display type
            t
            ;; attributes
            ;; (list
            ;;  :inherit 'org-done)
            (list :background zenburn-bg-05
                  :inherit 'org-done)))
     "Face spec for good/successful 'done'/'finished' keyword in todo sequence."
     :group 'innit:group:theme)


   (defface mantle:theme:face:org.todo.keyword:done/bad
     (list (cons
            ;; display type
            t
            ;; attributes
            ;; (list
            ;;  :inherit 'org-done)))
            (list :background zenburn-bg-05
                  :foreground zenburn-red-5)))
     "Face spec for bad/failed 'done'/'finished' keyword in todo sequence."
     :group 'innit:group:theme)


   (defface mantle:theme:face:org.todo.keyword:info
     (list (cons
            ;; display type
            t
            ;; attributes
            ;; (list
            ;;  :inherit 'org-done)
            (list :background zenburn-bg-05
                  :foreground zenburn-bg+3)))
     "Face spec for info keyword in todo sequence."
     :group 'innit:group:theme)


   (defface mantle:theme:face:org.todo.keyword:null
     (list (cons
            ;; display type
            t
            ;; attributes
            ;; (list
            ;;  :inherit 'org-done)
            (list :background zenburn-bg-05
                  :foreground zenburn-bg+3)))
     "Face spec for empty/null/spacer keyword in todo sequence."
     :group 'innit:group:theme)


   ;;---------------------------------------------------------------------------
   ;; Configure Org-Mode
   ;;---------------------------------------------------------------------------

   (innit:theme:face:set 'zenburn
     ;;---
     ;; Done states - little less dark.
     ;;---
     (list 'org-done                     :foreground 'zenburn-green-3)
     (list 'org-agenda-done              :foreground 'zenburn-bg+3)
     (list 'org-checkbox-statistics-done :foreground 'zenburn-green-3)
     ;; "Done" Headlines - success/fail/info/etc.
     ;;    - Would be nice to have green, red, and gray... but we only have the one 'done' face.
     ;;    - So a lighter gray?
     (list 'org-headline-done :foreground 'zenburn-fg-05)
     ;; (list 'org-headline-done :foreground 'zenburn-green-3)

     ;;---
     ;; Org Headlines - adjust a few to be less dark.
     ;;---
     ;; Some unique, headliney colors.
     (list 'outline-1 :foreground 'zenburn-orange)
     (list 'outline-2 :foreground 'zenburn-green+2)
     (list 'outline-3 :foreground 'zenburn-blue-1)
     (list 'outline-4 :foreground 'zenburn-red-1)
     ;; ...and repeat.
     (list 'outline-5 :foreground 'zenburn-orange)
     (list 'outline-6 :foreground 'zenburn-green+2)
     (list 'outline-7 :foreground 'zenburn-blue-1)
     (list 'outline-8 :foreground 'zenburn-red-1)
     ;; ...and 9+ starts over at `outline-1'.

     ;;---
     ;; Org TODO States
     ;;---
     ;; ├CURRENT┤
     (list 'mantle:theme:face:org.todo.keyword:active
           :foreground 'zenburn-violet
           :background 'zenburn-bg-05)
     ;; ├WAITING┤, ├HOLDING┤
     (list 'mantle:theme:face:org.todo.keyword:holding
           :foreground 'zenburn-magenta-03
           :background 'zenburn-bg-05)
     ;; ├PROJECT┤
     (list 'mantle:theme:face:org.todo.keyword:project
           :foreground 'zenburn-blue-3
           :background 'zenburn-bg-05)

     ;;---
     ;; Org ~inline code~ and =inline verbatim=
     ;;---
     ;; Want these to be different enough from all the outline levels.
     ;;   - `org-code' was exactly the same as `outline-1'.
     ;;   - `org-verbatim' was similar to `outline-2' and to code comments (green).
     ;; Change to inheriting from `org-cite', a teal affair, with some background color, maybe.
     ;; TODO: Make the foreground color of these slightly different from each other.
     (list 'org-code     :inherit 'org-cite :background 'zenburn-bg+05)
     (list 'org-verbatim :inherit 'org-cite :background 'zenburn-bg-05)

     ;;---
     ;; "#+DOC_KEYWORD" - needs to be slightly lighter.
     ;;---
     (list 'org-document-info-keyword :foreground 'zenburn-bg+3))
  ;; mantle:theme:face:org.todo.keyword:active  - bold green
  ;; mantle:theme:face:org.todo.keyword:holding  - orangey
  ;; mantle:theme:face:org.todo.keyword:project - darker green than bold green
  ;; org-checkbox
  ;; org-checkbox-statistics-todo
  ;; org-headline-todo
  ;; org-todo


   ;; TODO:
   ;; TODO:
   ;; TODO: YOU ARE HERE!
   ;; TODO:
   ;; TODO:


  ;;  ;;------------------------------
  ;;  ;; TODO Sequence Keywords
  ;;  ;;------------------------------
  ;;  ;; Adjust sequences to be more nicer...
  ;;  ;;   - Enforce all to be the same width.
  ;;  ;;   - Make them stand out from their surroundings better.
  ;;  ;;     - This includes making sure none of them are similar colors to the headline colors.

  ;; (let ((wrap "├─┤")
  ;;       ;; (wrap "[ ]")
  ;;       ;; (wrap "「 」")
  ;;       )
  ;;   (setq org-todo-keywords
  ;;         `((sequence  ;; Big Words sequence.
  ;;            ,(sss:org/todo.keyword "TODO"    wrap "t")  ; A task that needs doing & is ready to do
  ;;            ,(sss:org/todo.keyword "PROJECT" wrap "p")  ; A project, which usually contains other tasks
  ;;            ,(sss:org/todo.keyword "CURRENT" wrap "c" 'timestamp)  ; A task that is in progress
  ;;            ,(sss:org/todo.keyword "WAITING" wrap "w" 'timestamp)  ; Something external is holding up this task
  ;;            ,(sss:org/todo.keyword "HOLDING" wrap "h" 'timestamp)  ; This task is paused/on hold because of me
  ;;            "|"
  ;;            ,(sss:org/todo.keyword "───────" wrap "n" 'timestamp) ; No one cares.
  ;;            ,(sss:org/todo.keyword "INFO"    wrap "i" 'timestamp) ; Info.
  ;;            ,(sss:org/todo.keyword "MEETING" wrap "e" 'timestamp) ; Meeting Notes.
  ;;            ,(sss:org/todo.keyword "MOVED"   wrap "m" 'timestamp) ; Moved somewhere else; no further action here.
  ;;            ,(sss:org/todo.keyword "DONE"    wrap "d" 'timestamp) ; Task completed... whatever.
  ;;            ,(sss:org/todo.keyword "SUCCESS" wrap "s" 'notes)     ; Task completed successfully!!!
  ;;            ,(sss:org/todo.keyword "FAILURE" wrap "f" 'notes)     ; Task was completed the bad way.
  ;;            ,(sss:org/todo.keyword "KILLED"  wrap "k" 'notes))    ; Task was cancelled, aborted, or is no longer applicable.
  ;;           (sequence ;; Checkboxes sequence.
  ;;            ,(sss:org/todo.keyword "_" wrap "T")    ; A task that needs doing
  ;;            ,(sss:org/todo.keyword "▶" wrap "C" 'timestamp)    ; Task is in progress
  ;;            ;; ,(sss:org/todo.keyword "-" wrap "C" 'timestamp) ; Task is in progress
  ;;            ;; ,(sss:org/todo.keyword "?" wrap "W" 'timestamp) ; Task is being held up or paused
  ;;            ,(sss:org/todo.keyword "…" wrap "W" 'timestamp)    ; Task is being held up or paused
  ;;            ,(sss:org/todo.keyword "⁈" wrap "H" 'timestamp)    ; Task is on hold
  ;;            "|"
  ;;            ,(sss:org/todo.keyword "ⓘ" wrap "I" 'timestamp)    ; Info.
  ;;            ,(sss:org/todo.keyword "♫" wrap "E" 'timestamp)    ; Meeting Notes.
  ;;            ,(sss:org/todo.keyword "∅" wrap "N" 'timestamp)    ; Null/No one cares.
  ;;            ,(sss:org/todo.keyword "☇" wrap "M" 'timestamp)    ; Moved somewhere else; no further action here.
  ;;            ,(sss:org/todo.keyword "X" wrap "D" 'timestamp)    ; Task completed... whatever.
  ;;            ,(sss:org/todo.keyword "X" wrap "S" 'notes)        ; Task completed successfully!
  ;;            ,(sss:org/todo.keyword "✘" wrap "F" 'notes)        ; Task completed the bad way.
  ;;            ,(sss:org/todo.keyword "÷" wrap "K" 'notes)))      ; Task was cancelled, aborted, or is no longer applicable.

  ;;         ;; And set some faces for these. strings.
  ;;         org-todo-keyword-faces
  ;;         (list (list (sss:org/todo.keyword "TODO" wrap)    'mantle:theme:face:org.todo.keyword:todo)
  ;;               (list (sss:org/todo.keyword "PROJECT" wrap) 'mantle:theme:face:org.todo.keyword:project)

  ;;               (list (sss:org/todo.keyword "CURRENT" wrap) 'mantle:theme:face:org.todo.keyword:active)
  ;;               (list (sss:org/todo.keyword "▶" wrap)       'mantle:theme:face:org.todo.keyword:active)

  ;;               (list (sss:org/todo.keyword "WAITING" wrap) 'mantle:theme:face:org.todo.keyword:holding)
  ;;               (list (sss:org/todo.keyword "HOLDING" wrap) 'mantle:theme:face:org.todo.keyword:holding)
  ;;               (list (sss:org/todo.keyword "INFO" wrap)    'mantle:theme:face:org.todo.keyword:info)
  ;;               (list (sss:org/todo.keyword "MEETING" wrap) 'mantle:theme:face:org.todo.keyword:info)
  ;;               (list (sss:org/todo.keyword "ⓘ" wrap)       'mantle:theme:face:org.todo.keyword:info)
  ;;               (list (sss:org/todo.keyword "♫" wrap)       'mantle:theme:face:org.todo.keyword:info)
  ;;               (list (sss:org/todo.keyword "─" wrap)       'mantle:theme:face:org.todo.keyword:null)
  ;;               (list (sss:org/todo.keyword "∅" wrap)       'mantle:theme:face:org.todo.keyword:null)
  ;;               (list (sss:org/todo.keyword "?" wrap)       'mantle:theme:face:org.todo.keyword:holding)
  ;;               (list (sss:org/todo.keyword "…" wrap)       'mantle:theme:face:org.todo.keyword:holding)
  ;;               (list (sss:org/todo.keyword "⁈" wrap)       'mantle:theme:face:org.todo.keyword:holding)

  ;;               (list (sss:org/todo.keyword "MOVED" wrap)   'mantle:theme:face:org.todo.keyword:info)
  ;;               (list (sss:org/todo.keyword "☇" wrap)       'mantle:theme:face:org.todo.keyword:info)
  ;;               (list (sss:org/todo.keyword "DONE" wrap)    'mantle:theme:face:org.todo.keyword:done/good)
  ;;               (list (sss:org/todo.keyword "X" wrap)       'mantle:theme:face:org.todo.keyword:done/good)
  ;;               (list (sss:org/todo.keyword "SUCCESS" wrap) 'mantle:theme:face:org.todo.keyword:done/good)
  ;;               (list (sss:org/todo.keyword "X" wrap)       'mantle:theme:face:org.todo.keyword:done/good)
  ;;               (list (sss:org/todo.keyword "FAILURE" wrap) 'mantle:theme:face:org.todo.keyword:done/bad)
  ;;               (list (sss:org/todo.keyword "✘" wrap)       'mantle:theme:face:org.todo.keyword:done/bad)
  ;;               (list (sss:org/todo.keyword "KILLED" wrap)  'mantle:theme:face:org.todo.keyword:done/bad)
  ;;               (list (sss:org/todo.keyword "÷" wrap)       'mantle:theme:face:org.todo.keyword:done/bad)))

  ;;   ;; I guess this guy is covered by `hl-todo' instead of `org'?
  ;;   ;; (push `(,(sss:org/todo.keyword "TODO" wrap) warning bold) hl-todo-keyword-faces)
  ;;   ;; ...but `hl-todo' cannot do things that start/end with non-letters...
  ;;   ;; So yay.
  ;;   )

   ))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'theme 'zenburn 'org-mode)

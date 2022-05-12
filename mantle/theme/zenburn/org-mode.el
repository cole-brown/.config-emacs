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

   (defface mantle:theme:face:org.todo.keyword:background
     (list (cons
            ;; display type
            t
            ;; attributes
            ;; nil
            (list :background zenburn-bg-05)))
     "Face for the background of todo sequence keywords."
     :group 'innit:group:theme)


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
     "Face for todo keyword in todo sequence."
     :group 'innit:group:theme)


   (defface mantle:theme:face:org.todo.keyword:done.good
     (list (cons
            ;; display type
            t
            ;; attributes
            ;; (list
            ;;  :inherit 'org-done)
            (list :background zenburn-bg-05
                  :inherit 'org-done)))
     "Face for todo keyword in todo sequence."
     :group 'innit:group:theme)


   (defface mantle:theme:face:org.todo.keyword:done.bad
     (list (cons
            ;; display type
            t
            ;; attributes
            ;; (list
            ;;  :inherit 'org-done)))
            (list :background zenburn-bg-05
                  :foreground zenburn-red-5)))
     "Face for todo keyword in todo sequence."
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
     "Face for todo keyword in todo sequence."
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
     "Face for todo keyword in todo sequence."
     :group 'innit:group:theme)


   ;;---------------------------------------------------------------------------
   ;; Configure Org-Mode
   ;;---------------------------------------------------------------------------


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
  ;;         (list (list (sss:org/todo.keyword "TODO" wrap)    'spy:theme.face/org.todo.keyword/todo)
  ;;               (list (sss:org/todo.keyword "PROJECT" wrap) '+org-todo-project)

  ;;               (list (sss:org/todo.keyword "CURRENT" wrap) '+org-todo-active)
  ;;               (list (sss:org/todo.keyword "▶" wrap)       '+org-todo-active)

  ;;               (list (sss:org/todo.keyword "WAITING" wrap) '+org-todo-onhold)
  ;;               (list (sss:org/todo.keyword "HOLDING" wrap) '+org-todo-onhold)
  ;;               (list (sss:org/todo.keyword "INFO" wrap)    'spy:theme.face/org.todo.keyword/info)
  ;;               (list (sss:org/todo.keyword "MEETING" wrap) 'spy:theme.face/org.todo.keyword/info)
  ;;               (list (sss:org/todo.keyword "ⓘ" wrap)       'spy:theme.face/org.todo.keyword/info)
  ;;               (list (sss:org/todo.keyword "♫" wrap)       'spy:theme.face/org.todo.keyword/info)
  ;;               (list (sss:org/todo.keyword "─" wrap)       'spy:theme.face/org.todo.keyword/null)
  ;;               (list (sss:org/todo.keyword "∅" wrap)       'spy:theme.face/org.todo.keyword/null)
  ;;               (list (sss:org/todo.keyword "?" wrap)       '+org-todo-onhold)
  ;;               (list (sss:org/todo.keyword "…" wrap)       '+org-todo-onhold)
  ;;               (list (sss:org/todo.keyword "⁈" wrap)       '+org-todo-onhold)

  ;;               (list (sss:org/todo.keyword "MOVED" wrap)   'spy:theme.face/org.todo.keyword/info)
  ;;               (list (sss:org/todo.keyword "☇" wrap)       'spy:theme.face/org.todo.keyword/info)
  ;;               (list (sss:org/todo.keyword "DONE" wrap)    'spy:theme.face/org.todo.keyword/done.good)
  ;;               (list (sss:org/todo.keyword "X" wrap)       'spy:theme.face/org.todo.keyword/done.good)
  ;;               (list (sss:org/todo.keyword "SUCCESS" wrap) 'spy:theme.face/org.todo.keyword/done.good)
  ;;               (list (sss:org/todo.keyword "X" wrap)       'spy:theme.face/org.todo.keyword/done.good)
  ;;               (list (sss:org/todo.keyword "FAILURE" wrap) 'spy:theme.face/org.todo.keyword/done.bad)
  ;;               (list (sss:org/todo.keyword "✘" wrap)       'spy:theme.face/org.todo.keyword/done.bad)
  ;;               (list (sss:org/todo.keyword "KILLED" wrap)  'spy:theme.face/org.todo.keyword/done.bad)
  ;;               (list (sss:org/todo.keyword "÷" wrap)       'spy:theme.face/org.todo.keyword/done.bad)))

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

;;; cconfig/org-mode.el -*- lexical-binding: t; -*-


;;------------------------------------------------------------------------------
;;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;                        ORG-MODE - The Thing Itself
;;  The Cthulu of Emacs. Required and will drive you insane trying to grok.
;;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;------------------------------------------------------------------------------

(spy/require :spy 'hook 'def)
(spy/require :spy 'buffer 'search)
(spy/require :spy 'buffer 'name)
(spy/require :spy 'path)
(spy/require :spy 'jerky)
(spy/provide :spy 'datetime 'format)


;;------------------------------------------------------------------------------
;; Org-Mode
;;------------------------------------------------------------------------------

;;--------------------
;; MUST PROCEED ORG SETUP?
;;--------------------

;; DOOM-NOTE:
;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(if-let (lily (jerky/get "path/lily"))
    ;; Set org to use lily dir if we have it.
    (setq org-directory lily)
  ;; Otherwise not sure... This is fine until something is figured out.
  (setq org-directory "~/org/"))


;;--------------------
;; Org-Mode Set-Up
;;--------------------
(use-package! org
  ;;--------------------
  :init
  ;;--------------------

  ;; Define org-mode hooks.
  (spy/hook/defun org-mode-hook
    '(:name "org/jump-to-now-target"
      :file "doom/config/org-mode.el"
      :docstr "Jump point to \"now\" link, if it's in the first part of the file."
      :quiet t)
    (smd/buffer/search.header "[[--now"))


  ;;--------------------
  :hook
  ;;--------------------

  ;; Connect my hooks up.
  ((org-mode . -s//hook/org/jump-to-now-target))


  ;;--------------------
  :config
  ;;--------------------

  ;;--------------------
  ;; customization
  ;;--------------------

  ;; Doom or someone already sets this to org-directory/"notes.org".
  ;; (org-default-notes-file (spy/path/to-file org-directory "notes.org"))
  ;;   (mis/init/message "config for org vars... <org-startup-folded: %S" org-startup-folded)
  (customize-set-variable 'org-startup-folded t
                          "Change org back to opening a file with all the headers collapsed.")

  (customize-set-variable 'org-log-done t
                          "auto-timestamp when TODOs are turned to DONE state")

  ;; Leave headings to figure out if they want a newline or not.
  ;; But change plain-list-item to not try to be clever - it's annoying more
  ;; than it's helpful.
  (customize-set-variable 'org-blank-before-new-entry
                          '((heading . auto) (plain-list-item . nil))
                          "No auto newline in plain list items.")

  ;; Doom has this set to true already.
  ;; ;; Well structured indentation. Freehand notes/text stay indented to
  ;; ;; headline level.
  ;; (org-startup-indented t "I'm starting to like this. Leave globally on.")
  ;; ;; Note 1: This changes how it /looks/, not how the raw text is formatted.
  ;; ;; Note 2: This also hides leading stars for headlines.

  ;; Adjust sequences to be more nicer...

  (let ((wrap "[]")
        ;; (wrap "「」")
        )
    (setq org-todo-keywords
          `((sequence  ;; Big Words sequence.
             ,(-s//org/todo.keyword "TODO"    wrap "t")  ; A task that needs doing & is ready to do
             ,(-s//org/todo.keyword "PROJECT" wrap "p")  ; A project, which usually contains other tasks
             ,(-s//org/todo.keyword "CURRENT" wrap "c" 'timestamp)  ; A task that is in progress
             ,(-s//org/todo.keyword "WAITING" wrap "w" 'timestamp)  ; Something external is holding up this task
             ,(-s//org/todo.keyword "HOLDING" wrap "h" 'timestamp)  ; This task is paused/on hold because of me
             "|"
             ,(-s//org/todo.keyword "DONE"    wrap "d" 'timestamp)  ; Task completed... whatever.
             ,(-s//org/todo.keyword "SUCCESS" wrap "s" 'notes)  ; Task completed successfully!!!
             ,(-s//org/todo.keyword "FAILURE" wrap "f" 'notes)  ; Task was completed the bad way.
             ,(-s//org/todo.keyword "KILLED"  wrap "k" 'notes)) ; Task was cancelled, aborted, or is no longer applicable.
            (sequence ;; Checkboxes sequence.
             ,(-s//org/todo.keyword " " wrap "T")    ; A task that needs doing
             ,(-s//org/todo.keyword "▶" wrap "C" 'timestamp)    ; Task is in progress
             ;; ,(-s//org/todo.keyword "-" wrap "C" 'timestamp) ; Task is in progress
             ;; ,(-s//org/todo.keyword "?" wrap "W" 'timestamp) ; Task is being held up or paused
             ,(-s//org/todo.keyword "…" wrap "W" 'timestamp)    ; Task is being held up or paused
             ,(-s//org/todo.keyword "⁈" wrap "H" 'timestamp)    ; Task is on hold
             "|"
             ,(-s//org/todo.keyword "X" wrap "D" 'timestamp)    ; Task completed... whatever.
             ,(-s//org/todo.keyword "X" wrap "S" 'notes)        ; Task completed successfully!
             ,(-s//org/todo.keyword "✘" wrap "F" 'notes)        ; Task completed the bad way.
             ,(-s//org/todo.keyword "÷" wrap "K" 'notes)))      ; Task was cancelled, aborted, or is no longer applicable.

          ;; And set some faces for these. strings.
          org-todo-keyword-faces
          (list (list (-s//org/todo.keyword "TODO" wrap) 'warning 'bold)
                (cons (-s//org/todo.keyword "PROJECT" wrap) '+org-todo-project)

                (cons (-s//org/todo.keyword "CURRENT" wrap) '+org-todo-active)
                (cons (-s//org/todo.keyword "▶" wrap)       '+org-todo-active)

                (cons (-s//org/todo.keyword "WAITING" wrap) '+org-todo-onhold)
                (cons (-s//org/todo.keyword "HOLDING" wrap) '+org-todo-onhold)
                (cons (-s//org/todo.keyword "?" wrap)       '+org-todo-onhold)
                (cons (-s//org/todo.keyword "…" wrap)       '+org-todo-onhold)
                (cons (-s//org/todo.keyword "⁈" wrap)       '+org-todo-onhold)

                (cons (-s//org/todo.keyword "DONE" wrap)    'org-done)
                (cons (-s//org/todo.keyword "X" wrap)       'org-done)
                (cons (-s//org/todo.keyword "SUCCESS" wrap) 'org-done)
                (cons (-s//org/todo.keyword "X" wrap)       'org-done)
                (cons (-s//org/todo.keyword "FAILURE" wrap) 'org-done)
                (cons (-s//org/todo.keyword "✘" wrap)       'org-done)
                (cons (-s//org/todo.keyword "KILLED" wrap)  'org-done)
                (cons (-s//org/todo.keyword "÷" wrap)       'org-done)))

    ;; I guess this guy is covered by `hl-todo' instead of `org'?
    ;; (push `(,(-s//org/todo.keyword "TODO" wrap) warning bold) hl-todo-keyword-faces)
    ;; ...but `hl-todo' cannot do things that start/end with non-letters...
    ;; So yay.
    )

  ;; Old sequences:
  ;; ;; TODO sequence to pop up shortcuts to on running `C-c C-t' on a headline
  ;; ;;   (n) - n key will be shortcut into this state
  ;; ;;    @  - timestamp on enter
  ;; ;;    !  - prompt for note w/ timestamp on enter
  ;; ;;   /!  - prompt for note w/ timestamp on exit if none for next state
  ;; ;;    |  - separates "need action" states from "finished" states
  ;; (org-todo-keywords
  ;;  '((sequence "TODO(t)"
  ;;              "STARTED(s!)"
  ;;              "WAITING(w@/!)"
  ;;              "|"
  ;;              ;; Done/cancelled for task progression.
  ;;              "DONE(d!)"
  ;;              "CANCELLED(c@)"
  ;;              ;; Failure/success for bug hunt or investigations.
  ;;              ;; E.g.: Where you're done exploring a possible solution thing,
  ;;              ;; but it's a dead path that shouldn't be paid attention to:
  ;;              ;; mark it a FAILURE.
  ;;              "FAILURE(f@)"
  ;;              "SUCCESS(.@)"))
  ;;  "Custom sequence of keywords & actions.")

  ;; `Drawer' to log to. (Property/subheading thing). "LOGBOOK" is default if
  ;; this is set to `t'. This is for the place org-mode puts those todo
  ;; timestamps, state change notes, and user notes just under the headline.
  (customize-set-variable 'org-log-into-drawer "LOGBOOK"
                          "See my comment or emacs help for more details.")

  ;; Trying out 'smart for invisible edits...
  ;; ;; Don't allow accidental edits of invisible regions in org files.
  ;; ;; https://yiufung.net/post/org-mode-hidden-gems-pt1/
  ;; (org-catch-invisible-edits 'show-and-error
  ;;                            "Never allow edits of invisible regions.")

  ;; Hide extra newlines between (sub)trees.
  ;; https://yiufung.net/post/org-mode-hidden-gems-pt1/
  ;; Really useful because I tend to like the bonus whitespace for visually
  ;; separating one tree from the next...
  (customize-set-variable 'org-cycle-separator-lines 0
                          "Hide extra newlines between (sub)trees")

  ;; Hide extra newlines between (sub)trees.
  ;; https://yiufung.net/post/org-mode-hidden-gems-pt1/
  ;; Really useful because I tend to like the bonus whitespace for visually
  ;; separating one tree from the next...
  (customize-set-variable 'org-cycle-separator-lines 0
                          "Hide /all/ extra newlines between (sub)trees.")

  ;; Doom already has good abbrevs.
  ;; ;; [[link:tag]] becomes something else.
  ;; ;; e.g.: [[google:test]] becomes link:
  ;; ;;       'https://www.google.com/search?q=test' when clicked
  ;; ;;   - '%s' in link-alist replaced with link's 'tag'
  ;; ;;   - '%h' in link-alist replaced with link's (html encoded) 'tag'
  ;; ;; https://yiufung.net/post/org-mode-hidden-gems-pt4/
  ;; (org-link-abbrev-alist
  ;;  '(("google" . "https://www.google.com/search?q=%h")
  ;;    ("map" . "https://maps.google.com/maps?q=%h"))
  ;; "Shortcuts for links. Translates [[link:tag]] (and [[link:tag][desc]]).")

  ;; TODO: Not sure if works well with evil.
  ;; ;; Enable Speed Keys as per my speed-commands predicate function.
  ;; (org-use-speed-commands
  ;;  #'spy/custom/org-mode/speed-commands-p
  ;;  "Allow speed keys when at any headline *, not just beginning of line.")


  ;;--------------------
  ;; configuration
  ;;--------------------

  ;; Put .org.txt into the mode list for org-mode. Useful for org-mode files in
  ;; dropbox - dropbox website/app doesn't know how to read ".org", but it can
  ;; do ".txt".
  (add-to-list 'auto-mode-alist '("\\.org.txt$" . org-mode))

  ;; TODO: whitespace-mode fix for org-mode... still needed?
  ;;   ;; This does double the work on the org-indent-strings array, but meh.
  ;;   (require 'cl-lib)
  ;;   (defun spy/advice/org-indent/prefix-munger ()
  ;;     "Initialize the indentation strings so the motherfucking
  ;; `org-indent-boundary-char' is set with a proper face you god damn
  ;; savages."
  ;;     (setq org-indent--text-line-prefixes
  ;;           (make-vector org-indent--deepest-level nil))
  ;;     (dotimes (n org-indent--deepest-level)
  ;;       (let ((indentation (if (<= n 1) 0
  ;;                            (* (1- org-indent-indentation-per-level)
  ;;                               (1- n)))))
  ;;         ;; Text line prefixes.
  ;;         (aset org-indent--text-line-prefixes
  ;;               n
  ;;               ;; spy change: concat org-indent-boundary-char inside
  ;;               ;;   of org-add-props, not outside.
  ;;               (org-add-props (concat
  ;;                               (make-string (+ n indentation) ?\s)
  ;;                               (and (> n 0)
  ;;                                    (char-to-string org-indent-boundary-char)))
  ;;                   nil 'face 'org-indent)
  ;;               ))))
  ;;   (advice-add 'org-indent--compute-prefixes
  ;;               :after #'spy/advice/org-indent/prefix-munger)

  ;; Map some things for org-mode.
  (map! :after org
        :map org-mode-map
        :localleader
        ;; :map evil-org-mode-map
        :prefix "l" ;; links
        :desc "Link as 'here'." "h" #'smd/org/here.link
        :desc "Paste as 'here' link." "p" #'smd/org/here.yank)

  ;; TODO: Do I want this again?
  ;; ;; 'C-c <tab>' to show headings only (no top parent notes, no
  ;; ;; children notes, just headings). Org-Mode had 'C-c <tab>' as
  ;; ;; outline-show-children, which only shows direct children
  ;; ;; headings, not all descendants' headings.
  ;; ;; https://yiufung.net/post/org-mode-hidden-gems-pt1/
  ;; ("C-c <tab>" . #'org-kill-note-or-show-branches)

  ;; TODO: If/when Zenburn, see if I want this again:
  ;;
  ;; ;; There is a `:custom-face' section of use-packge, but I don't think I can do
  ;; ;; the zenburn feature check or the `zenburn-with-color-variables' call.
  ;; ;;
  ;; ;; Change some headline colors.
  ;; (require 'with)
  ;; (with-feature 'zenburn-theme
  ;;   (zenburn-with-color-variables
  ;;     ;; I don't like green in these as it confuses me with the "DONE" etc
  ;;     ;; flags, and I apparently like having my level 2 headings set to DONE and
  ;;     ;; it was exactly the same color as far as my eyes could tell.
  ;;     (set-face-foreground 'org-level-1 zenburn-orange)
  ;;     (set-face-foreground 'org-level-2 zenburn-blue-1)
  ;;     (set-face-foreground 'org-level-3 zenburn-yellow-2)
  ;;     (set-face-foreground 'org-level-4 zenburn-cyan)
  ;;     (set-face-foreground 'org-level-5 zenburn-red-4)
  ;;     (set-face-foreground 'org-level-6 zenburn-blue-4)
  ;;     ;; these get a bit weird but we're really super deeper than I've been
  ;;     (set-face-foreground 'org-level-7 zenburn-cyan)
  ;;     (set-face-foreground 'org-level-8 zenburn-yellow-2)
  ;;     ;; and after 8 it repeats from 1
  ;;     )
  )


;;---------------------
;; TODO: Do these work well with Evil?
;;---------------------

;;----------
;; Speed Keys
;;----------
;;  ;;   "Enable Speed Keys, which allows quick single-key commands when the
;;  ;; cursor is placed on a heading. Usually the cursor needs to be at the
;;  ;; beginning of a headline line, but defining it with this function makes them
;;  ;; active on any of the asterisks at the beginning of the line (useful with
;;  ;; the font highlighting I use, as all but the last asterisk are sometimes not
;;  ;; visible)."
;;  ;;   https://zzamboni.org/post/my-emacs-configuration-with-commentary/
;;  ;; Manual:
;;  ;;   https://orgmode.org/manual/Speed-keys.html
;;  (defun spy/custom/org-mode/speed-commands-p ()
;;    "Allow speed keys when at any headline *, not just beginning of line."
;;    (and (looking-at org-outline-regexp) (looking-back "^\**")))


;;------------------------------------------------------------------------------
;; Org-Journal
;;------------------------------------------------------------------------------

(use-package! org-journal
  :after org

  ;;--------------------
  :init
  ;;--------------------

  ;;---
  ;; "Home" Domain
  ;;---
  (jerky/set 'org-journal 'file 'format
             :namespace :home
             :value (concat (spy/datetime/format.get 'iso-8601 'short)
                            ;; TODO: 'notebook' not quickest to
                            ;; auto-complete to. Find better.
                            ".notebook.org")
             :docstr "`org-journal-file-format' for :home")

  ;;---
  ;; "Work" Domain
  ;;---
  (jerky/set 'org-journal 'file 'format
             :namespace :work
             :value (concat (spy/datetime/format.get 'iso-8601 'short)
                            ;; TODO: 'logbook' not quickest to
                            ;; auto-complete to. Find better.
                            ".logbook.org")
             :docstr "`org-journal-file-format' for :work")

  ;;---
  ;; Domain Switcher
  ;;---
  (defmacro -s//org.journal/namespaced (namespace &rest body)
    "Sets (lexical context) all org-journal custom vars related to NAMESPACE. Then runs BODY."
    `(let ((org-journal-file-format ,(jerky/get 'org-journal 'file 'format
                                                :namespace namespace))
           (org-journal-dir ,(jerky/get 'path 'org 'journal
                                        :namespace namespace)))
       ,@body
       ))
  ;; (-s//org.journal/namespaced :home (message "%s %s" org-journal-file-format org-journal-dir))
  ;; (-s//org.journal/namespaced :work (message "%s %s" org-journal-file-format org-journal-dir))


  ;; ;;--------------------
  ;; :custom
  ;; ;;--------------------
  ;; Doom doesn't fucking bother with `:custom' section, so they all have to go
  ;; in `:config'. *sigh*


  ;;--------------------
  :config
  ;;--------------------

  ;;--------------------
  ;; customization
  ;;--------------------

  ;; This is only the path to the default namespace, so multi-namespace
  ;; shenanigans have to constantly mess with this, I think?
  (customize-set-variable 'org-journal-dir
                          (jerky/get 'path 'org 'journal
                                     :namespace (jerky/get 'namespace 'system)))

  ;; Tack day name onto our format for the org-journal headline.
  (customize-set-variable 'org-journal-date-format
                          (concat (spy/datetime/format.get 'iso-8601 'short)
                                  ", %A"))
  ;; This can be a function if more is wanted. E.g. inserting new header text
  ;; into empty files.
  ;;  - https://github.com/bastibe/org-journal#journal-file-content

  ;; A year per file. Could do monthly if too big. Weekly and daily are also
  ;; options. Daily is the default.
  ;; [2019-12-03] - A month per file. Year is getting too big...
  (customize-set-variable 'org-journal-file-type 'monthly)

  ;; org-journal-file-format: Make it a bit more ISO-ish (yyyy-mm-dd).
  ;;   - default:   yyyymmdd
  ;;   - better:    yyyy-mm-dd.org
  ;; But those both are difficult to switch to when various other buffers open,
  ;; so we'll go to this:
  ;;   - betterer:  yyyy-mm-dd.journal.org
  (customize-set-variable 'org-journal-file-format
                          (jerky/get 'org-journal 'file 'format
                                     :namespace (jerky/get 'namespace 'system)))

  ;;--------------------
  ;; configuration
  ;;--------------------


  (when (jerky/namespace/has :work)
    ;; Add `:home' namespaced org-journal stuff.
    (when (featurep! :lang org +journal)
      ;; Insert :work dir local variable(s).
      (jerky/dlv/set nil
                     (jerky/get 'path 'org 'journal :namespace :work)
                     'org-journal-mode
                     'org-journal-dir
                     :namespace :work
                     :value (jerky/get 'path 'org 'journal :namespace :work)
                     :docstr "org-journal's :work directory"
                     :dlv 'full)
      ;; (jerky/dlv/set nil
      ;;                (jerky/get 'path 'taskspace :namespace :work)
      ;;                'taskspace-mode
      ;;                'org-journal-dir
      ;;                :namespace :work
      ;;                :value (jerky/get 'path 'org 'journal :namespace :work)
      ;;                :docstr "org-journal's :work directory"
      ;;                :dlv 'full)

      ;; Insert :work journal shortcuts if appropriate.
      ;; Add to Doom Leader...
      (map! :leader
            ;; :normal, :visual states of evil
            ;; (not :motion, :emacs, :insert, :operator-pending)
            (:prefix "n" ;; notes
             (:prefix "j" ;; ("j" . "journal")
              ;; Work namespaced commands.
              (:prefix ("w" . ":work journal")

               :desc ":work - New Entry"           "j" (cmd!
                                                        (-s//org.journal/namespaced
                                                         :work
                                                         (funcall-interactively #'org-journal-new-entry current-prefix-arg)))
               :desc ":work - New Scheduled Entry" "J" (cmd!
                                                        (-s//org.journal/namespaced
                                                         :work
                                                         (funcall-interactively #'org-journal-new-scheduled-entry current-prefix-arg)))
               :desc ":work - Visit Journal"       "v" (cmd!
                                                        (-s//org.journal/namespaced
                                                         :work
                                                         (funcall-interactively #'org-journal-open-current-journal-file)))
               :desc ":work - Search Forever"      "s" (cmd!
                                                        (-s//org.journal/namespaced
                                                         :work
                                                         (funcall-interactively #'org-journal-search-forever nil)))))))))

  ;; Insert :home journal shortcuts if appropriate.
  (when (jerky/namespace/has :home)
    ;; Add `:home' namespaced org-journal stuff.
    (when (featurep! :lang org +journal)
      ;; Insert :home dir local variable(s).
      (jerky/dlv/set nil
                     (jerky/get 'path 'org 'journal :namespace :home)
                     'org-journal-mode
                     'org-journal-dir
                     :namespace :home
                     :value (jerky/get 'path 'org 'journal :namespace :home)
                     :docstr "org-journal's :home directory"
                     :dlv 'full)

      ;; Insert :home journal shortcuts if appropriate.
      ;; Add to Doom Leader...
      (map! :leader
            ;; :normal, :visual states of evil
            ;; (not :motion, :emacs, :insert, :operator-pending)
            (:prefix "n" ;; notes
             (:prefix "j" ;; journal
              ;; Home namespaced commands.
              (:prefix ("h" . ":home journal")
               :desc ":home - New Entry"           "j" (cmd!
                                                        (-s//org.journal/namespaced
                                                         :home
                                                         (funcall-interactively #'org-journal-new-entry current-prefix-arg)))
               :desc ":home - New Scheduled Entry" "J" (cmd!
                                                        (-s//org.journal/namespaced
                                                         :home
                                                         (funcall-interactively #'org-journal-new-scheduled-entry current-prefix-arg)))
               :desc ":home - Visit Journal"       "v" (cmd!
                                                        (-s//org.journal/namespaced
                                                         :home
                                                         (funcall-interactively #'org-journal-open-current-journal-file)))
               :desc ":home - Search Forever"      "s" (cmd!
                                                        (-s//org.journal/namespaced
                                                         :home
                                                         (funcall-interactively #'org-journal-search-forever nil)))))))))
  )


;;------------------------------------------------------------------------------
;; Org-Mode's Legion of Minions
;;------------------------------------------------------------------------------

;;---------------------
;; Org-Roam Funcs
;;---------------------
;; Zettelkasten Note-Taking with Org-Mode

(defun -s//org-roam/file-name/timestamp-title (title)
  "Return a file name (without extension) for new files.

It uses TITLE and the current timestamp to form a unique title.
"
  (let ((timestamp (spy/datetime/string.get 'iso-8601 'file))
        (slug (org-roam--title-to-slug title)))
    (format "%s_%s" timestamp slug)))


(defun -s//org-roam/buffer/deactivate ()
  "Like `org-roam-buffer-deactivate' but don't delete the window."
  (interactive)

  (setq org-roam-last-window (get-buffer-window))
  ;; (delete-window (get-buffer-window org-roam-buffer)
  ;; (kill-buffer org-roam-buffer)
  )


;;---
;; NOTE!
;;---
;; This must be before Doom's org-roam packages' `use-package' `:config' section!
;; ---
;; Doom's .emacs.d/modules/lang/org/contrib/roam.el wants org-roam to be in a
;; subdirectory of `org-directory', so they do a lot to try to make that happen
;; and the only out is to put this in early...
;;---
;; Every org file within this directory tree root is part of
;; the org-roam ecosystem.
;; So: everything in lily.
(customize-set-variable 'org-roam-directory
                        (jerky/get 'path 'lily))


;;--------------------
;; Org-Roam
;;--------------------
(after! org-roam
  (customize-set-variable 'org-roam-buffer
                          (spy/buffer/special-name "lily" nil :info))

  ;; Doom is a little pesky about keeping the org-roam buffer open.
  ;; It even does it when you close a file and an org buffer is in any visible frame.
  (customize-set-variable '+org-roam-open-buffer-on-find-file
                          nil)

  ;; TODO: Do I want/need these?
  ;; ;; Org-Roam filenames need to be uniquely named, but name doesn't matter much?
  ;; ;; Or so they say... So they can just gen file names from the current time.
  ;; (org-roam-filename-noconfirm t)
  ;; (org-roam-file-name-function #'spy/org-roam/file-name/timestamp-title)

  ;; If 'right isn't desirable:
  ;; (org-roam-position 'left)

  ;; What % of total frame width to use.
  ;; (org-roam-buffer-width 0.4)

  ;; By default, links are inserted with the title as the link description. This
  ;; can make them hard to distinguish from external links. If you wish, you may
  ;; choose add special indicators for Org-roam links by tweaking
  ;; this, for example:
  (customize-set-variable 'org-roam-link-title-format "r://%s")

  ;; `right' (default) is probably correct? But trying out `bottom'.
  (customize-set-variable 'org-roam-buffer-position 'bottom)

  (map! :leader
        :prefix "nr" ;; notes -> roam
        :desc "Kill roam info buffer" "K" #'-s//org-roam/buffer/deactivate
        :desc "Delete roam info window" "k" #'org-roam-buffer-deactivate)
  (map! :after org
        :map org-mode-map
        :localleader
        :prefix "m" ;; org-roam is... 'm' here instead of 'r'.
        ;; and copy the above 'map!':
        :desc "Kill roam info buffer" "K" #'-s//org-roam/buffer/deactivate
        :desc "Delete roam info window" "k" #'org-roam-buffer-deactivate))


;; TODO: This probably slows stuff down too much, yeah? :(
;; ;;---------------------
;; ;; Org-Mode Headline Bullets: (Making Org-Mode Pretty)
;; ;;---------------------
;;
;; ;; Display the titles with nice unicode bullets instead of the text ones.
;; (use-package org-bullets
;;   :disabled ;; is this making things slow?
;;   :after org
;;   :demand t
;;   :hook (org-mode . org-bullets-mode)
;;
;;   :custom
;;   (org-bullets-bullet-list
;;    ;; default: '("◉" "○" "✸" "✿")
;;    '("◆" "♦" "●" "○" "►" "▸")
;;     ;; ♥ ● ◇ ✚ ✜ ☯ ◆ ♠ ♣ ♦ ☢ ❀ ◆ ◖ ▶
;;     ;;; Small
;;     ;; ► • ★ ▸
;;     )
;;   )


;; TODO: Get org-contacts actually working like it never really was on vanilla?
;;
;; ;;---------------------
;; ;; Org Contacts
;; ;;---------------------
;; (use-package org-contacts
;;   :disabled ;; is this making things slow?
;;   :ensure nil
;;   :after org
;;   :demand t
;;
;;   :custom
;;   (org-contacts-files (spy/path/to-file
;;                        (spy/dirky/path :secrets :secrets/org)
;;                        "contacts.org"))
;;
;;   :config
;;   ;; §-TODO-§ [2019-10-14]: get Org-Contacts working...
;;   ;; https://github.com/tkf/org-mode/blob/master/contrib/lisp/org-contacts.el
;;   (add-to-list 'org-capture-templates
;;                `("c" "Contacts" entry
;;                   (file+headline org-contacts-files
;;                                  "INBOX")
;;                   ,(concat "* %(org-contacts-template-name)\n"
;;                            ":PROPERTIES:\n"
;;                            ":EMAIL: %(org-contacts-template-email)\n"
;;                            ":END:\n"))))


;;------------------------------------------------------------------------------
;; WANT!, but... slow. :(
;;------------------------------------------------------------------------------

;;----------
;; Pretty Checkboxes in Unicode
;;----------
;;  ;; Check box visual upgrade.
;;  ;;   empty, indeterminate, marked: [ ], [-], [X] -> ☐, ▣, ☒
;;  ;;     aka 'unchecked', 'mixed checked/unchecked children', 'checked'
;;  ;; Trial: [2019-07-30 Tue]
;;  ;;   - Con: This doesn't update until point has moved off the line... Possibly
;;  ;;     interacting with my highlight row thing/mode?
;;  ;; Nice lil search for symbols: http://www.unicode.org/charts/
;;  (spy/hook/defun org-mode-hook t
;;      nil "checkboxes" "init/config/configure-org-mode.el"
;;    "Beautify Org Checkbox Symbol"
;;    (setq prettify-symbols-alist
;;          '(("[ ]" . "☐")
;;            ;; other options:
;;            ;;   - ☐ - 2610 ballot box
;;            ;;     https://www.unicode.org/charts/PDF/U2600.pdf
;;            ("[X]" . "☒")
;;            ;; other options:
;;            ;;   - ☒ - 2612 ballot box with X
;;            ;;     https://www.unicode.org/charts/PDF/U2600.pdf
;;            ;;   - ☑ - 2611 ballot box with check
;;            ("[-]" . "▣")
;;            ;; other options:
;;            ;;   - ▣ - 25A3 white square containing black small square
;;            ;;     https://www.unicode.org/charts/PDF/U25A0.pdf
;;            ;;   - ❍ - ...idk, what other people used at reddit thread.
;;            ;;   - ▽ - 25BD white down-pointing triangle
;;            ;;   - ◎ - 25CE BULLSEYE
;;            ;;   - ☯ - 262F yin-yang
;;            ))
;;    (prettify-symbols-mode 1))

;;----------
;; Pretty List Bullet in Unicode
;;----------
;;  ;; Show list markers with a middle dot instead of the
;;  ;; original character.
;;  (spy/hook/defun org-mode-hook t
;;      nil "simple-list" "init/config/configure-org-mode.el"
;;    "Nice up simple lists - replacing hypen with a unicode middle dot."
;;    (font-lock-add-keywords
;;     nil ;; 'org-mode - some org-mode stuff (e.g. org-journal) is a derived
;;         ;; major mode and thus needed either more than just `org-mode', or to
;;         ;; be `nil' and put in the org-mode hook.
;;     '(("^ *\\([-]\\) "
;;        (0 (prog1 () (compose-region (match-beginning 1)
;;                                     (match-end 1) "•")))))))

;;; mantle/config/org/mode.el --- Configure Org-Mode -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2022-06-02
;; Modified:   2023-03-16
;; URL:        https://github.com/cole-brown/.config-emacs
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;  Configure Org-Mode.
;;
;;; Code:


;;------------------------------------------------------------------------------
;;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;                        ORG-MODE - The Thing Itself
;;  The Cthulu of Emacs. Required and will drive you insane trying to grok.
;;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;------------------------------------------------------------------------------

(imp:require :nub)
(imp:require :datetime)
(imp:require :innit)
(imp:require :jerky)
(imp:require :path)
(imp:require :buffer 'search)
(imp:require :buffer 'name)
(imp:require :mode 'org)
(imp:require :template)

;;------------------------------------------------------------------------------
;; NOTE: Org-Mode TODO Sequence Keywords
;;------------------------------------------------------------------------------
;; See "mantle/theme/<theme>/org-mode.el" (e.g.
;; "mantle/theme/zenburn/org-mode.el") for set-up of org-mode TODO keywords,
;; faces, and other such theme-related stuff.


;;------------------------------------------------------------------------------
;; Org-Mode
;;------------------------------------------------------------------------------

;;------------------------------
;; Org-Mode Set-Up
;;------------------------------
(imp:use-package org
  ;;------------------------------
  :init
  ;;------------------------------

  ;;---
  ;; Non-interactive Snippets
  ;;---
  (template:define :src 'org-mode
    "#+begin_src %s" ; language
    "%s"             ; yank/region
    "#+end_src"
    "")

  (template:define :quote 'org-mode
    "#+begin_quote"
    "%s"            ; yank/region
    "#+end_quote"
    "")


  ;;---
  ;; Create Org-Mode Hooks
  ;;---
  (innit:hook:defun
      (:name    "org/jump-to-now-target"
       :docstr  "Jump point to \"now\" link, if it's in the first part of the file."
       :squelch t)
    (when-let ((location (buffer:search:header "[[--now")))
      (goto-char location)))

  (innit:hook:defun
      (:name    "org/local-settings"
       :docstr  "Set up buffer local vars."
       :squelch t)
    (setq-local yas-indent-line 'auto)

    ;; Automatically becomes buffer local.
    (setq tab-width (jerky:get 'docs 'tab 'short))

    ;; Auto-indentation is more annoying than useful in org, I think.
    (electric-indent-local-mode -1))


  ;;------------------------------
  :hook
  ;;------------------------------

  ((org-mode-hook . mantle:hook:org/jump-to-now-target) ;; (innit:hook:func/name:symbol "org/jump-to-now-target" nil)
   (org-mode-hook . mantle:hook:org/local-settings))    ;; (innit:hook:func/name:symbol "org/local-settings"     nil)

  ;;------------------------------
  :custom
  ;;------------------------------

  ;; Doom or someone already sets this to org-directory/"notes.org".
  ;; (org-default-notes-file (path:abs:file org-directory "notes.org"))
  ;;   (mis0/init/message "config for org vars... <org-startup-folded: %S" org-startup-folded)
  (org-startup-folded t
                      "Change org back to opening a file with all the headers collapsed.")

  (org-log-done t
                "auto-timestamp when TODOs are turned to DONE state")

  ;; Leave headings to figure out if they want a newline or not.
  ;; But change plain-list-item to not try to be clever - it's annoying more
  ;; than it's helpful.
  (org-blank-before-new-entry
   '((heading . auto)
     (plain-list-item . nil))
   "No auto newline in plain list items.")

  ;; Well structured indentation. Freehand notes/text stay indented to
  ;; headline level.
  (org-startup-indented t)
  ;; Note 1: This changes how it /looks/, not how the raw text is formatted.
  ;; Note 2: This also hides leading stars for headlines.

  ;; `Drawer' to log to. (Property/subheading thing). "LOGBOOK" is default if
  ;; this is set to `t'. This is for the place org-mode puts those todo
  ;; timestamps, state change notes, and user notes just under the headline.
  (org-log-into-drawer "LOGBOOK"
                       "See my comment or emacs help for more details.")

  ;; Don't allow accidental edits of invisible regions in org files.
  ;; https://yiufung.net/post/org-mode-hidden-gems-pt1/
  (org-catch-invisible-edits 'show-and-error ;; Doom had `smart'...
                             "Never allow edits of invisible regions.")

  ;; Hide extra newlines between (sub)trees.
  ;; https://yiufung.net/post/org-mode-hidden-gems-pt1/
  ;; Really useful because I tend to like the bonus whitespace for visually
  ;; separating one tree from the next...
  (org-cycle-separator-lines 0
                             "Hide extra newlines between (sub)trees")

  ;; [[link:tag]] becomes something else.
  ;; e.g.: [[google:test]] becomes link:
  ;;       'https://www.google.com/search?q=test' when clicked
  ;;   - '%s' in link-alist replaced with link's 'tag'
  ;;   - '%h' in link-alist replaced with link's (html encoded) 'tag'
  ;; https://yiufung.net/post/org-mode-hidden-gems-pt4/
  (org-link-abbrev-alist
   '(("google" . "https://www.google.com/search?q=%h")
     ("map"    . "https://maps.google.com/maps?q=%h")
     ("image"  . "https://google.com/images?q=%s")
     ;; Stolen from Doom:
     ("wolfram" . "https://wolframalpha.com/input/?i=%s")
     ("wikipedia" . "https://en.wikipedia.org/wiki/%s")
     ("duckduckgo" . "https://duckduckgo.com/?q=%s")
     ("youtube" . "https://youtube.com/watch?v=%s")
     ("github" . "https://github.com/%s"))
   "Shortcuts for links. Translates [[link:tag]] (and [[link:tag][desc]]) into searches.")

  ;; TODO-meow: Not sure if works well with evil/meow.
  ;; ;; Enable Speed Keys as per my speed-commands predicate function.
  ;; (org-use-speed-commands
  ;;  #'mode:org:speed-commands?
  ;;  "Allow speed keys when at any headline *, not just beginning of line.")

  (org-indent-indentation-per-level
   (jerky:get 'docs 'tab 'short)
   "Set indent to tab-width.")

  ;; Not sure exactly what this is, since Doom sets
  ;; `org-src-preserve-indentation' to t and the documentation for this variable
  ;; isn't super clear. I think it indents /everything/ inside a src block,
  ;; maybe? Or maybe it's only related to exported stuff?
  ;;
  ;; Oh. The correct explanation for /this/ variable is in the
  ;; `org-src-preserve-indentation' variable's documentation. Because of course
  ;; it is. :eyeroll:
  ;;
  ;;   > When this variable is nil, after editing with M-x org-edit-src-code,
  ;;   > the minimum (across-lines) number of leading whitespace characters
  ;;   > are removed from all lines, and the code block is uniformly indented
  ;;   > according to the value of org-edit-src-content-indentation.
  ;;
  ;; So yeah, doesn't really matter.
  ;; (org-edit-src-content-indentation
  ;;  (jerky:get 'code 'tab 'standard)
  ;;  "Set indent to tab-width.")

  (org-src-preserve-indentation t)


  ;;------------------------------
  :config
  ;;------------------------------

  ;;------------------------------
  ;; conditional customization: org-agenda
  ;;------------------------------

  (when-let ((agenda-files (jerky:get 'path 'org 'agenda)))
    (innit:customize-set-variable org-agenda-files
                            agenda-files
                            "My paths to search for agenda items."))

  ;;---
  ;; Extra Aliases for Org-Mode Source Blocks
  ;;---
  ;; (push '("shell" . sh) org-src-lang-modes) ; There's already a `shell-mode'; which is different from `sh-mode'.
  (push '("C#"           . csharp)      org-src-lang-modes)
  ;; Various "Not a real language/mode but..." aliases:
  (push '("console"      . fundamental) org-src-lang-modes) ; shell/console output (not input!)
  (push '("browser"      . fundamental) org-src-lang-modes) ; Browser web page output
  (push '("output"       . fundamental) org-src-lang-modes)
  (push '("build-output" . fundamental) org-src-lang-modes)

  ;;------------------------------
  ;; configuration
  ;;------------------------------

  ;; NOTE: Keybinds are in: TODO: WHERE ARE KEYBINDS IN sn-004??? Previously: config/keybinds/org-mode.el

  ;; NOTE: Theme tweaks are in: mantle/theme/zenburn/org-mode.el

  ;; Put '.org.txt' into the mode list for org-mode. Useful for org-mode files
  ;; in dropbox - dropbox website/app doesn't know how to read ".org", but it
  ;; can do ".txt".
  (add-to-list 'auto-mode-alist '("\\.org.txt$" . org-mode))

  ;; TODO: whitespace-mode fix for org-mode... still needed?
  ;;   ;; This does double the work on the org-indent-strings array, but meh.
  ;;   (require 'cl-lib)
  ;;   (defun org:advice/org-indent/prefix-munger ()
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
  ;;               ;; NOTE: changed to concat org-indent-boundary-char inside
  ;;               ;; of org-add-props, not outside.
  ;;               (org-add-props (concat
  ;;                               (make-string (+ n indentation) ?\s)
  ;;                               (and (> n 0)
  ;;                                    (char-to-string org-indent-boundary-char)))
  ;;                   nil 'face 'org-indent)
  ;;               ))))
  ;;   (advice-add 'org-indent--compute-prefixes
  ;;               :after #'org:advice/org-indent/prefix-munger)
  )


;;------------------------------
;; Keybinds : Meow
;;------------------------------

(imp:use-package org
  :when  (imp:flag? :keybinds +meow)
  :after meow

  ;;------------------------------
  :bind ; meow
  ;;------------------------------
  ;; NOTE: `org-mode-map' already has some stuff bound, and they use `<return>'
  ;; (apparently) instead of `RET'? So use both just to make sure these actually
  ;; take hold as the binds.
  (:map org-mode-map
   ;;---
   ;; Return of the +Jedi+ Enter Key:
   ;; Fix how various enter keys work in org.
   ;;---
   ;; 1. "RET" should be a boring, plain, and unsurprising "\n".
   ("RET" . newline)
   ("<return>" . newline)

   ;; 2. Move "newline and maybe indent for me?" to shift.
   ("S-RET" . org-newline-and-indent)
   ("S-<return>" . org-newline-and-indent)

   ;; 3. These are useful in lists to make more list items.
   ;; TODO:keybind:org: make/find a smarter function for:
   ;;   - "create first list item" or
   ;;   - "toggle an empty list item between checkbox or no"
   ;;   - "create another list item, with or without checkbox depending on current list item"
   ("C-RET" . org-insert-todo-heading)      ; TODO heading or a checkbox list item
   ("C-<return>" . org-insert-todo-heading) ; TODO heading or a checkbox list item
   ("M-RET" . org-meta-return)              ; Normal heading, normal list item, or various other things
   ("M-<return>" . org-meta-return)         ; Normal heading, normal list item, or various other things

   ;;---
   ;; Movement
   ;;---
   ;; TODO:meow: Why the fuck can't I do "hey `meow', this is a bind for `meow-normal-mode' in `org-mode-map'?!
   ("M-." . org-previous-visible-heading)
   ("M-e" . org-next-visible-heading))


  ;;------------------------------
  :config
  ;;------------------------------

  ;;------------------------------
  ;; `General'
  ;;------------------------------

  (defun mantle:meow/keybind/general:notes ()
    "Create the \"Notes...\" keybinds in `general' for `meow'."
    ;;------------------------------
    ;; Global Keybinds
    ;;------------------------------
    (keybind:leader/global:def
      :infix (keybind:infix "n")      ; notes
      "" '(nil :which-key "Notes...") ; infix title
      ;; Short(er)cuts
      "s" (list #'org-store-link         :which-key "Link: Store"))

    (keybind:leader/global:def
      :infix (keybind:infix "n" "l")  ; notes -> links
      "" '(nil :which-key "Links...") ; infix title
      "s" (list #'org-store-link         :which-key "Link: Store")
      "l" (list #'org-insert-link        :which-key "Link: Insert")
      "h" (list #'mode:cmd:org:here/link :which-key "Here: Insert Link")
      "k" (list #'mode:cmd:org:here/yank :which-key "Here: Yank as Link"))

    (keybind:leader/global:def
      :infix (keybind:infix "n" "a")  ; notes -> agenda
      "" '(nil :which-key "Agenda...") ; infix title
      "A" (list #'org-agenda      :which-key "`org-agenda'")
      "t" (list #'org-tags-view   :which-key "Tags Search")
      "v" (list #'org-search-view :which-key "View Search"))

    ;;------------------------------
    ;; Local (`org-mode') Keybinds
    ;;------------------------------
    (keybind:meow:leader/local:bind-keys
        'org-mode-map
      "t" #'org-todo
      "c" #'org-toggle-checkbox ; TODO:org: Could do `org-ctrl-c-ctrl-c' here instead?
      "u" (list #'org-metaright :which-key "Indent/Demote")
      "o" (list #'org-metaleft  :which-key "Outdent/Promote")
      ;; TODO:org: More?
      )

  ;; TODO:meow: Make this function able to process infixes:
  ;; (keybind:meow:leader/local:bind-keys
  ;;     'org-mode-map
  ;;   :infix (keybind:infix "i")        ; insert
  ;;   "" '(nil :which-key "Insert...")) ; infix title

  (keybind:meow:leader/local:bind-keys
      'org-mode-map
    ;; TODO:meow: Make this function able to process infixes:
    ;; :infix (keybind:infix "i" "t")                 ; insert -> template
    ;; "" '(nil :which-key "Templates...") ; infix title

    ;; TODO:template: Better way to do an arg that will change but is generally the same?
    ;;                Like a history variable or something? Or saving what mode something was killed from?
    "it s" (template:cmd:insert :src "placeholder" :yank)
    "it q" (template:cmd:insert :quote :yank)))


  ;;------------------------------
  ;; `Transient'
  ;;------------------------------

  (defun mantle:meow/keybind/transient:notes ()
    "Create the \"Notes...\" keybinds in `transient' for `meow'."
    ;; Creation of this Should be moved to "keybinds/notes.el" if any non-org
    ;; stuff wants added.
    (transient-define-prefix mantle:meow/transient:notes ()
      "Notes commands like org-mode links, org-journal entries, etc."
      [["Links"
        ("s" "Link: Store"  org-store-link)
        ("l" "Link: Insert" org-insert-link)
        ("h" "Link: Insert as \"here\"/region" mode:cmd:org:here/link)
        ("k" "Link: Yank as \"here\"/region"  mode:cmd:org:here/yank)]
       ["Agenda"
        ("A" "`org-agenda'" org-agenda)
        ("t" "Tags Search" org-tags-view)
        ("v" "View Search" org-search-view)]])
    ;; (mantle:meow/transient:notes)

    (meow-leader-define-key '("n" . mantle:meow/transient:notes)))

  ;;------------------------------
  ;; Actually Create Keybinds:
  ;;------------------------------

  (if (imp:provided? :keybinds 'user 'general 'meow)
      (mantle:meow/keybind/general:notes)
    (mantle:meow/keybind/transient:notes)))


;;------------------------------
;; Keybinds : Evil
;;------------------------------

(imp:use-package org
  :when  (imp:flag? :keybinds +evil)
  :after (:and (:keybinds user general evil)
               evil evil-collection evil-org)

  ;;------------------------------
  :general ; evil
  ;;------------------------------

  ;; Override the usual `evil-open-below' with a Special Org Version.
  ;; See "mantle/config/keybinds/evil/evil.el" for the usual evil state keybinds.
  (:states 'normal
   :keymaps '(evil-org-mode-map)
   :prefix "s"
   "t" #'evil-org-open-below))


;; TODO-meow: Delete after checking org-mode to see if these can be deleted.
;; ;;------------------------------
;; ;; Undo Doom hacks to Org-Mode
;; ;;------------------------------
;; (after! evil-org
;;   ;; Make [TAB] cycle through all (sub)tree visibilities (the default behavior) instead of just current tree.
;;   ;;   - https://github.com/hlissner/doom-emacs/blob/develop/modules/lang/org/README.org#hacks
;;   (remove-hook 'org-tab-first-hook #'+org-cycle-only-current-subtree-h)

;;   ;; Make 'open link' open file in other window like org-mode defaults to doing.
;;   ;; Doom changes this to #'find-file.
;;   (setf (alist-get 'file org-link-frame-setup) #'find-file-other-window))


;;------------------------------
;; TODO-meow: Do these work well with Evil/Meow/whatever?
;;------------------------------
;; TODO: Move to `:mode/org' module?
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
;;  (defun mode:org:speed-commands? ()
;;    "Allow speed keys when at any headline *, not just beginning of line."
;;    (and (looking-at org-outline-regexp) (looking-back "^\**")))


;;------------------------------
;; Org-Mode Hacks
;;------------------------------
(imp:use-package org

  ;;------------------------------
  :init
  ;;------------------------------

  ;; Also used by `org-journal'.
  (defun mantle:advice:org/window:org-pop-to-buffer (buffer &optional norecord)
    "`:override' advice for `org-pop-to-buffer'.

Use `pop-to-buffer' instead of `switch-to-buffer' to open BUFFER.'

Ensure todo, agenda, and other minor popups are delegated to the popup system.

From Doom's `+popup--org-pop-to-buffer-a' in \"modules/ui/popup/+hacks.el\"."
    (pop-to-buffer buffer nil norecord))


  ;;------------------------------
  :config
  ;;------------------------------

  (advice-add #'org-pop-to-buffer :override #'mantle:advice:org/window:org-pop-to-buffer)


  (defun mantle:advice:org/window:suppress-delete-other-windows (fn &rest args)
    "Org has a scorched-earth window management policy I'm not fond of. i.e. it
kills all other windows just so it can monopolize the frame. No thanks. We can
do better.

From Doom's `+popup--suppress-delete-other-windows-a' in
\"modules/ui/popup/+hacks.el\"."
    (cl-letf (((symbol-function #'delete-other-windows) #'ignore)
              ((symbol-function #'delete-window)        #'ignore))
      (apply fn args)))

  (advice-add #'org-add-log-note                       :around #'mantle:advice:org/window:suppress-delete-other-windows)
  (advice-add #'org-capture-place-template             :around #'mantle:advice:org/window:suppress-delete-other-windows)
  (advice-add #'org-export--dispatch-ui                :around #'mantle:advice:org/window:suppress-delete-other-windows)
  (advice-add #'org-agenda-get-restriction-and-command :around #'mantle:advice:org/window:suppress-delete-other-windows)
  (advice-add #'org-goto-location                      :around #'mantle:advice:org/window:suppress-delete-other-windows)
  (advice-add #'org-fast-tag-selection                 :around #'mantle:advice:org/window:suppress-delete-other-windows)
  (advice-add #'org-fast-todo-selection                :around #'mantle:advice:org/window:suppress-delete-other-windows)


  ;; TODO-org: Do I need to fix `org-goto'? Don't currently use it, but seems like it could be quite useful?
  ;; Save this in case I do start using `org-goto'...
  ;;   (defun mantle:advice:org/window:fix-goto (fn &rest args)
  ;;     "`org-goto' uses `with-output-to-temp-buffer' to display its help buffer,
  ;; for some reason, which is very unconventional, and so requires these gymnastics
  ;; to tame (i.e. to get the popup manager to handle it).
  ;;
  ;; From Doom's `+popup--org-fix-goto-a'  in \"modules/ui/popup/+hacks.el\"."
  ;;     (letf! (defun internal-temp-output-buffer-show (buffer)
  ;;                  (let ((temp-buffer-show-function
  ;;                         (doom-rpartial #'+popup-display-buffer-stacked-side-window-fn nil)))
  ;;                    (with-current-buffer buffer
  ;;                      (+popup-buffer-mode +1))
  ;;                    (funcall internal-temp-output-buffer-show buffer)))
  ;;           (apply fn args))
  ;;       (apply fn args))
  ;;   (advice-add #'org-goto-location :around #'mantle:advice:org/window:fix-goto)


  (defun mantle:advice:org/window:read-char-exclusive (&rest _args)
    "`:before' advice for `read-char-exclusive'.

Advice-ception for function `mantle:advice:org/window:fix-popup-shrinking'.

From Doom's `+popup--org-fix-popup-window-shrinking-a' in
\"modules/ui/popup/+hacks.el\"."
    (message nil))


  (defun mantle:advice:org/window:split-window-vertically (&optional _size)
    "`:filter-args' advice for `split-window-vertically'.

Advice-ception for function `mantle:advice:org/window:fix-popup-shrinking'.

From Doom's `+popup--org-fix-popup-window-shrinking-a' in
\"modules/ui/popup/+hacks.el\"."
    ;; Have to return a list? Otherwise:
    ;;   > Debugger entered--Lisp error: (wrong-type-argument listp -5)
    ;;   >   apply(split-window-below -5)
    ;;   >   split-window-vertically()
    (list
     ;; Ignore `_SIZE'...
     (- 0 window-min-height 1)))


  (defun mantle:advice:org/window:org-fit-window-to-buffer (&optional window _max-height _min-height _shrink-only)
    "`:override' advice for `org-fit-window-to-buffer'.

Advice-ception for function `mantle:advice:org/window:fix-popup-shrinking'.

From Doom's `+popup--org-fix-popup-window-shrinking-a' in
\"modules/ui/popup/+hacks.el\"."
    ;; (when-let (buf (window-buffer window))
    ;;   (with-current-buffer buf
    ;;     (+popup-buffer-mode)))
    (when (> (window-buffer-height window)
             (window-height window))
      (fit-window-to-buffer window (window-buffer-height window))))


  (defun mantle:advice:org/window:fix-popup-shrinking (fn/orig &rest args)
    "`:around' advice for org fast selection functions.

Hide the mode-line in *Org tags* buffer so you can actually see its
content and displays it in a side window without deleting all other windows.
Ugh, such an ugly hack.

From Doom's `+popup--org-fix-popup-window-shrinking-a' in
\"modules/ui/popup/+hacks.el\"."
    ;; Want to be able to remove our advice when we're done.
    (unwind-protect
        (progn
          (advice-add #'read-char-exclusive      :before      #'mantle:advice:org/window:read-char-exclusive)
          (advice-add #'split-window-vertically  :filter-args #'mantle:advice:org/window:split-window-vertically)
          (advice-add #'org-fit-window-to-buffer :override    #'mantle:advice:org/window:org-fit-window-to-buffer)

          (apply fn/orig args))

      ;; Always remove our advice when we're done.
      (advice-remove #'read-char-exclusive      #'mantle:advice:org/window:read-char-exclusive)
      (advice-remove #'split-window-vertically  #'mantle:advice:org/window:split-window-vertically)
      (advice-remove #'org-fit-window-to-buffer #'mantle:advice:org/window:org-fit-window-to-buffer)))


  (advice-add #'org-fast-tag-selection  :around #'mantle:advice:org/window:fix-popup-shrinking)
  (advice-add #'org-fast-todo-selection :around #'mantle:advice:org/window:fix-popup-shrinking)


  (defun mantle:advice:org/window:org-edit-src-code (fn &rest args)
    "`:after' advice for `org-edit-src-code'.

Mark the `org-edit-src-code' buffer so we can kill it if needed.

From Doom's `+popup--org-edit-src-exit-a' in \"modules/ui/popup/+hacks.el\"."
    (let ((window (selected-window)))
      (set-window-parameter window 'mantle:org/window:org-edit-src-code t)))


  (defun mantle:advice:org/window:org-edit-src-exit (fn &rest args)
    "`:around' advice for `org-edit-src-exit'.

If you switch workspaces or the src window is recreated...
delete the src window?

From Doom's `+popup--org-edit-src-exit-a' in \"modules/ui/popup/+hacks.el\"."
    (let ((window (selected-window)))
      (prog1 (apply fn args)
        (when (and (window-parameter window 'mantle:org/window:org-edit-src-code)
                   (window-live-p window))
          (delete-window window)))))


  (advice-add #'org-edit-src-exit :around #'mantle:advice:org/window:org-edit-src-exit))


;;------------------------------------------------------------------------------
;; Org Exporters (ox)
;;------------------------------------------------------------------------------

;;------------------------------
;; Org Exporter: GitHub-Flavored Markdown
;;------------------------------
(imp:use-package ox-gfm
  :after org)


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'config 'user 'org 'mode)

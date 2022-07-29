;;; mantle/config/org-mode.el -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2022-06-02
;; Modified:   2022-06-03
;; URL:        https://github.com/cole-brown/.config-emacs
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;  Configure Org-Mode and Friends.
;;
;;; Code:


;;------------------------------------------------------------------------------
;;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;                        ORG-MODE - The Thing Itself
;;  The Cthulu of Emacs. Required and will drive you insane trying to grok.
;;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;------------------------------------------------------------------------------

(imp:require :datetime)
(imp:require :innit)
(imp:require :jerky)
(imp:require :path)
(imp:require :buffer 'search)
(imp:require :buffer 'name)
(imp:require :mode 'org)


;;------------------------------------------------------------------------------
;; Documents in General
;;------------------------------------------------------------------------------

;; Could move out of org-mode setup if we get more doc mode setups.

(jerky:set 'docs 'tab 'short
           :namespace :default
           :value 2
           :docstr "Short tab width is 2 spaces.")
(jerky:set 'docs 'tab 'long
           :namespace :default
           :value 4
           :docstr "Long tab width is 4 spaces.")


;;------------------------------------------------------------------------------
;; Org-Mode
;;------------------------------------------------------------------------------

;; TODO: delete this lil block:
;; ;;--------------------
;; ;; MUST PROCEED ORG SETUP?
;; ;;--------------------
;;
;; ;; DOOM-NOTE:
;; ;; If you use `org' and don't want your org files in the default location below,
;; ;; change `org-directory'. It must be set before org loads!
;; (if-let (lily (jerky:get 'path 'lily))
;;     ;; Set org to use lily dir if we have it.
;;     (setq org-directory lily)
;;   ;; Otherwise not sure... This is fine until something is figured out.
;;   (setq org-directory "~/org/"))


;;--------------------
;; Org-Mode Set-Up
;;--------------------
(imp:use-package org
  ;;--------------------
  :init
  ;;--------------------

  ;;---
  ;; Create Org-Mode Hooks
  ;;---
  (let ((path/rel (path:relative (path:current:file) user-emacs-directory)))
    (innit:hook:defun org-jump-to-now-hook
                      '(:name "org/jump-to-now-target"
                        :file path/rel
                        :docstr "Jump point to \"now\" link, if it's in the first part of the file."
                        :quiet nil) ;; TODO: t)
                      (buffer:cmd:search:header "[[--now"))

    (innit:hook:defun org-local-settings-hook
                      '(:name "org/local-settings"
                        :file path/rel
                        :docstr "Set up buffer local vars."
                        :quiet nil) ;; TODO: t)
                      (setq-local yas-indent-line 'auto)
                      ;; Automatically becomes buffer local.
                      (setq tab-width (jerky:get 'docs 'tab 'short))))


  ;;--------------------
  :hook
  ;;--------------------

  ((org-mode . mantle:hook:org/jump-to-now-target) ;; (innit:hook:func-symbol "org/jump-to-now-target" nil)
   (org-mode . mantle:hook:org/local-settings))    ;; (innit:hook:func-symbol "org/local-settings"     nil)


  ;;--------------------
  :custom
  ;;--------------------

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

  ;; TODO: Not sure if works well with evil.
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
  ;;  (jerky:get 'code 'tab 'normal)
  ;;  "Set indent to tab-width.")

  (org-src-preserve-indentation t)


  ;;--------------------
  :config
  ;;--------------------

  ;;--------------------
  ;; conditional customization: org-agenda
  ;;--------------------

  (when-let ((agenda-files (jerky:get 'path 'org 'agenda)))
    (customize-set-variable 'org-agenda-files
                            agenda-files
                            "My paths to search for agenda items."))

  ;;--------------------
  ;; configuration
  ;;--------------------

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


;; TODO: Delete after checking org-mode to see if these can be deleted.
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
;; TODO: Do these work well with Evil?
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


;;------------------------------------------------------------------------------
;; Org-Journal
;;------------------------------------------------------------------------------

;;------------------------------
;; Namespaced Commands
;;------------------------------
;; TODO: Move to `:mode/org' module?
(defun mode:org/journal:namespaced (namespace command &rest args)
  "Run an org-journal COMMAND in NAMESPACE.

Sets (lexical context) all org-journal custom vars related to NAMESPACE.
Then runs COMMAND interactively with ARGS."
  ;; TODO: Why is this interative? If it needs to be, add a way to actually set the params interactively...
  (interactive)
  (let ((org-journal-file-format (jerky:get 'org 'journal 'file 'format
                                            :namespace namespace))
        (org-journal-dir (jerky:get 'path 'org 'journal
                                    :namespace namespace)))
    (apply #'funcall-interactively command args)))
;; (mode:org/journal:namespaced :home #'message "%s %s" org-journal-file-format org-journal-dir)
;; (mode:org/journal:namespaced :work #'message "%s %s" org-journal-file-format org-journal-dir)


;;------------------------------
;; Org-Journal Set-Up
;;------------------------------
(imp:use-package org-journal
  :after org

  ;;--------------------
  :init
  ;;--------------------

  ;;---
  ;; "Home" Domain
  ;;---
  (jerky:set 'org 'journal 'file 'format
             :namespace :home
             :value (concat (datetime:format/get 'iso-8601 'date)
                            ;; TODO: 'notebook' not quickest to
                            ;; auto-complete to. Find better.
                            ".notebook.org")
             :docstr "`org-journal-file-format' for :home")

  ;;---
  ;; "Work" Domain
  ;;---
  (jerky:set 'org 'journal 'file 'format
             :namespace :work
             :value (concat (datetime:format/get 'iso-8601 'date)
                            ;; TODO: 'logbook' not quickest to
                            ;; auto-complete to. Find better.
                            ".logbook.org")
             :docstr "`org-journal-file-format' for :work")


  ;;--------------------
  :custom
  ;;--------------------

  (org-journal-dir (jerky:get 'path 'org 'journal
                              :namespace (jerky:get 'namespace 'system)))

  ;; Tack (full) day name onto our format for the org-journal headline.
  (org-journal-date-format (concat (datetime:format/get 'iso-8601 'date)
                                   ", %A"))
  ;; This can be a function if more is wanted. E.g. inserting new header text
  ;; into empty files.
  ;;  - https://github.com/bastibe/org-journal#journal-file-content
  ;; TODO: Do stuff when file is empty.

  ;; A year per file. Could do monthly if too big. Weekly and daily are also
  ;; options. Daily is the default.
  ;; [2019-12-03] - A month per file. Year is getting too big...
  (org-journal-file-type 'monthly)

  ;; org-journal-file-format: Make it a bit more ISO-ish (yyyy-mm-dd).
  ;;   - default:   yyyymmdd
  ;;   - better:    yyyy-mm-dd.org
  ;; But those both are difficult to switch to when various other buffers open,
  ;; so we'll go to this:
  ;;   - betterer:  yyyy-mm-dd.journal.org
  ;; Or, even better, leave it up to someone else to decide and get it from a jerky key:
  (org-journal-file-format (jerky:get 'org 'journal 'file 'format
                                      :namespace (jerky:get 'namespace 'system)))


  ;;--------------------
  :config
  ;;--------------------

  ;;--------------------
  ;; customization
  ;;--------------------

  ;; This is only the path to the default namespace, so multi-namespace
  ;; shenanigans have to constantly mess with this, I think?
  (unless (dlv:var:safe/predicate? 'org-journal-dir)
    ;; It's marked as risky - force it to safe?
    (dlv:var:safe/predicate 'org-journal-dir #'file-directory-p :quiet))


  ;;--------------------
  ;; configuration
  ;;--------------------

  ;; Keybinds are in:
  ;;   - TODO: WHERE ARE KEYBINDS IN sn-004???
  ;;   - Previously: config/keybinds/org-mode.el
  )


;;------------------------------------------------------------------------------
;; Org-Mode's Legion of Minions
;;------------------------------------------------------------------------------

;;------------------------------
;; Org Exporter: GitHub-Flavored Markdown
;;------------------------------
(imp:use-package ox-gfm
  :after org)


;;------------------------------
;; Orgit: Link to Magit buffers from Org docs
;;------------------------------
;; https://github.com/magit/orgit
;; Use the usual `org-store-link' & `org-insert-link' to link to Magit buffers.
;;
;;---
;; Format
;;---
;; https://github.com/magit/orgit#format
;;
;; orgit:/path/to/repo/            links to a magit-status buffer
;; orgit-log:/path/to/repo/::REV   links to a magit-log buffer
;; orgit-rev:/path/to/repo/::ARGS  links to a magit-revision buffer
;;
;;---
;; Export
;;---
;; https://github.com/magit/orgit#export
;;
;; When an Org file containing such links is exported, then the url of the
;; remote configured with `orgit-remote' is used to generate a web url according
;; to `orgit-export-alist'. That webpage should present approximately the same
;; information as the Magit buffer would.
;;
;; Both the remote to be considered the public remote, as well as the actual web
;; urls can be defined in individual repositories using Git variables.
;;
;; To use a remote different from `orgit-remote' but still use `orgit-export-alist'
;; to generate the web urls, use:
;;   git config orgit.remote REMOTE-NAME
;;
;; To explicitly define the web urls, use something like:
;;   git config orgit.status http://example.com/repo/overview
;;   git config orgit.log http://example.com/repo/history/%r
;;   git config orgit.rev http://example.com/repo/revision/%r

(imp:use-package orgit
  :after (:and org magit))


;; TODO: delete this if not using org-roam
;; ;;------------------------------
;; ;; Org-Roam Funcs
;; ;;------------------------------
;; ;; Zettelkasten Note-Taking with Org-Mode

;; (defun mode:org/roam:file/name:timestamp/title (title)
;;   "Return a file name (without extension) for new files.

;; It uses TITLE and the current timestamp to form a unique title."
;;   (let ((timestamp (datetime:string/get 'file 'datetime))
;;         (slug (org-roam--title-to-slug title)))
;;     (format "%s_%s" timestamp slug)))


;; (defun mode:org/roam:buffer/deactivate ()
;;   "Like `org-roam-buffer-deactivate' but don't delete the window."
;;   (interactive)

;;   (setq org-roam-last-window (get-buffer-window))
;;   ;; (delete-window (get-buffer-window org-roam-buffer)
;;   ;; (kill-buffer org-roam-buffer)
;;   )


;; ;;---
;; ;; NOTE!
;; ;;---
;; ;; This must be before Doom's org-roam packages' `use-package' `:config' section!
;; ;; ---
;; ;; Doom's .emacs.d/modules/lang/org/contrib/roam.el wants org-roam to be in a
;; ;; subdirectory of `org-directory', so they do a lot to try to make that happen
;; ;; and the only out is to put this in early...
;; ;;---
;; ;; Every org file within this directory tree root is part of the org-roam
;; ;; ecosystem.
;; ;; So: everything in lily.
;; (customize-set-variable 'org-roam-directory
;;                         (jerky:get 'path 'lily))


;; ;;------------------------------
;; ;; Org-Roam
;; ;;------------------------------
;; (imp:use-package org-roam
;;   :after org

;;   ;;--------------------
;;   :custom
;;   ;;--------------------

;;   (org-roam-buffer (buffer:name:special "lily" nil :info))

;;   ;; ;; Doom is a little pesky about keeping the org-roam buffer open.
;;   ;; ;; It even does it when you close a file and an org buffer is in any visible frame.
;;   ;; (+org-roam-open-buffer-on-find-file nil)

;;   ;; TODO: Do I want/need these?
;;   ;; ;; Org-Roam filenames need to be uniquely named, but name doesn't matter much?
;;   ;; ;; Or so they say... So they can just gen file names from the current time.
;;   ;; (org-roam-filename-noconfirm t)
;;   ;; (org-roam-file-name-function #'mode:org/roam:file/name:timestamp/title)

;;   ;; If 'right isn't desirable:
;;   ;; (org-roam-position 'left)

;;   ;; What % of total frame width to use.
;;   ;; (org-roam-buffer-width 0.4)

;;   ;; By default, links are inserted with the title as the link description. This
;;   ;; can make them hard to distinguish from external links. If you wish, you may
;;   ;; choose add special indicators for Org-roam links by tweaking
;;   ;; this, for example:
;;   (org-roam-link-title-format "r://%s")

;;   ;; `right' (default) is probably correct? But trying out `bottom'.
;;   (org-roam-buffer-position 'bottom)


;;   ;;--------------------
;;   :config
;;   ;;--------------------

;;   ;;--------------------
;;   ;; conditional customization
;;   ;;--------------------

;;   ;; Mitigate annoyance of org-roam (on Windows) throwing "Selecting deleted
;;   ;; buffer" errors every other time it runs `org-roam-build-cache'.
;;   (when (eq system-type 'windows-nt)
;;     ;; default value: 'idle-timer
;;     (customize-set-variable 'org-roam-db-update-method 'immediate))

;;   ;;--------------------
;;   ;; configuration
;;   ;;--------------------
;;   ;; none
;;   )


;; TODO: Get org-contacts actually working like it never really was on vanilla?
;;------------------------------
;; Org Contacts
;;------------------------------
;; (imp:use-package org-contacts
;;   :after org
;;   :disabled ;; is this making things slow?
;;   :ensure nil
;;   :demand t
;;
;;   :custom
;;   (org-contacts-files (path:abs:file
;;                        (spy:dirky/path :secrets :secrets/org)
;;                        "contacts.org"))
;;
;;   :config
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

;; TODO: This probably slows stuff down too much, yeah? :(
;;------------------------------
;; Org-Mode Headline Bullets: (Making Org-Mode Pretty)
;;------------------------------
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


;;------------------------------
;; Pretty Checkboxes in Unicode
;;------------------------------
;;  ;; Check box visual upgrade.
;;  ;;   empty, indeterminate, marked: [ ], [-], [X] -> ☐, ▣, ☒
;;  ;;     aka 'unchecked', 'mixed checked/unchecked children', 'checked'
;;  ;; Trial: [2019-07-30 Tue]
;;  ;;   - Con: This doesn't update until point has moved off the line... Possibly
;;  ;;     interacting with my highlight row thing/mode?
;;  ;; Nice lil search for symbols: http://www.unicode.org/charts/
;;  (innit:hook:defun org-mode-hook t
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


;;------------------------------
;; Pretty List Bullet in Unicode
;;------------------------------
;;  ;; Show list markers with a middle dot instead of the
;;  ;; original character.
;;  (innit:hook:defun org-mode-hook t
;;      nil "simple-list" "init/config/configure-org-mode.el"
;;    "Nice up simple lists - replacing hypen with a unicode middle dot."
;;    (font-lock-add-keywords
;;     nil ;; 'org-mode - some org-mode stuff (e.g. org-journal) is a derived
;;         ;; major mode and thus needed either more than just `org-mode', or to
;;         ;; be `nil' and put in the org-mode hook.
;;     '(("^ *\\([-]\\) "
;;        (0 (prog1 () (compose-region (match-beginning 1)
;;                                     (match-end 1) "•")))))))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'config 'user 'org-mode)

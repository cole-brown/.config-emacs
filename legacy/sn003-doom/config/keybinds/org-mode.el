;;; config/keybinds/org-mode.el -*- lexical-binding: t; -*-

;;                                                               ──────────                                                               ;;
;; ╔════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════╗ ;;
;; ║                                                              KEYBINDS                                                              ║ ;;
;; ╚════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════╝ ;;
;;                                                               ──────────                                                               ;;


;;------------------------------------------------------------------------------------------------------------------------------------------
;; Org-Mode
;;------------------------------------------------------------------------------------------------------------------------------------------

;; Map some things for org-mode.
(map! :after org
      :map org-mode-map
      :localleader
      ;; :map evil-org-mode-map
      :prefix "l" ;; links
      :desc "Link as 'here'." "h" #'spy:cmd:org/here.link
      :desc "Paste as 'here' link." "p" #'spy:cmd:org/here.yank)


;; TODO: Do I want this again?
;; ;; 'C-c <tab>' to show headings only (no top parent notes, no
;; ;; children notes, just headings). Org-Mode had 'C-c <tab>' as
;; ;; outline-show-children, which only shows direct children
;; ;; headings, not all descendants' headings.
;; ;; https://yiufung.net/post/org-mode-hidden-gems-pt1/
;; ("C-c <tab>" . #'org-kill-note-or-show-branches)

;;------------------------------
;; Org-Mode & the Thousand Different "RET" Key Functions
;;------------------------------

;; Auto-indent is a bit odd... Try swapping around RET and S-RET these so the literal newline is on normal enter? Also, `evil-org-return'
;; is... fucked up. It's description says 'Pressing return twice cancels the continuation of the itemlist or table.', but it's not used for
;; continuing lists. "C-RET" is - `+org/insert-item-below'.
;; But you can do "C-RET C-u" to delete the indentation...
;; ...But you can keep doing that to delete previous lines. So - fuck that noise.
;; And it's attempt to detect that you're on an empty line (eolp) and (bolp) to do a literal `org-return' isn't working for me?
;; ...Am I using `evil-org-return' even? I don't think so...
;; +org/shift-return

(after! evil-org
  (map! :map evil-org-mode-map
    ;; Fuck your "more intuitive RET keybinds", Doom.
    :i [return]   #'+org/shift-return
    :i "RET"      #'+org/shift-return
    :i [S-return] (cmd! (org-return electric-indent-mode))
    :i "S-RET"    (cmd! (org-return electric-indent-mode))
  ))


;;------------------------------------------------------------------------------------------------------------------------------------------
;; Org-Agenda
;;------------------------------------------------------------------------------------------------------------------------------------------

(after! org-mode
  ;;------------------------------
  ;; Org-Agenda & the Billion Orphaned Org Buffers
  ;;------------------------------
  ;; Thanks to: https://emacs.stackexchange.com/a/20438
  ;;---

  ;; `org-agenda-quit' is evil - it leaves a billion org mode buffers open.
  ;; And it's innocently sat there at "q" that apparently nothing in evil overrides...
  (bind-key [remap org-agenda-quit]
            #'org-agenda-exit
            org-agenda-keymap)
  ;; And this is at "Q".
  (bind-key [remap org-agenda-Quit]
            #'org-agenda-exit
            org-agenda-keymap)
  ;; Fuck y'all both.
  )


;;------------------------------------------------------------------------------------------------------------------------------------------
;; Org-Roam
;;------------------------------------------------------------------------------------------------------------------------------------------

;; Org-Roam is currently not enabled [2021-06-10].
(when (featurep! :lang org +roam)
  (spy:secret/if "config/keybinds/org-mode.el"
      "Skipping org-roam keybind set-up."
    (map! :leader
          :prefix "nr" ;; notes -> roam
          :desc "Kill roam info buffer" "K" #'sss:org-roam/buffer/deactivate
          :desc "Delete roam info window" "k" #'org-roam-buffer-deactivate)
    (map! :after org
          :map org-mode-map
          :localleader
          :prefix "m" ;; org-roam is... 'm' here instead of 'r'.
          ;; and copy the above 'map!':
          :desc "Kill roam info buffer" "K" #'sss:org-roam/buffer/deactivate
          :desc "Delete roam info window" "k" #'org-roam-buffer-deactivate)))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :dot-emacs 'config 'keybinds 'org-mode)

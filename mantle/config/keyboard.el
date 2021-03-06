;;; mantle/config/keyboard.el --- Change basic keybinds. -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2022-07-19
;; Modified:   2022-07-20
;; URL:        https://github.com/cole-brown/.config-emacs
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Change basic keybinds.
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Keybinds: Evil
;;------------------------------------------------------------------------------
;; We're using Emacs, so obivously don't use standard Emacs keybinds.
;; Use Vim keybinds, via Evil.
;; But...
;; Obivously don't use standard Evil keybinds, use something totally different.


(imp:eval:after (:and evil evil-collection general)


  ;; ╔═════════════════════════════════════════════════════════════════════════╗
  ;; ║ Undefine Keys                                                           ║
  ;; ╚═════════════════════════════════════════════════════════════════════════╝

  ;; ┌────────────────────────────────┐
  ;; │ Keymap:                        │
  ;; │   - nil/global                 │
  ;; └────────────────────────────────┘
  ;; Undefine some keys so we can redefine them later.
  ;;
  ;; TODO: Doom wasn't happy just overwriting, which is why we undefine then
  ;; redefine... Is just standard general/evil/etc ok without the undefine?
  ;;
  ;; TODO: What of these do we need now, and what don't we need now?

  ;; Unbind from `evil-emacs-state', which is confusing to get out of (need to call w/ negative prefix arg).
  (general-define-key
   :states '(motion insert)
   "C-z" #'nil)
  ;; Unbind from `suspend-frame', which takes over "C-z" when `evil-emacs-state' gets unbound.
  (general-define-key
   "C-z" #'nil)

  (general-define-key
   :states 'normal
   ;; Unbind from `evil-substitute'.
   "s" #'nil
   ;; Unbind from `evil-change'.
   "c" #'nil
   ;; Unbind from `evil-backward-char'.
   ;; TODO: Also exists in a lot of other keymaps.
   "h" #'nil
   ;; Unbind from `evil-ex-search-next'.
   ;; TODO: Also exists in some magit keymaps.
   "n" #'nil
   ;; Unbind from `evil-repeat-pop'.
   "C-." #'nil
   ;; Unbind from `better-jumper-jump-backward'.
   "C-o" #'nil
   ;; Unbind from `evil-normal-state-map'.
   "A" #'nil
   ;; Unbind from `evil-open-above'.
   "O" #'nil
   ;; Unbind from `evil-insert-line'.
   "I" #'nil)

  ;; ┌────────────────────────────────┐
  ;; │ Keymap:                        │
  ;; │  - `evil-snipe-mode-map'       │
  ;; │  - `evil-snipe-local-mode-map' │
  ;; └────────────────────────────────┘

  (imp:eval:after evil-snipe
    ;; Doom or General or someone will complain if bound keys are not
    ;; unbound before being used for prefixes...
    ;; TODO: Who complained? Was it Doom? If so do we need still?

    (general-define-key :states '(motion visual normal)
                        :keymaps '(evil-snipe-mode-map evil-snipe-local-mode-map)
                        ;; Unbind 's' and 't' from snipe.
                        "s" #'nil
                        "t" #'nil))

  ;; ┌────────────────────────────────┐
  ;; │ Keymap:                        │
  ;; │   - `evil-org-mode-map'        │
  ;; └────────────────────────────────┘
  (imp:eval:after (:and org evil-org)
    (general-define-key :states 'normal
                        :keymaps '(evil-org-mode-map)
                        ;; Unbind from `evil-open-below'.
                        "o" #'nil))


  ;; ╔═════════════════════════════════════════════════════════════════════════╗
  ;; ║ Define Keys                                                             ║
  ;; ╚═════════════════════════════════════════════════════════════════════════╝

  ;; ┌────────────────────────────────┐
  ;; │ Movement                       │
  ;; └────────────────────────────────┘
  ;; ESDF position keys (shifted-WASD left-hand, index on home key)
  ;;   - Except Dvorak, so .oeu/>OEU keys.
  ;; A & G (Dvorak A & I) are "extra left" and "extra right", basically.
  ;;   - example:
  ;;     - "shift left" (O) is `evil-backward-word-end' which goes back to the
  ;;       end of the previous word.
  ;;     - "shift extra left" (A) is `evil-backward-word-begin' which goes back
  ;;       further, to the beginning of the previous word.

  (general-define-key
   :states '(motion visual normal)
   ;; ──┬────────────────
   ;;   │ ↑ ↓ ← →
   ;; ──┴────────────────
   "." #'evil-previous-line
   "e" #'evil-next-line
   "o" #'evil-backward-char
   "u" #'evil-forward-char)

  (general-define-key
   :states 'motion

   ;; ──┬────────────────
   ;;   │ Word
   ;; ──┴────────────────
   ;; ESDF-based. To go 'farther away', move finger away (e.g. word-next-begin is "U", word-next-end is "I".)
   ;; "Small" words: Shift
   ;; "Big"   words: Meta
   ;; TODO: swap modifier if using "Big" more than "Small".
   ;; TODO: TODO: Use derive? But nothing to derive "A"/"I" from.
   "A"     #'evil-backward-word-begin
   "O"     #'evil-backward-word-end
   "U"     #'evil-forward-word-begin
   "I"     #'evil-forward-word-end
   "M-a"   #'evil-backward-WORD-begin
   "M-o"   #'evil-backward-WORD-end
   "M-u"   #'evil-forward-WORD-begin
   "M-i"   #'evil-forward-WORD-end

   ;; ──┬────────────────
   ;;   │ Sentences
   ;; ──┴────────────────
   "("     #'evil-backward-sentence-begin
   ")"     #'evil-forward-sentence-begin

   ;; ──┬────────────────
   ;;   │ Paragraphs
   ;; ──┴────────────────
   ;; TODO: Leave as-is or move near movement keys?
   "{"     #'evil-backward-paragraph
   "}"     #'evil-forward-paragraph

   ;; ──┬────────────────
   ;;   │ Scroll
   ;; ──┴────────────────
   ;; TODO: Recentering & other scrolling.
   "C-."   #'evil-scroll-up
   "C-e"   #'evil-scroll-down
   "M-C-." #'evil-scroll-page-up
   "M-C-e" #'evil-scroll-page-down

   ;; ──┬────────────────
   ;;   │ Lines
   ;; ──┴────────────────
   "C-o"   #'evil-beginning-of-line
   "C-u"   #'evil-end-of-line)

  ;; ┌────────────────────────────────┐
  ;; │ Evil States                    │
  ;; └────────────────────────────────┘
  ;; IJKL position keys (shifted-WASD right-hand, index on home key)
  ;;   - Except Dvorak, so CHTN keys.
  ;; H & ; (Dvorak D & S) are "extra left" and "extra right", basically.

  ;; TODO: define a prefix differently?
  ;; 'general' has `general-create-definer' for "frequently used prefix keys" (e.g. SPC leader key).
  ;; https://github.com/noctuid/general.el#evil-examples
  (general-define-key
   :states 'normal
   :prefix "s"
   "h" #'evil-insert
   "n" #'evil-append
   "t" #'evil-open-below
   "c" #'evil-open-above
   "H" #'evil-insert-line
   "N" #'evil-append-line
   "T" #'evil-replace-state)

  (general-define-key
   :states 'visual
   :prefix "s"
   "h" #'evil-insert
   "n" #'evil-append)

  (general-define-key
   :states 'motion
   :prefix "s"
   ;; TODO: Leave original Qwerty 'v' as Dvorak 'v' or change?
   "v" #'evil-visual-char
   "V" #'evil-visual-line)

  (general-define-key
   :prefix "s"
   "" (list :ignore t
            :which-key "Evil States"))

  ;; ┌────────────────────────────────┐
  ;; │ Undo Tree                      │
  ;; └────────────────────────────────┘

  (imp:eval:after undo-tree
    (general-define-key
     :states 'normal
     "h" #'undo-tree-undo
     "n" #'undo-tree-redo
     "t" #'undo-tree-visualize
     "c" #'undo-tree-switch-branch

     ;; TODO: Keybinds for these maps:
     ;; `undo-tree-visualizer-selection-mode-map'
     ;; `undo-tree-visualizer-mode-map'
     ))

  ;; ┌────────────────────────────────┐
  ;; │ Mode: Org-Mode                 │
  ;; └────────────────────────────────┘

  (imp:eval:after (:and org evil-org)
    (general-define-key
     :states 'normal
     :keymaps '(evil-org-mode-map)
     ;; Special Org Version of `evil-open-below'.
     :prefix "s"
     "t" #'evil-org-open-below)

    (general-define-key
     :keymaps '(evil-org-mode-map)
     :prefix "s"
     "" (list :ignore t
              :which-key "Evil States"))))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'config 'user 'keyboard)

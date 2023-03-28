;;; mantle/config/keybinds/+evil.el --- Evil's own keybinds. -*- lexical-binding: t; -*-
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
;; Evil's own keybinds.
;;
;;; Code:



;;------------------------------------------------------------------------------
;; Infixes : Evil
;;------------------------------------------------------------------------------

(imp:eval:after (:and evil
                      (:keybinds user general))

  ;;------------------------------------------------------------------------------
  ;; Global
  ;;------------------------------------------------------------------------------
  ;; NOTE: Keep in alphanumerical order!

  (keybind:leader/global:def
   :infix "i"                        ; insert
   "" '(nil :which-key "Insert...")) ; infix's title


  (keybind:leader/global:def
   :infix "f"                      ; files
   "" '(nil :which-key "File...")) ; infix's title


  (keybind:leader/global:def
   :infix "n"                       ; notes
   "" '(nil :which-key "Notes...")) ; infix's title


  (keybind:leader/global:def
   :infix "t"                      ; text
   "" '(nil :which-key "Text...")) ; infix's title


  (keybind:leader/global:def
   :infix "/"                        ; search
   "" '(nil :which-key "Search...")) ; infix's title


  ;;-----------------------------------------------------------------------------
  ;; Local
  ;;-----------------------------------------------------------------------------

  ;;------------------------------
  ;; NOTE:
  ;;------------------------------
  ;; Local Leaders are often defined closer to where they're used.
  ;;
  ;; For example, Python buffers have a few menus under the local leader, and they
  ;; are defined in: "mantle/config/dev-env/languages/python.el"
  ;;
  ;; (keybind:leader/local:def
  ;;  :keymaps 'python-mode-map
  ;;  :infix "i"                        ; insert
  ;;  "" '(nil :which-key "insert...")) ; infix's title
  ;;
  ;; (keybind:leader/local:def
  ;;  :keymaps 'python-mode-map
  ;;  :infix "t"                      ; test
  ;;  "" '(nil :which-key "test...")) ; infix's title
  )


;;------------------------------------------------------------------------------
;; Keybinds: Evil
;;------------------------------------------------------------------------------
;; We're using Emacs, so obivously don't use standard Emacs keybinds.
;; Use Vim keybinds, via Evil.
;; But...
;; Obivously don't use standard Evil keybinds, use something totally different.

(imp:eval:after (:and (:keybinds user general)
                      (:keybinds user general evil))

  ;; ╔═════════════════════════════════════════════════════════════════════════╗
  ;; ║ Undefine Keys                                                           ║
  ;; ╚═════════════════════════════════════════════════════════════════════════╝

  ;; ┌────────────────────────────────┐
  ;; │ Keymap:                        │
  ;; │   - nil/global                 │
  ;; └────────────────────────────────┘
  ;; Undefine some keys so we can redefine them later.
  ;;
  ;; NOTE: Use `override' keymap and you won't have to undefine keys.
  ;; See also:
  ;;   - `keybind:keymaps:override'
  ;;   - `keybind:leader/global:keymaps'
  ;;   - `keybind:leader/local:keymaps'
  ;; Or better, use one of the definers:
  ;;   - `keybind:leader/global:def'
  ;;   - `keybind:leader/local:def'
  ;;   - `keybind:global:def'


  ;; ┌────────────────────────────────┐
  ;; │ Keymap:                        │
  ;; │  - `evil-snipe-mode-map'       │
  ;; │  - `evil-snipe-local-mode-map' │
  ;; └────────────────────────────────┘
  (imp:use-package emacs
    :when  (imp:flag? :keybinds +evil)
    :after (:and evil-snipe evil evil-collection)

    :general
    ;; Doom or General or someone will complain if bound keys are not
    ;; unbound before being used for prefixes...
    ;; TODO-evil: Who complained? Was it Doom? If so do we need still?
    (:states '(motion visual normal)
     :keymaps '(evil-snipe-mode-map evil-snipe-local-mode-map)
     ;; Unbind 's' and 't' from snipe.
     "s" nil
     "t" nil))


  ;; ┌────────────────────────────────┐
  ;; │ Keymap:                        │
  ;; │   - `evil-org-mode-map'        │
  ;; └────────────────────────────────┘
  (imp:use-package emacs
    :when  (imp:flag? :keybinds +evil)
    :after (:and org evil-org evil evil-collection)

    :general
    (:states 'normal
     :keymaps '(evil-org-mode-map)
     ;; Unbind from `evil-open-below'.
     "o" nil))


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
   :keymaps keybind:keymaps:override
   :states '(motion visual normal)
   ;; ──┬────────────────
   ;;   │ ↑ ↓ ← →
   ;; ──┴────────────────
   "." #'evil-previous-line
   "e" #'evil-next-line
   "o" #'evil-backward-char
   "u" #'evil-forward-char)

  (general-define-key
   :keymaps keybind:keymaps:override
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

  (general-define-key
   :keymaps keybind:keymaps:override
   :states 'normal
   :prefix "s"
   "" (list :ignore t
            :which-key "Evil States")

   "h" #'evil-insert
   "n" #'evil-append
   "t" #'evil-open-below
   "c" #'evil-open-above
   "H" #'evil-insert-line
   "N" #'evil-append-line
   "T" #'evil-replace-state)

  (general-define-key
   :keymaps keybind:keymaps:override
   :states 'visual
   :prefix "s"
   "h" #'evil-insert
   "n" #'evil-append)

  (general-define-key
   :keymaps keybind:keymaps:override
   :states 'motion
   :prefix "s"
   ;; TODO: Leave original Qwerty 'v' as Dvorak 'v' or change?
   "v" #'evil-visual-char
   "V" #'evil-visual-line))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'config 'keybinds '+evil)

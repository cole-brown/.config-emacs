;;; mantle/config/meow.el --- 'Less is More' Modal Editing -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2022-07-12
;; Modified:   2023-02-09
;; URL:        https://github.com/cole-brown/.config-emacs
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Meow: 'Less is More' Modal Editing
;;
;; https://github.com/meow-edit/meow
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Meow
;;------------------------------------------------------------------------------

(imp:eval:after meow

  ;;------------------------------
  ;; NOTE:
  ;;------------------------------
  ;; Meow's 5 states: `normal', `insert', `motion', `keypad', and `beacon'.
  ;; Evil has... 7??: `normal', `insert', `motion', `replace', `emacs', `operator-pending', and `visual'.
  ;;
  ;; For suggested defaults for Dvorak, see:
  ;;   https://github.com/meow-edit/meow/blob/master/KEYBINDING_DVORAK.org
  ;; I am... ignoring the suggestions.

  ;; ╔═════════════════════════════════════════════════════════════════════════╗
  ;; ║ Undefine Keys                                                           ║
  ;; ╚═════════════════════════════════════════════════════════════════════════╝

  (global-unset-key (kbd "C-s")) ; `isearch-forward'


  ;; ╔═════════════════════════════════════════════════════════════════════════╗
  ;; ║ (Re)define Keys                                                         ║
  ;; ╚═════════════════════════════════════════════════════════════════════════╝

  (mantle:meow:orphan:bind "C-k"
                           kill-line
                           meow--kbd-kill-line)
  ;; TODO: Redefine _all_ meow keybinds??
  ;; TODO: Or... allow to define as "just use the functions"?


  ;; ╔═════════════════════════════════════════════════════════════════════════╗
  ;; ║ Define Keys                                                             ║
  ;; ╚═════════════════════════════════════════════════════════════════════════╝

  ;; ┌────────────────────────────────┐
  ;; │ Meow States                    │
  ;; └────────────────────────────────┘
  ;; https://github.com/meow-edit/meow/blob/master/TUTORIAL.org#states

  ;; ──┬────────────────
  ;;   │ `normal'
  ;; ──┴────────────────
  ;; The default state; most commands are bound here.

  (meow-define-keys
      'insert
    ;; '("<escape>" . meow-insert-exit) ; already bound by `meow'
    '("C-s" . meow-insert-exit)
    ;; "C-g" stays as `keyboard-quit'
    )


  ;; ──┬────────────────
  ;;   │ `insert'
  ;; ──┴────────────────
  ;; The "insert letters into buffer" state.
  ;;
  ;; IJKL position keys (shifted-WASD right-hand, index on home key)
  ;;   - Except Dvorak, so CHTN keys.
  ;; H & ; (Dvorak D & S) are "extra left" and "extra right", basically.

  (defvar mantle:meow/keymap:insert:enter
    (let ((map (make-sparse-keymap)))
      (define-key map "h" #'meow-insert)
      (define-key map "n" #'meow-append)
      (define-key map "c" #'meow-open-above)
      (define-key map "t" #'meow-open-below)
      (define-key map "s" #'meow-change)
      map)
    "Keymap for entering `meow-insert-mode' state.")

  (meow-normal-define-key
   (cons "s" mantle:meow/keymap:insert:enter))


  ;; ──┬────────────────
  ;;   │ `motion'
  ;; ──┴────────────────
  ;; Transitive state after(/when?) using a motion command and it gives you numbers/whatever to jump to.

  ;; These should be ignored as they are our cancel/exit keys.
  (meow-motion-overwrite-define-key
   '("C-s"      . ignore)
   '("<escape>" . ignore)
   '("C-g"      . ignore))


  ;; ──┬────────────────
  ;;   │ `keypad'
  ;; ──┴────────────────
  ;; "The state used for executing commands without modifier keys."
  ;; https://github.com/meow-edit/meow/blob/master/TUTORIAL.org#keypad
  ;;
  ;; NOTE:
  ;;   1. Meow enters `keypad' state when SPC (leader key) is pressed in NORMAL or MOTION state.
  ;;   2. In `keypad' state, single keys will be _translated_:
  ;;      - Start with 'x' / 'h' / 'c' / 'm' / 'g' will begin with 'C-x' / 'C-h' / 'C-c' / 'M-' / 'C-M-' respectively.
  ;;      - Any other key will start with itself, and temporarily activate the leader keymap.
  ;;
  ;; The following keys will act according to following rules:
  ;;   - 'm' will be translated to 'M-'.
  ;;   - 'g' will be translated to 'C-M-'.
  ;;   - Any key following a prefix like 'm' or 'g' is interpreted as 'C-<key>'.
  ;;   - 'SPC' stands for literal prefix, means that the key will not be modified with 'C-'.
  ;;   - If the translation results in an undefined binding, the last key will fallback
  ;;     to an unmodified version. (e.g. If 'C-c C-a' is undefined, fallback to 'C-c a')
  ;;
  ;; Some examples (assuming in `normal' state):
  ;;   | Input       | Translation          | Explanation
  ;;   |-------------+----------------------+--------------
  ;;   | SPC a       | 'a' in leader keymap | leader map default is mode-specific-map, C-c
  ;;   | SPC c t t   | C-c C-t C-t          | start with 'c' as 'C-c'
  ;;   | SPC x m t   | C-x M-t              | 'm' as meta prefix ('M-')
  ;;   | SPC g x     | C-M-x                | 'g' as control + meta prefix ('C-M-')
  ;;   | SPC x SPC p | C-x p                | 'SPC' as literal prefix

  ;; TODO-meow-now: have helper check that I don't use the hard-coded ones (x, h, c, m, g)
  ;; TODO-meow: Can I change what keys it claims for this shit, or should I just learn/adjust?


  ;; ──┬────────────────
  ;;   │ `beacon'
  ;; ──┴────────────────
  ;; "The state used for applying kmacro to multiple places quickly."
  ;; It's kinda like multiple-cursors, but it works differently."


  ;; ┌────────────────────────────────┐
  ;; │ Misc.                          │
  ;; └────────────────────────────────┘

  (meow-leader-define-key
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   ;; TODO-meow: Does "hh" work as a keybind?
   '("hk" . meow-keypad-describe-key)
   '("hh" . meow-cheatsheet))

  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)

   '("-" . negative-argument)

   '("'" . repeat)

   '("<escape>" . ignore))


  ;; ┌────────────────────────────────┐
  ;; │ Movement & Selection           │
  ;; └────────────────────────────────┘
  ;; ESDF position keys (shifted-WASD left-hand, index on home key)
  ;;   - Except Dvorak, so .oeu/>OEU keys.
  ;; A & G (Dvorak A & I) are "extra left" and "extra right", basically.
  ;;   - (evil) example:
  ;;     - "shift left" (O) is `evil-backward-word-end' which goes back to the
  ;;       end of the previous word.
  ;;     - "shift extra left" (A) is `evil-backward-word-begin' which goes back
  ;;       further, to the beginning of the previous word.

  ;; ──┬───────────────────────────────
  ;;   │ Movement in Insert State
  ;; ──┴───────────────────────────────
  (meow-define-keys
      'insert
   ;; ──┬────────────────
   ;;   │ ↑ ↓ ← →
   ;; ──┴────────────────
   '("M-." . meow-prev)
   '("M-e" . meow-next)
   '("M-o" . meow-left)
   '("M-u" . meow-right))


  ;; ──┬───────────────────────────────
  ;;   │ Normal Movement & Selection
  ;; ──┴───────────────────────────────
  (meow-normal-define-key
   ;; ──┬────────────────
   ;;   │ ↑ ↓ ← →
   ;; ──┴────────────────
   ;; Do we want non-selecting movement keys?
   ;;   - To make `M-x meow-tutor` easier to start, yes.
   '("M-." . meow-prev)
   '("M-e" . meow-next)
   '("M-o" . meow-left)
   '("M-u" . meow-right)

   ;; ↑ / prev / up / ...expand?
   '("." . meow-prev-expand)
   '(">" . meow-bounds-of-thing) ; expand to bounds of thing?

   ;; ↓ / next / down / ...contract?
   '("e" . meow-next-expand)
   '("E" . meow-inner-of-thing) ; contract to inner bounds of thing?

   ;; ← / back / left
   '("o" . meow-left-expand)
   '("O" . meow-back-word)
   '("a" . meow-back-symbol)
   '("A" . meow-beginning-of-thing)

   ;; → / forward / right
   '("u" . meow-right-expand)
   '("U" . meow-next-word)
   '("i" . meow-next-symbol)
   '("I" . meow-end-of-thing)

  ;; lines
  '("C-o" . meow-join) ; beginning of line (effectively)
  '("C-u" . meow-line) ; end of line (effectively)
  ;; TODO: (smart) beginning/end of lines?

   ;; region / selection...
   '("m"  . meow-mark-word)
   '("M"  . meow-mark-symbol)
   '(";"  . meow-reverse)
   '("g"  . meow-cancel-selection)
   ;; I guess "d" is some meow command menu?
   '("dd" . meow-block)
   '("db" . meow-to-block)
   '("dl" . meow-goto-line)
   '("ds" . meow-grab)      ; create secondary selection
   '("dr" . meow-swap-grab) ; swap region and secondary selection
   '("dz" . meow-pop-selection)

   ;; cut / copy / paste / kill / yank / violence
   ;;   - cut / kill   = qwerty('x') = dvorak('q')
   ;;   - copy / save  = qwerty('c') = dvorak('j')
   ;;   - paste / yank = qwerty('v') = dvorak('k')
   '("q"   . meow-kill)
   '("j"   . meow-save)
   '("k"   . meow-yank)
   '("K"   . meow-replace)   ; Replace current selection with yank.
   '("C-k" . meow-sync-grab) ; Sync grab to region.

   ;; delete / kill
   '("x" . meow-delete)
   '("X" . meow-backward-delete)

   ;; scroll
   '("C-." . scroll-down-command)
   '("C-e" . scroll-up-command))


  ;; ┌────────────────────────────────┐
  ;; │ Search & Movement              │
  ;; └────────────────────────────────┘
  (meow-normal-define-key
   '("," . meow-find)    ; search (forward) and select to the end of that char
   '("<" . meow-till)    ; search (forward) and select to the end of that char
   '("v" . meow-visit)   ; prompt for regex, search (forward) and select it
   '("V" . meow-search)) ; search (forward) for (unprompted) regex and select it


  ;; ┌────────────────────────────────┐
  ;; │ Undo                           │
  ;; └────────────────────────────────┘
  ;; TODO-meow: Integration with `undo-tree'?

  ;;  '("u" . meow-undo)
  ;;  '("U" . meow-undo-in-selection)


  ;; ╔═════════════════════════════════════════════════════════════════════════╗
  ;; ║ Enable Meow!                                                            ║
  ;; ╚═════════════════════════════════════════════════════════════════════════╝
  (meow-global-mode +1))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'config 'keybinds '+meow)

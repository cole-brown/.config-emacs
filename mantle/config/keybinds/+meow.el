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
  ;; ║ Define Keys                                                             ║
  ;; ╚═════════════════════════════════════════════════════════════════════════╝

  ;; ┌────────────────────────────────┐
  ;; │ Meow States                    │
  ;; └────────────────────────────────┘
  ;; IJKL position keys (shifted-WASD right-hand, index on home key)
  ;;   - Except Dvorak, so CHTN keys.
  ;; H & ; (Dvorak D & S) are "extra left" and "extra right", basically.

  ;; `normal'

  (meow-define-keys
      'insert
    ;; '("<escape>" . meow-insert-exit) ; already bound by `meow'
    '("C-s" . meow-insert-exit)
    ;; "C-g" stays as `keyboard-quit'
    )


  ;; `insert'

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


  ;; `motion': Transitive state after(/when?) using a motion command and it gives you numbers/whatever to jump to.
  ;; TODO-meow: Can I have use this for multiple defined keys, or do I need to...
  (meow-motion-overwrite-define-key
   '("C-s"       . ignore)
   '("<escape>" . ignore)
   '("C-g"      . ignore))

  ;; `keypad': TODO-meow Who/what/where/when/why/how is this state???

  ;; `beacon': TODO-meow Who/what/where/when/why/how is this state???


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

  (meow-normal-define-key
   ;; ──┬────────────────
   ;;   │ ↑ ↓ ← →
   ;; ──┴────────────────
   ;; ↑ / prev / up / ...expand?
   '("."   . meow-prev)
   '("C-." . meow-prev-expand)
   '(">"   . meow-bounds-of-thing) ; expand to bounds of thing?
   '("M-." . meow-block)    ; TODO-meow: ???
   '("M->" . meow-to-block) ; TODO-meow: ???

   ;; ↓ / next / down / ...contract?
   '("e"   . meow-next)
   '("C-e" . meow-next-expand)
   '("E"   . meow-inner-of-thing) ; contract to inner bounds of thing?

   ;; ← / back / left
   '("o"   . meow-left)
   '("C-o" . meow-left-expand)
   '("O"   . meow-back-word)
   '("a"   . meow-back-symbol)
   '("A"   . meow-beginning-of-thing)

   ;; → / forward / right
   '("u"   . meow-right)
   '("C-u" . meow-right-expand)
   '("U"   . meow-next-word)
   '("i"   . meow-next-symbol)
   '("I"   . meow-end-of-thing)

   ;; region / selection...
   '(";"  . meow-reverse)
   '("dd" . meow-line)
   '("dl" . meow-goto-line)
   '("dx" . meow-cancel-selection)
   '("ds" . meow-grab)      ; create secondary selection
   '("dr" . meow-swap-grab) ; swap region and secondary selection
   '("dz"  . meow-pop-selection)

   ;; cut / copy / paste / kill / yank / violence
   ;;   - cut / kill   = qwerty('x') = dvorak('q')
   ;;   - copy / save  = qwerty('c') = dvorak('j')
   ;;   - paste / yank = qwerty('v') = dvorak('k')
   '("q" . meow-kill)
   '("j" . meow-save)
   '("k" . meow-yank)
   '("K" . meow-replace) ; Replace current selection with yank.
   '("C-k" . meow-sync-grab) ; Sync grab to region.

   ;; delete / kill
   '("x" . meow-delete)
   '("X" . meow-backward-delete)

  ;; lines
  '("j" . meow-join)
  ;; TODO: (smart) beginning/end of lines?

  ;; TODO: scroll?
  )


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

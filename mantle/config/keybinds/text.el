;;; text.el --- Keybinds for Editting Text -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2022-11-14
;; Modified:   2022-11-14
;; URL:        https://github.com/cole-brown/.config-emacs
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Keybinds for Editting Text
;;
;;; Code:


;; TODO: move `buffer' shit to `buffer'.

(require 'hydra)


;;------------------------------------------------------------------------------
;; Line Joining Hydra
;;------------------------------------------------------------------------------

;; Call `hydra:join-lines/body' to enter.
(defhydra buffer:hydra:join-lines (:color red  ;; Allow & quit on non-hydra-heads.
                                   :hint none) ;; no hint - just docstr
  "
Join lines.
_o_: ?o?     _c_: ?c?
_O_: ?O?     _t_: ?t?"
  ;;---
  ;; NOTE: Arrow Meanings:
  ;;---
  ;; ↑: Above line.
  ;; ↓: Below line.

  ;;------------------------------
  ;; Emacs Functions
  ;;------------------------------
  ("c" #'join-line               "↑ `join-line' (Trim)")
  ("t" (emacs:cmd (join-line 1)) "↓ `join-line' (Trim)")

  ;;------------------------------
  ;; Evil Functions
  ;;------------------------------
  ("o" #'evil-join            (format "%-50s" "↓ `evil-join' (Smart Comments)"))
  ("O" #'evil-join-whitespace (format "%-50s" "↓ `evil-join-whitespace' (As-Is)")))


;;------------------------------------------------------------------------------
;; Centering Helpers
;;------------------------------------------------------------------------------

;; TODO: move to buffer.
;;;###autoload
(defun buffer:region:active? ()
  "Return non-nil if selection is active.
Detects evil visual mode as well."
  (declare (side-effect-free t))
  (or (use-region-p)
      (and (bound-and-true-p evil-local-mode)
           (evil-visual-state-p))))


;; TODO: move to buffer.
;;;###autoload
(defun buffer:region:beginning ()
  "Return beginning position of selection.
Uses `evil-visual-beginning' if available."
  (declare (side-effect-free t))
  (or (and (bound-and-true-p evil-local-mode)
           (markerp evil-visual-beginning)
           (marker-position evil-visual-beginning))
      (region-beginning)))


;; TODO: move to buffer.
;;;###autoload
(defun buffer:region:end ()
  "Return end position of selection.
Uses `evil-visual-end' if available."
  (declare (side-effect-free t))
  (if (bound-and-true-p evil-local-mode)
      evil-visual-end
    (region-end)))


;; TODO: move to buffer.
(defun buffer:cmd:center/width (full-width)
  "Center region (or line) on width of FULL-WIDTH.

Set `fill-column' to FULL-WIDTH then invokes `center-region', or
`center-line' if there is no active region."
  (interactive "nColumn Width: ")
  (let ((fill-column full-width))
    (if (buffer:region:active?)
        (center-region (buffer:region:beginning)
                       (buffer:region:end))
      (center-line))))


;; TODO: move to buffer.
(defun buffer:cmd:center/to (center-column)
  "Center region (or line) on width of CENTER-COLUMN * 2.

Set `fill-column' to CENTER-COLUMN * 2 then invokes `center-region', or
`center-line' if there is no active region."
  (interactive "nCenter at Column: ")
  (let ((fill-column (* center-column 2)))
    (if (buffer:region:active?)
        (center-region (buffer:region:beginning)
                       (buffer:region:end))
      (center-line))))


;;------------------------------------------------------------------------------
;; Align Regex Helpers
;;------------------------------------------------------------------------------
;; `Align-Regex' helper function idea came from this nice chap:
;; http://pragmaticemacs.com/emacs/aligning-text/

(defun buffer:cmd:align/before (start end text)
  "Align columns by whitespace before TEXT.

E.g. with text \"+=\" and region:
  Jeff.Jet(jeff.it).onClick += OnJeffClick;
  Jeff.Jet(jefferson.it).onClick += OnJeffersonClick;
becomes
  Jeff.Jet(jeff.it).onClick      += OnJeffClick;
  Jeff.Jet(jefferson.it).onClick += OnJeffersonClick;

Align currently selected region (if interactive), or region indicated by START
and END."
  (interactive "r\nsAlign Before: ")

  (let ((regexp (rx-to-string `(sequence
                                ;; target group: whitespace before input text
                                (group (zero-or-more whitespace))
                                ,text))))
    (align-regexp start end
                  regexp
                  ;; target group 1, min spacing 1, no repeat.
                  1 1 nil)))


(defun buffer:cmd:align/after (start end text)
  "Align columns by whitespace after TEXT.

E.g. with text \"+=\" and region:
  Jeff.Jet(jeff.it).onClick += OnJeffClick;
  Jeff.Jet(jefferson.it).onClick += OnJeffersonClick;
becomes
  Jeff.Jet(jeff.it).onClick +=      OnJeffClick;
  Jeff.Jet(jefferson.it).onClick += OnJeffersonClick;

Align currently selected region (if interactive), or region indicated by START
and END."
  (interactive "r\nsAlign After: ")

  (let ((regexp (rx-to-string `(sequence
                                ,text
                                ;; target group: whitespace after input text
                                (group (zero-or-more whitespace))))))
    (align-regexp start end
                  regexp
                  ;; target group 1, min spacing 1, no repeat.
                  1 1 nil)))


;;------------------------------------------------------------------------------
;; Fill/Unfill Commands, Functions, Hydras
;;------------------------------------------------------------------------------

(defun buffer:cmd:fill/paragraph/unfill ()
  "Unfill paragraph.

This is actually the inverse of `fill-paragraph'. Take a multi-line paragraph
and makes it into a single line of text.

from: nhoffman http://nhoffman.github.io/.emacs.d/#org40b27e4
  which is from: http://defindit.com/readme_files/emacs_hints_tricks.html
    which is from: Stefan Monnier <foo at acm.org>
      which is probably from the turtles that go all the way down"
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))


(defun int<buffer>:fill/paragraph/fn-for-mode ()
  "Mode-aware fill-paragraph.

So I only have to bind one thing in the fill hydra. Separated the 'get func' out
here so I can see if in a mode with a special fill for hydra hinting."
  (cond
   ((derived-mode-p 'csharp-mode)
    #'c-fill-paragraph)

   ;; c-mode and all derivatives
   ((and (functionp 'c-buffer-is-cc-mode)
         (c-buffer-is-cc-mode))
    #'c-fill-paragraph)

   ;; elisp, other lispses
   ((or (derived-mode-p 'emacs-lisp-mode)
        (derived-mode-p 'lisp-mode))
    #'lisp-fill-paragraph)
   ;; Might just use `fill-paragraph'?
   ;; Seems to be what "M-q" is using right now?

   ;; python-mode
   ((derived-mode-p 'python-mode) #'python-fill-paragraph)

   ;; org-mode
   ((derived-mode-p 'org-mode) #'org-fill-paragraph)

   ;; default to the usual fill-paragraph
   (t #'fill-paragraph)))


(defun buffer:cmd:fill/paragraph/per-mode (&optional justify?)
  "Mode-aware fill-paragraph.

So I only have to bind one thing in the fill prefix map.

If optional JUSTIFY? is non-nil, justify the text filled (see function
`fill-paragraph')."
  (interactive "P")
  (if (called-interactively-p)
      (funcall-interactively (int<buffer>:fill/paragraph/fn-for-mode) justify?)
    (funcall (int<buffer>:fill/paragraph/fn-for-mode) justify?)))


(defun buffer:cmd:fill/region/single-line (&optional justify?)
  "Grab start/end of current line and call `fill-region'.

That is: \"'Fill Region' on just this line, please.\"

If optional JUSTIFY? is non-nil, justify the text filled (see function
`fill-region')."
  (interactive "P")

  (let ((from (save-excursion (beginning-of-line) (point)))
        (to   (save-excursion (end-of-line)       (point))))
    (if (called-interactively-p)
        (funcall-interactively #'fill-region from to justify?)
      (fill-region from to justify?))))


(defun buffer:cmd:fill/dwim/to-column (fill-to-column &optional justify?)
  "Fill line/region based on FILL-TO-COLUMN.

If a region is selected, fills that region as if the `fill-column' variable was
FILL-TO-COLUMN.

If no region is active, fill current line.

If optional JUSTIFY? is non-nil, justify the text filled (see function
`fill-region')."
  (interactive (list
                (read-number "Fill to Column: " 80)
                "P"))

  (let ((fill-column fill-to-column))
    ;; DWIM: Region? Fill that.
    (if (buffer:region:active?)
        ;; Region selected - fill that.
        (fill-region (region-beginning) (region-end) justify?)

      ;; No region? Fill this line.
      (buffer:cmd:fill/region/single-line justify?))))


;;------------------------------------------------------------------------------
;; Keybinds
;;------------------------------------------------------------------------------

;; TODO: Evil vs Emacs keybinds?


(imp:eval:after (:and evil evil-collection)
  ;; NOTE: /mantle/config/keyboard.el has the new general defs

  ;;------------------------------
  ;; Text
  ;;------------------------------
  (keybind:leader/global:def
   :infix (keybind:infix "t")

   "j" (list #'hydra:join-lines/body :which-key "Join Lines..."))


  ;;------------------------------
  ;; Alignment
  ;;------------------------------

  ;; TODO: Should these be in ':' instead? Are they alread in ':' somewhere?
  (keybind:leader/global:def
   :infix (keybind:infix "t a")      ;; text -> alignment
   "" '(nil :which-key "Alignment...") ;; Infix Title

   "a" (list #'buffer:cmd:align-before :which-key "Align Before")

   "o" (list buffer:cmd:align-after :which-key "Align After")

   ";" (list #'align-regexp :which-key "Align Regex")

   "q" (list (elisp:cmd
              (setq current-prefix-arg '(4))
              (call-interactively #'align-regexp))
             :which-key "C-u Align Regex")

   "'" (list #'align :which-key "Align")

   "," (list #'align-current :which-key "Align Current"))


  ;;------------------------------
  ;; Alignment - Centering
  ;;------------------------------
  ;; Centering is... kind of alignment?
  ;; Pull out of alignment if too confusing for my fingers.

  ;; TODO: Should these be in ':' instead? Are they alread in ':' somewhere?
  (keybind:leader/global:def
   :infix (keybind:infix "t a c")      ;; text -> alignment -> centering
   "" '(nil :which-key "Centering...") ;; Infix Title

   "c" (list (elisp:cmd (buffer:cmd:center/width 80))
             :which-key "Center at 40 (80 width)")

   "t" (list #'buffer:cmd:center/to :which-key "Center to Column...")

   "w" (list #'buffer:cmd:center/width :which-key "Center at Width..."))


  ;;------------------------------
  ;; Fill
  ;;------------------------------
  (keybind:leader/global:def
   :infix (keybind:infix "t f")      ;; text -> fill
   "" '(nil :which-key "Fill...") ;; Infix Title

   ;; Regions
   "r" (list #'fill-region
             :which-key "Region")

   "a" (list #'fill-region-as-paragraph
             :which-key "Region as Paragraph")

   "l" (list #'buffer:cmd:fill/region/single-line
             :which-key "Line")

   ;; Paragraphs
   "p" (list #'buffer:cmd:fill/paragraph/per-mode
             :which-key (if (eq (int<buffer>:fill/paragraph/fn-for-mode) #'fill-paragraph)
                            "Default Fill ¶"
                          "Mode-Aware Fill ¶"))

   "i" (list #'fill-individual-paragraphs
             :which-key "Individual ¶")

   "n" (list #'fill-nonuniform-paragraphs
             :which-key "Non-Uniform ¶")

   "d" (list #'fill-paragraph
             :which-key "Default ¶")

   ;; DWIM
   "8" (list (elisp:cmd (buffer:cmd:fill/dwim/to-column 80))
             :which-key "Fill to 80 (line/region)")

   "?" (list #'buffer:cmd:fill/dwim/to-column
             :which-key "Fill to... (line/region)")

   ;; Unfill
   "u" (list #'buffer:cmd:fill/paragraph/unfill
             :which-key "Unfill ¶"))

  )


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'config 'user 'keybinds 'text)

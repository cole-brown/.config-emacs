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


(require 'hydra)


(imp:require :buffer)

;; Require all the optional features needed in case they weren't provided.
(imp:require :str    '+hydra '+case)
(imp:require :buffer '+commands)
(imp:require :buffer '+hydra '+line)


;;------------------------------------------------------------------------------
;; Load Files
;;------------------------------------------------------------------------------

;; Load Unicode line Art Hydras.
(imp:load :feature  '(:input art)
          :filename "art")


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

   ;; ASCII/Unicode Lines Box Art Hydra
   "b" '(art:cmd:box/draw                     :which-key "Unicode Box...")

   ;; Join Lines Hydra
   "j" '((hydra:call buffer:hydra:join-lines) :which-key "Join Lines...")

   ;; Case Conversion Hydra
   "'" '((hydra:call str:hydra:case/body)     :which-key "Case Conversion..."))


  ;;------------------------------
  ;; Alignment
  ;;------------------------------

  ;; TODO: Should these be in ':' instead? Are they alread in ':' somewhere?
  (keybind:leader/global:def
   :infix (keybind:infix "t a")        ;; text -> alignment
   "" '(nil :which-key "Alignment...") ;; Infix Title

   "a" (list #'buffer:cmd:align-before :which-key "Align Before")

   "o" (list #'buffer:cmd:align-after :which-key "Align After")

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
   :infix (keybind:infix "t f")   ;; text -> fill
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


  ;;------------------------------
  ;; Transpose
  ;;------------------------------
  (keybind:leader/global:def
   :infix (keybind:infix "t t")        ;; text -> fill
   "" '(nil :which-key "Transpose...") ;; Infix Title

   ;; Emacs
   "c" (list #'transpose-chars      :which-key "Characters")

   "w" (list #'transpose-words      :which-key "Words")

   "l" (list #'transpose-lines      :which-key "Lines")

   "s" (list #'transpose-sentences  :which-key "Sentences")

   "p" (list #'transpose-paragraphs :which-key "Paragraphs")

   "x" (list #'transpose-sexps      :which-key "S-Expressions")

   ;; Org-Mode
   "o" (list #'org-transpose-words                :which-key "Org-Mode Words")

   "e" (list #'org-transpose-element              :which-key "Org-Mode Elements")

   "t" (list #'org-table-transpose-table-at-point :which-key "Org-Mode Table"))


  )


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'config 'user 'keybinds 'text)

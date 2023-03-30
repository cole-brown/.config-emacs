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
;; Keybinds : Meow
;;------------------------------------------------------------------------------

(imp:use-package emacs
  :when  (imp:flag? :keybinds +meow)
  :after meow

  :config

  ;;------------------------------
  ;; `General'
  ;;------------------------------

  (defun mantle:meow/keybind/general:text ()
    "Create the \"Text...\" keybinds in `general' for `meow'."

    (keybind:leader/global:def
      :infix "t"                      ; text
      "" '(nil :which-key "Text...")) ; infix title

    ;;---
    ;; Misc (Hydras)
    ;;---
    (keybind:leader/global:def
      :infix (keybind:infix "t")

      ;; ASCII/Unicode Lines Box Art Hydra
      "b" '(art:cmd:box/draw                                 :which-key "Unicode Box...")

      ;; Join Lines Hydra
      "j" (list (elisp:cmd (hydra:call 'buffer:hydra:join-lines)) :which-key "Join Lines...")

      ;; Case Conversion Hydra
      "'" (list (elisp:cmd (hydra:call 'str:hydra:case))          :which-key "Case Conversion..."))

    ;;---
    ;; Alignment
    ;;---
    (keybind:leader/global:def
      :infix (keybind:infix "t" "a")      ; text -> alignment
      "" '(nil :which-key "Alignment...") ; Infix Title

      "a" (list #'buffer:cmd:align/before :which-key "Align Before")
      "o" (list #'buffer:cmd:align/after  :which-key "Align After")
      ";" (list #'align-regexp            :which-key "Align Regex")
      "q" (list (elisp:cmd
                 (setq current-prefix-arg '(4))
                 (call-interactively #'align-regexp))
                :which-key "C-u Align Regex")
      "'" (list #'align                   :which-key "Align")
      "," (list #'align-current           :which-key "Align Current"))

    ;;---
    ;; Alignment - Centering
    ;;---
    ;; Centering is... kind of alignment?
    ;; Pull out of alignment if too confusing for my fingers.
    (keybind:leader/global:def
      :infix (keybind:infix "t" "a" "c")  ; text -> alignment -> centering
      "" '(nil :which-key "Centering...") ; Infix Title

      "c" (list (elisp:cmd (buffer:cmd:center/width 80)) :which-key "Center at 40 (80 width)")
      "t" (list #'buffer:cmd:center/to                   :which-key "Center to Column...")
      "w" (list #'buffer:cmd:center/width                :which-key "Center at Width..."))


    ;;------------------------------
    ;; Fill
    ;;------------------------------
    (keybind:leader/global:def
      :infix (keybind:infix "t" "f") ; text -> fill
      "" '(nil :which-key "Fill...") ; Infix Title

      ;; Regions
      "r" (list #'fill-region              :which-key "Region")
      "a" (list #'fill-region-as-paragraph :which-key "Region as Paragraph")
      "l" (list #'buffer                   :cmd:fill/region/single-line :which-key "Line")

      ;; Paragraphs
      "p" (list #'buffer:cmd:fill/paragraph/per-mode
                :which-key (if (eq (int<buffer>:fill/paragraph/fn-for-mode) #'fill-paragraph)
                               "Default Fill ¶"
                             "Mode-Aware Fill ¶"))
      "i" (list #'fill-individual-paragraphs :which-key "Individual ¶")
      "n" (list #'fill-nonuniform-paragraphs :which-key "Non-Uniform ¶")
      "d" (list #'fill-paragraph             :which-key "Default ¶")

      ;; DWIM
      "8" (list (elisp:cmd (buffer:cmd:fill/dwim/to-column 80)) :which-key "Fill to 80 (line/region)")
      "?" (list #'buffer:cmd:fill/dwim/to-column                :which-key "Fill to... (line/region)")

      ;; Unfill
      "u" (list #'buffer:cmd:fill/paragraph/unfill :which-key "Unfill ¶"))

    ;;---
    ;; Transpose
    ;;---
    (keybind:leader/global:def
      :infix (keybind:infix "t" "t")      ; text -> fill
      "" '(nil :which-key "Transpose...") ; Infix Title

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
      "t" (list #'org-table-transpose-table-at-point :which-key "Org-Mode Table")))
  ;; (mantle:meow/keybind/general:text)


  ;;------------------------------
  ;; `Transient'
  ;;------------------------------

  (defun mantle:meow/keybind/transient:text ()
    "Create the \"Text...\" keybinds in `transient' for `meow'."

    (transient-define-suffix mantle:meow/transient:text:join-lines ()
      "Join Lines Hydra"
      :key "j"
      :description "Join Lines..."
      (interactive)
      (hydra:call 'buffer:hydra:join-lines))


    (transient-define-suffix mantle:meow/transient:text:case ()
      "Many many CASE CoNvErSiOnS"
      :key "mc"
      :description "Case Conversion..."
      (interactive)
      (hydra:call 'str:hydra:case))


    (transient-define-suffix mantle:meow/transient:text:align/regex/prefix ()
      "`C-u M-x align-regexp'"
      :key "aq"
      :description "C-u Align Regex"
      (interactive)
      (setq current-prefix-arg '(4))
      (call-interactively #'align-regexp))


    (transient-define-suffix mantle:meow/transient:text:center/40 ()
      "Center at 40 (80 width)"
      :key "cc"
      :description "Center at 40 (80 width)"
      (interactive)
      (buffer:cmd:center/width 80))


    (transient-define-suffix mantle:meow/transient:text:fill/paragraph ()
      "Fill Paragraph (possibly mode-aware)"
      :key "fp"
      :description "Hi";(if (eq (int<buffer>:fill/paragraph/fn-for-mode) #'fill-paragraph)
                                        ;"Default Fill ¶"
                                        ;"Mode-Aware Fill ¶")
      (interactive)
      (buffer:cmd:fill/paragraph/per-mode))


    (transient-define-suffix mantle:meow/transient:text:fill/80 ()
      "Fill to 80 (line/region)"
      :key "f8"
      :description "Fill to 80 (line/region)"
      (interactive)
      (buffer:cmd:fill/dwim/to-column 80))


    (transient-define-prefix mantle:meow/transient:text ()
      "Commands for doing things to text..."
      ["Text..."
       ["Alignment"
        ("aa" "Align Before" buffer:cmd:align/before)
        ("ao" "Align After" buffer:cmd:align/after)
        ("a;" "Align Regex" align-regexp)
        (mantle:meow/transient:text:align/regex/prefix)
        ("a'"  "Align" align)
        ("a,"  "Align Current" align-current)]

       ["Centering"
        (mantle:meow/transient:text:center/40)
        ("ct" "Center to Column..." buffer:cmd:center/to)
        ("cw" "Center at Width..."  buffer:cmd:center/width)]

       ["Fill"
        ;; Regions
        ("fr" "Region"              fill-region)
        ("fa" "Region as Paragraph" fill-region-as-paragraph)
        ("fl" "Line"                buffer:cmd:fill/region/single-line)

        ;; Paragraphs
        (mantle:meow/transient:text:fill/paragraph)
        ("fi" "Individual ¶"  fill-individual-paragraphs)
        ("fn" "Non-Uniform ¶" fill-nonuniform-paragraphs)
        ("fd" "Default ¶"     fill-paragraph)

        ;; DWIM
        (mantle:meow/transient:text:fill/80)
        ("f?" "Fill to... (line/region)" buffer:cmd:fill/dwim/to-column)

        ;; Unfill
        ("fu" "Unfill ¶" buffer:cmd:fill/paragraph/unfill)]

       ["Transpose"
        ;; Emacs
        ("tc" "Characters"    transpose-chars)
        ("tw" "Words"         transpose-words)
        ("tl" "Lines"         transpose-lines)
        ("ts" "Sentences"     transpose-sentences)
        ("tp" "Paragraphs"    transpose-paragraphs)
        ("tx" "S-Expressions" transpose-sexps)

        ;; Org-Mode
        ("to" "Org-Mode Words"    org-transpose-words)
        ("te" "Org-Mode Elements" org-transpose-element)
        ("tt" "Org-Mode Table"    org-table-transpose-table-at-point)]

       ["Misc"
        ("b" "Unicode Box..." art:cmd:box/draw) ; ASCII/Unicode Lines Box Art Hydra
        (mantle:meow/transient:text:join-lines) ; Join Lines Hydra
        (mantle:meow/transient:text:case)
        ("SPC" "Just One Space™" just-one-space)]

       ])
    ;; (mantle:meow/transient:text)

    (meow-leader-define-key '("t" . mantle:meow/transient:text)))


  ;;------------------------------
  ;; Actually Create Keybinds:
  ;;------------------------------

  (if (imp:provided? :keybinds 'user 'general 'meow)
      (mantle:meow/keybind/general:text)
    (mantle:meow/keybind/transient:text)))


;;------------------------------------------------------------------------------
;; Keybinds : Evil
;;------------------------------------------------------------------------------

(imp:eval:after (:and evil evil-collection)

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
    "'" '((hydra:call str:hydra:case)     :which-key "Case Conversion..."))


  ;;------------------------------
  ;; Alignment
  ;;------------------------------

  ;; TODO-evil: Should these be in ':' instead? Are they alread in ':' somewhere?
  ;;            What even is ':' in evil anyways, exactly?
  (keybind:leader/global:def
    :infix (keybind:infix "t" "a")      ; text -> alignment
    "" '(nil :which-key "Alignment...") ; Infix Title

    "a" (list #'buffer:cmd:align/before :which-key "Align Before")
    "o" (list #'buffer:cmd:align/after  :which-key "Align After")
    ";" (list #'align-regexp            :which-key "Align Regex")
    "q" (list (elisp:cmd
               (setq current-prefix-arg '(4))
               (call-interactively #'align-regexp))
              :which-key "C-u Align Regex")
    "'" (list #'align                   :which-key "Align")
    "," (list #'align-current           :which-key "Align Current"))


  ;;------------------------------
  ;; Alignment - Centering
  ;;------------------------------
  ;; Centering is... kind of alignment?
  ;; Pull out of alignment if too confusing for my fingers.

  ;; TODO: Should these be in ':' instead? Are they alread in ':' somewhere?
  (keybind:leader/global:def
    :infix (keybind:infix "t" "a" "c")  ; text -> alignment -> centering
    "" '(nil :which-key "Centering...") ; Infix Title

    "c" (list (elisp:cmd (buffer:cmd:center/width 80)) :which-key "Center at 40 (80 width)")
    "t" (list #'buffer:cmd:center/to                   :which-key "Center to Column...")
    "w" (list #'buffer:cmd:center/width                :which-key "Center at Width..."))


  ;;------------------------------
  ;; Fill
  ;;------------------------------
  (keybind:leader/global:def
    :infix (keybind:infix "t" "f") ; text -> fill
    "" '(nil :which-key "Fill...") ; Infix Title

    ;; Regions
    "r" (list #'fill-region              :which-key "Region")
    "a" (list #'fill-region-as-paragraph :which-key "Region as Paragraph")
    "l" (list #'buffer                   :cmd:fill/region/single-line :which-key "Line")

    ;; Paragraphs
    "p" (list #'buffer:cmd:fill/paragraph/per-mode
              :which-key (if (eq (int<buffer>:fill/paragraph/fn-for-mode) #'fill-paragraph)
                             "Default Fill ¶"
                           "Mode-Aware Fill ¶"))
    "i" (list #'fill-individual-paragraphs :which-key "Individual ¶")
    "n" (list #'fill-nonuniform-paragraphs :which-key "Non-Uniform ¶")
    "d" (list #'fill-paragraph             :which-key "Default ¶")

    ;; DWIM
    "8" (list (elisp:cmd (buffer:cmd:fill/dwim/to-column 80)) :which-key "Fill to 80 (line/region)")
    "?" (list #'buffer:cmd:fill/dwim/to-column                :which-key "Fill to... (line/region)")

    ;; Unfill
    "u" (list #'buffer:cmd:fill/paragraph/unfill :which-key "Unfill ¶"))


  ;;------------------------------
  ;; Transpose
  ;;------------------------------
  (keybind:leader/global:def
    :infix (keybind:infix "t" "t")      ; text -> fill
    "" '(nil :which-key "Transpose...") ; Infix Title

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
    "t" (list #'org-table-transpose-table-at-point :which-key "Org-Mode Table")))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'config 'user 'keybinds 'text)

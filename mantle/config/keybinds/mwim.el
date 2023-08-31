;;; mantle/config/keybinds/mwim.el --- Move Where I Mean -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2023-08-31
;; Timestamp:  2023-08-31
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Move Where I Mean
;;
;; I have a set of smarter BOL/EOL (beginning/end of line) function in
;; "core/modules/emacs/buffer/line.el". They are almost good enough, but when
;; adding in knowledge about comments vs code, I found this `mwim' (Move Where I
;; Mean) package for BOL/EOL movement, which was smarter than my smarter BOL/EOL
;; funcs.
;;
;; So try that out instead.
;;
;;; Code:


(imp:require :buffer)


;;------------------------------------------------------------------------------
;; Move Where I Mean!
;;------------------------------------------------------------------------------

;; https://github.com/alezost/mwim.el
;; https://melpa.org/#/mwim
(imp:use-package mwim
  ;;------------------------------
  :bind
  ;;------------------------------

  (;;---
   ;; :map global
   ;;---
   ;; Remap to smarter BOL function for logical lines.
   ([remap move-beginning-of-line] . mwim-beginning)
   ([remap move-end-of-line]       . mwim-end)

   ;;---
   :map visual-line-mode-map
   ;;---
   ;; Remap to smarter BOL/EOL function for visual lines (in `visual-line-mode-map').
   ([remap beginning-of-visual-line] . mwim-beginning)
   ([remap end-of-visual-line]       . mwim-end)
   ([remap move-beginning-of-line]   . mwim-beginning)
   ([remap move-end-of-line]         . mwim-end)))


;;------------------------------------------------------------------------------
;; Keybinds : Meow
;;------------------------------------------------------------------------------

(imp:use-package mwim
  :when  (imp:flag? :keybinds +meow)
  :after meow

  ;;------------------------------
  :init
  ;;------------------------------

  (defun mantle:advice:mwim+meow:selection (func &rest args)
    "Graft `meow'-ish selection onto `mwim' movement functions.

Indended as `:around' advice for FUNCs like:
  - `mwim-beginning'
  - `mwim-end'
  - etc

Usage:
  (advice-add 'mwim-beginning
              :around
              #'mantle:advice:mwim+meow:selection)
  (advice-add 'mwim-end
              :around
              #'mantle:advice:mwim+meow:selection)"
    (let ((point/before (point)))
      ;; Begin a selection region?
      (unless (region-active-p)
        (push-mark-command nil :no-message))

      (prog1 ; Return whatever FUNC returns.
          ;; Do the thing!
          (apply func args)

        ;; Retcon the selection region if you end up with nothing?
        (let ((point/after (point)))
          (when (= (buffer:region:start) (buffer:region:end))
            ;; Select from wherever we started the movement from.
            (goto-char point/before)
            (push-mark-command nil :no-message)
            (goto-char point/after))))))


  ;;------------------------------
  :config
  ;;------------------------------

  ;; ──┬───────────────────────────────
  ;;   │ Advice: Add `meow' Selection to `mwim'
  ;; ──┴───────────────────────────────

  (advice-add 'mwim-beginning
              :around
              #'mantle:advice:mwim+meow:selection)
  (advice-add 'mwim-end
              :around
              #'mantle:advice:mwim+meow:selection)

  ;; ──┬───────────────────────────────
  ;;   │ Normal Movement & Selection
  ;; ──┴───────────────────────────────

  ;; Change meow binding for BOL/EOL functions.
  ;; "./+meow.el" has bindings:
  ;;   '("C-o" . buffer:cmd:line/smart:move-beginning/visual/select)
  ;;   '("C-u" . buffer:cmd:line/smart:move-end/visual/select)
  (meow-normal-define-key
   ;; lines
   '("C-o" . mwim-beginning)
   '("C-u" . mwim-end)))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'config 'keybinds 'mwim)

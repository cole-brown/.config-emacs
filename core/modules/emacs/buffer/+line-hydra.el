;;; core/modules/emacs/buffer/+line-hydra.el --- Join Lines Hydra -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2022-11-28
;; Timestamp:  2023-06-21
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:


;;------------------------------------------------------------------------------
;; Hydra
;;------------------------------------------------------------------------------

;; Delay until after Hydra is loaded...
(imp:eval:after hydra

  ;;------------------------------------------------------------------------------
  ;; Line Joining Hydra
  ;;------------------------------------------------------------------------------
  (if (imp:flag? :keybinds +evil)
      ;;------------------------------
      ;; Evil?
      ;;------------------------------
      ;; Evil has some of its own line joining functions, so... it gets its own hydra.
      (imp:eval:after (:and evil evil-collection)
        ;; Call `buffer:hydra:join-lines/body' to enter.
        (defhydra buffer:hydra:join-lines (:color red  ;; Allow & quit on non-hydra-heads.
                                           :hint none) ;; no hint - just docstr
          "
Join Lines...
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
          ("t" (elisp:cmd (join-line 1)) "↓ `join-line' (Trim)")

          ;;------------------------------
          ;; Evil Functions
          ;;------------------------------
          ("o" #'evil-join            (format "%-50s" "↓ `evil-join' (Smart Comments)"))
          ("O" #'evil-join-whitespace (format "%-50s" "↓ `evil-join-whitespace' (As-Is)"))))
    ;; (buffer:hydra:join-lines/body)


    ;;------------------------------
    ;; Not Evil?
    ;;------------------------------
    ;; Vanilla Emacs and/or Meow - just standard join lines.

    ;; Call `buffer:hydra:join-lines/body' to enter.
    (defhydra buffer:hydra:join-lines (:color red  ;; Allow & quit on non-hydra-heads.
                                       :hint none) ;; no hint - just docstr
      "
Join Lines...
_._: ?.?
_e_: ?e?"
      ;;---
      ;; NOTE: Arrow Meanings:
      ;;---
      ;; ↑: Above line.
      ;; ↓: Below line.
      ("." #'join-line               "↑ `join-line' (Trim)")
      ("e" (elisp:cmd (join-line 1)) "↓ `join-line' (Trim)"))))
;; (buffer:hydra:join-lines/body)


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :buffer '+hydra '+line)

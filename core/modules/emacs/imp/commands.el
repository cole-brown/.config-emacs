;;; emacs/imp/commands.el -*- lexical-binding: t; -*-


;;                                 ──────────                                 ;;
;; ╔════════════════════════════════════════════════════════════════════════╗ ;;
;; ║                            Command the Imps                            ║ ;;
;; ╚════════════════════════════════════════════════════════════════════════╝ ;;
;;                                   ──────                                   ;;
;;                     Interact with the imps; have fun.                      ;;
;;                                 ──────────                                 ;;


;;------------------------------------------------------------------------------
;; Custom Variables
;;------------------------------------------------------------------------------

(defgroup imp:group nil
  "Automatically-ish commit/push git repos for note, docs, etc."
  :prefix "imp:"
  :group 'tools)


(defcustom imp:output:features:buffer
  "*imp:features*"
  "Name of the buffer for `imp:features:print' to output a pretty-printed tree
of the features imp has provided."
  :group 'imp:group)


;;------------------------------------------------------------------------------
;; Display Features
;;------------------------------------------------------------------------------

(defun imp:cmd:features:print ()
  "Pretty print `imp:features' to a temp buffer."
  (interactive)
  (pp-display-expression imp:features imp:output:features:buffer))
;; (imp:features:print)

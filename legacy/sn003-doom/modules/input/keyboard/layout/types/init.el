;;; input/keyboard/layout/types/init.el -*- lexical-binding: t; -*-


;;                                 ──────────                                 ;;
;; ╔════════════════════════════════════════════════════════════════════════╗ ;;
;; ║                Build & Initialize the Keyboard Layout.                 ║ ;;
;; ╚════════════════════════════════════════════════════════════════════════╝ ;;
;;                                   ──────                                   ;;
;;                        Only for the desired layout.                        ;;
;;                                 ──────────                                 ;;


;;------------------------------------------------------------------------------
;; Layout Type Functions
;;------------------------------------------------------------------------------

(load! "define")


;;------------------------------------------------------------------------------
;; Layout Types (:common, :emacs, :evil)
;;------------------------------------------------------------------------------

;;------------------------------
;; Layout Keyword -> Functions
;;------------------------------

;; Always include the common ones.
(load! "common")

;; Include either evil or emacs keybinds, depending on if evil-mode is being used.
(if (featurep! :editor evil)
    (load! "evil")
  (load! "emacs"))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :input 'keyboard 'layout 'types)

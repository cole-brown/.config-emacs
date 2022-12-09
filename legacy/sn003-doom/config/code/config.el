;;; config/code/config.el -*- lexical-binding: t; -*-


(imp:require :modules 'spy 'hook)
(imp:require :jerky)


;;------------------------------
;; NOTE:
;;----------
;; 1) Set up some vars, defaults.
;; 2) Call out to other files to set up other modes.
;;    - Base 'prog-mode' should be set up first.
;;------------------------------


;;------------------------------------------------------------------------------
;; Auto-Formatting
;;------------------------------------------------------------------------------

;; TODO: Change into a func call so various mode configs can do it themselves?

;; Update doom's presets for :format/+onsave's enabled modes.
(setq +format-on-save-enabled-modes
      ;; Starting with `not' inverts this list's meaning to "everything except:"
      '(not emacs-lisp-mode  ; elisp's mechanisms are good enough
            sql-mode         ; sqlformat is currently broken
            tex-mode         ; latexindent is broken
            latex-mode
            python-mode      ; Don't even want to know what this would do if it has one.
            csharp-mode))    ; TODO: Should I enable for this?


;;------------------------------------------------------------------------------
;; Comments
;;------------------------------------------------------------------------------

;; TODO: Should be in keybinds?
;; TODO: Unmap both as `g c' works good?

;; Comment/Uncomment
;; C-x C-; is super awkward on dvorak w/ CAPS-as-ctrl...
;; The evil way:
;;   gc -> start a commenting thing...
;;   gsj -> choose from upcoming line based on letters assigned.
;; or
;;   Vttgc -> Visual mode (V), two lines forward (tt), comment region (gc)
;;   Ctrl+SPC ttgc -> Visual mode (Ctrl+SPC), two lines forward (tt), comment region (gc)
;; or probably a lot of other ones.
(map!
 ;; Unmap the one I don't want.
 "C-x C-;" nil

 ;; "C-/" was 'undo, but I'm used to "C-S--" aka "C-_"
 :desc "Comment/Uncomment" "C-/" #'evilnc-comment-or-uncomment-lines)


;;------------------------------------------------------------------------------
;; Programming Mode Configs
;;------------------------------------------------------------------------------

(imp:load :feature  '(:dot-emacs config code +c-and-cpp)
          :path     (imp:path:current:dir/relative :dot-emacs)
          :filename "+c-and-cpp")
(imp:load :feature  '(:dot-emacs config code +c-sharp)
          :path     (imp:path:current:dir/relative :dot-emacs)
          :filename "+c-sharp")
(imp:load :feature  '(:dot-emacs config code +python)
          :path     (imp:path:current:dir/relative :dot-emacs)
          :filename "+python")
(imp:load :feature  '(:dot-emacs config code +fish)
          :path     (imp:path:current:dir/relative :dot-emacs)
          :filename "+fish")



;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :dot-emacs 'config 'code)

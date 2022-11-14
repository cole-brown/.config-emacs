;;; config/spy/keybinds.el -*- lexical-binding: t; -*-


(require 'hydra)


(imp:require :modules 'spy 'io 'signature)
(imp:require :str 'hydra 'case)

(imp:load :feature  '(:dot-emacs config spy art)
          :path     (imp:path:join dot-emacs:path:config "spy")
          :filename "art")

(imp:load :feature  '(:dot-emacs config spy join)
          :path     (imp:path:join dot-emacs:path:config "spy")
          :filename "join")


;;------------------------------------------------------------------------------
;; Evil Spy Stuff.
;;------------------------------------------------------------------------------
;; See Doom's keybindings for how to do complicated things:
;; https://github.com/hlissner/doom-emacs/blob/develop/modules/config/default/+evil-bindings.el
(map! :leader       ; Be under the Doom SPC leader.

      ;;------------------------------
      ;; Start of 'spy' prefix "-"
      ;;------------------------------
      (:prefix ("-" . "spy") ; Not my first choice but no one uses dash,
                                        ; and it's easy on Dvorak.

       ;;------------------------------
       ;; Transpose
       ;;------------------------------
       (:prefix ("t" . "Transpose")
        ;; Emacs
        :desc "Characters"                  "c" #'transpose-chars
        :desc "Words"                       "w" #'transpose-words
        :desc "Lines"                       "l" #'transpose-lines
        :desc "Sentences"                   "s" #'transpose-sentences
        :desc "Paragraphs"                  "p" #'transpose-paragraphs
        :desc "S-Expressions"               "x" #'transpose-sexps

        ;; Org-Mode
        :desc "Org-Mode Words"              "o" #'org-transpose-words
        :desc "Org-Mode Elements"           "e" #'org-transpose-element
        :desc "Org-Mode Table"              "t" #'org-table-transpose-table-at-point)


       ;;------------------------------
       ;; Box Drawning
       ;;------------------------------
       ;; Hydra
       :desc "Unicode Box"                  "b" #'spy:art.box/draw


       ;;------------------------------
       ;; Case Conversion
       ;;------------------------------
       ;; Hydra
       :desc "Case Conversion"              "'" #'str:hydra:case/body


       ;;------------------------------
       ;; File/Dir Names
       ;;------------------------------
       :desc "Copy Buffer's File Name"    "k" #'spy:cmd:file-or-dir-name/clipboard
       :desc "Copy Buffer's Dir Name"     "K" (cmd!! #'spy:cmd:file-or-dir-name/clipboard '(4)) ;; Call with simulated C-u prefix arg.
       ))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :dot-emacs 'config 'keybinds 'spy-leader)

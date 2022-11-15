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
       ;; File/Dir Names
       ;;------------------------------
       :desc "Copy Buffer's File Name"    "k" #'spy:cmd:file-or-dir-name/clipboard
       :desc "Copy Buffer's Dir Name"     "K" (cmd!! #'spy:cmd:file-or-dir-name/clipboard '(4)) ;; Call with simulated C-u prefix arg.
       ))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :dot-emacs 'config 'keybinds 'spy-leader)

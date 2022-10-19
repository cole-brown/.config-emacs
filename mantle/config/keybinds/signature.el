;;; mantle/config/keybinds/signature.el --- Signatures, Emails, and Such -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <code@brown.dev>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2022-10-18
;; Modified:   2022-10-18
;;
;;; Commentary:
;;
;; Signatures, Emails, and Such
;;
;;; Code:


(imp:require :elisp 'utils 'functions)


;;------------------------------------------------------------------------------
;; Keybinds
;;------------------------------------------------------------------------------

;; NOTE: /mantle/config/keyboard.el has the new general defs
;; NOTE: /legacy/sn003-doom/config/keybinds/spy-leader.el has the old shit


 ;;------------------------------
 ;; Signatures
 ;;------------------------------

;; (general-define-key
;; :states keybind:leader/global:states
(keybind:leader/global:def
 :infix (keybind:infix "i" "s")      ;; insert -> signature
 "" '(nil :which-key "Signature...") ;; Infix Title

 ;; "TODO" signature with timestamp; commented if needed:
 "t" (list (elisp:cmd (signature:insert 'sigil 'todo :timestamp t :comment t))
           :which-key (concat "TODO: " (signature:string 'sigil 'todo)))

 ;; Sigil for Note Prefix.
 "n" (list (elisp:cmd (signature:insert 'sigil 'note))
           :which-key (concat "Note: " (signature:string 'sigil 'note)))


 ;; Signature for the end of an email or something.
 "s" (list (elisp:cmd (signature:insert 'name 'sign))
           :which-key (concat "Sign: " (signature:string 'name 'sign)))

 ;; IDs
 (:prefix ("i" . "ID")
  ;; Just the Sigil.
  "s" (list (elisp:cmd (signature:insert 'id 'sigil))
            :which-key (concat "Sigil: " (signature:string 'id 'sigil)))

  ;; Just the Name.
  "n" (list (elisp:cmd (signature:insert 'id 'name))
            :which-key (concat "Name: " (signature:string 'id 'name)))))

;;------------------------------
;; Emails
;;------------------------------
;; Only show if we have any.

;; work namespace.
(when (signature:exists? 'id 'email :namespace :work)
  (keybind:leader/global:def
   :infix (keybind:infix "i" "s" "e") ;; insert -> signature -> email
   "" '(nil :which-key "Email...")    ;; Infix Title

   "w" (list (elisp:cmd (signature:insert 'id 'email :namespace :work))
             :which-key (concat "work: " (signature:string 'id 'email :namespace :work)))))

   ;; home namespace.
(when (signature:exists? 'id 'email :namespace :home)
  (keybind:leader/global:def
   :infix (keybind:infix "i" "s" "e") ;; insert -> signature -> email
   "" '(nil :which-key "Email...")    ;; Infix Title

   "h" (list (elisp:cmd (signature:insert 'id 'email :namespace :home))
             :which-key (concat "home: " (signature:string 'id 'email :namespace :home)))))

   ;; default namespace.
(when (signature:exists? 'id 'email :namespace :default)
  (keybind:leader/global:def
   :infix (keybind:infix "i" "s" "e") ;; insert -> signature -> email
   "" '(nil :which-key "Email...")    ;; Infix Title

   "c" (list (elisp:cmd (signature:insert 'id 'email :namespace :default))
                :which-key (concat "default: " (signature:string 'id 'email :namespace :default)))))


;;------------------------------
;; Search
;;------------------------------

;; TODO: Search for: sigil, todo...


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'config 'user 'keybinds 'identity)
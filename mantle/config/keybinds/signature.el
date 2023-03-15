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
(imp:require :tools 'signature)


;;------------------------------------------------------------------------------
;; Keybinds : Meow
;;------------------------------------------------------------------------------

(imp:use-package emacs
  :when  (imp:flag? :keybinds +meow)
  :after meow

  :config

  ;; TODO-meow: A transient arg for `:home' or `:work' versions of things?
  ;;            Like, instead of 2 functions & 2 keys for many of the things?


  ;;------------------------------
  ;; Signatures
  ;;------------------------------

  (transient-define-suffix mantle:meow/transient:insert:signature/todo ()
    "\"TODO\" signature with timestamp; commented if needed."
    :key "st"
    :description (concat "TODO: " (signature:string 'sigil 'todo))
    (interactive)
    (signature:insert 'sigil 'todo :timestamp t :comment t))


  (transient-define-suffix mantle:meow/transient:insert:signature/note ()
    "Sigil for Note Prefix"
    :key "sn"
    :description (concat "Note: " (signature:string 'sigil 'note))
    (interactive)
    (signature:insert 'sigil 'note))


  (transient-define-suffix mantle:meow/transient:insert:signature/sign ()
    "Signature for the end of an email or something."
    :key "ss"
    :description (concat "Sign: " (signature:string 'name 'sign))
    (interactive)
    (signature:insert 'name 'sign))


  (transient-define-suffix mantle:meow/transient:insert:signature/id ()
    "Just the Sigil."
    :key "is"
    :description (concat "Sigil: " (signature:string 'id 'sigil))
    (interactive)
    (signature:insert 'id 'sigil))


  (transient-define-suffix mantle:meow/transient:insert:signature/name ()
    "Just the Name."
    :key "in"
    :description (concat "Name: " (signature:string 'id 'name))
    (interactive)
    (signature:insert 'id 'name))


  (transient-define-prefix mantle:meow/transient:insert ()
    "Buffer commands that should be available globally."
    ["Insert..."
     ["Signature"
      (mantle:meow/transient:insert:signature/todo)
      (mantle:meow/transient:insert:signature/note)
      (mantle:meow/transient:insert:signature/sign)]
     ["ID"
      (mantle:meow/transient:insert:signature/id)
      (mantle:meow/transient:insert:signature/name)]
     ["Email"]])
  ;; (mantle:meow/transient:insert)

  (meow-leader-define-key '("i" . mantle:meow/transient:insert))

  ;;------------------------------
  ;; Emails
  ;;------------------------------
  ;; Only show if we have any.

  ;; work namespace
  (when (signature:exists? 'id 'email :namespace :work)
    (transient-define-suffix mantle:meow/transient:insert:email/work ()
      "Work Email"
      :key "ew"
      :description (concat "work: " (signature:string 'id 'email :namespace :work))
      (interactive)
      (signature:insert 'id 'email :namespace :work)))


  ;; home namespace
  (when (signature:exists? 'id 'email :namespace :home)
    (transient-define-suffix mantle:meow/transient:insert:email/home ()
      "Home Email"
      :key "eh"
      :description (concat "home: " (signature:string 'id 'email :namespace :home))
      (interactive)
      (signature:insert 'id 'email :namespace :home)))

  ;; default namespace
  (when (signature:exists? 'id 'email :namespace :default)
    (transient-define-suffix mantle:meow/transient:insert:email/default ()
      "Default Email"
      :key "ed"
      :description (concat "default: " (signature:string 'id 'email :namespace :default))
      (interactive)
      (signature:insert 'id 'email :namespace :default)))


  ;; ...is there a better way to build transient groups dynamically like this?
  ;; This seems a bit of a faff.
  (when (or (functionp #'mantle:meow/transient:insert:email/work)
            (functionp #'mantle:meow/transient:insert:email/home)
            (functionp #'mantle:meow/transient:insert:email/default))
    (transient-append-suffix 'mantle:meow/transient:insert
      '(0 -1) ; Append at end of first group...
      (apply #'vector
             "Email"
             (seq-filter #'identity
                         (list
                          (when (functionp #'mantle:meow/transient:insert:email/work)
                            '(mantle:meow/transient:insert:email/work))
                          (when (functionp #'mantle:meow/transient:insert:email/home)
                            '(mantle:meow/transient:insert:email/home))
                          (when (functionp #'mantle:meow/transient:insert:email/default)
                            '(mantle:meow/transient:insert:email/default)))))))
  ;; (mantle:meow/transient:insert)


  ;;------------------------------
  ;; Search
  ;;------------------------------

  ;; TODO: Search for: sigil, todo...

  )


;;------------------------------------------------------------------------------
;; Keybinds : Evil
;;------------------------------------------------------------------------------

(imp:eval:after (:and evil evil-collection)

  ;;------------------------------
  ;; Signatures
  ;;------------------------------

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
             :which-key (concat "Sign: " (signature:string 'name 'sign))))

  (keybind:leader/global:def
   :infix (keybind:infix "i" "s" "i")      ;; insert -> signature -> ID
   "" '(nil :which-key "ID...") ;; Infix Title

   ;; Just the Sigil.
   "s" (list (elisp:cmd (signature:insert 'id 'sigil))
             :which-key (concat "Sigil: " (signature:string 'id 'sigil)))

   ;; Just the Name.
   "n" (list (elisp:cmd (signature:insert 'id 'name))
             :which-key (concat "Name: " (signature:string 'id 'name))))

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

     "h" (list (elisp:cmd (signature:insert 'id 'email :namespace :home))
               :which-key (concat "home: " (signature:string 'id 'email :namespace :home)))))

  ;; default namespace.
  (when (signature:exists? 'id 'email :namespace :default)
    (keybind:leader/global:def
     :infix (keybind:infix "i" "s" "e") ;; insert -> signature -> email

     "c" (list (elisp:cmd (signature:insert 'id 'email :namespace :default))
               :which-key (concat "default: " (signature:string 'id 'email :namespace :default)))))


  ;;------------------------------
  ;; Search
  ;;------------------------------

  ;; TODO: Search for: sigil, todo...

)


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'config 'user 'keybinds 'signature)

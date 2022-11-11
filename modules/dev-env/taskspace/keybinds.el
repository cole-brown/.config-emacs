;;; keybinds.el --- Keybindings -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2022-07-06
;; Modified:   2022-07-06
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Keybinds for Taskspace
;;
;;; Code:


;;---------------------------------taskspace------------------------------------
;;--                   Simple Taskspace / Task Management                     --
;;------------------------------------------------------------------------------

(require 'cl-lib) ;; for `some'
(require 'seq) ;; for `seq-contains'
(require 'dash)
(require 'org-element)

(imp:require :dlv)

(imp:require :nub)
(imp:require :taskspace 'taskspace)


;;------------------------------------------------------------------------------
;; DOOM!
;;------------------------------------------------------------------------------

(defun taskspace:keybind:doom ()
  "Create keybinds in Doom, or raise an error if not in Doom.

Create a keymap; insert into doom/evil or vanilla Emacs as
appropriate/parameters say.

Creates the taskspace keymap under the doom leader key (default SPC)"
  (if (null (symbolp 'doom!)) ;; Doom's loading function should mean this is doom?
      (nub:error
       :taskspace
       "taskspace:keybind:doom"
       "We are not in a Doom Emacs environment..? Cannot set Doom Emacs keybinds.")

    ;; Map under the Doom leader, probably.
    (map! :leader
          ;; Give it a description...
          :desc "taskspace"

          ;; ...and give it keybinds.
          (:prefix ("T" . "taskspace")

           ;; Top level commands...
           :desc "Create new..."  "T" #'taskspace:create
           :desc "Visit notes..." "v" #'taskspace:notes
           :desc "Shell..."       "s" #'taskspace:shell

           ;; 'Copy to kill ring' functions:
           (:prefix ("k" . "Kill...")

            :desc "dir"  "k" #'taskspace:dwim:dir
            :desc "name" "n" #'taskspace:dwim:name)

           ;; 'Open a dired buffer' functions:
           (:prefix ("d" . "dired")
            :desc "task dired buffer" "d" #'taskspace:dired:task
            :desc "root dired buffer" "r" #'taskspace:dired:root)))))


;;------------------------------------------------------------------------------
;; General
;;------------------------------------------------------------------------------

(defmacro taskspace:keybind:general (&rest args)
  "Create keybinds using the supplied General definer.

If using `evil', consider placing this in your config:
  ;; https://github.com/emacs-evil/evil-collection#making-spc-work-similarly-to-spacemacs
  ;; NOTE: `evil-collection' binds over SPC in many packages. To use SPC as a
  ;; leader key with `general', first set these override states:
  (setq general-override-states '(insert
                                  emacs
                                  hybrid
                                  normal
                                  visual
                                  motion
                                  operator
                                  replace))

Call this function with the desired keybind settings:
  (taskspace:keybind:general
    :prefix  \"SPC n\"
    :states  '(normal visual motion)
    :keymaps 'override)"
  (unless (featurep 'general)
    (nub:error
     :taskspace
     "taskspace:keybind:general"
     "`General' keybind feature is not loaded/defined! Cannot create keybinds."))

  `(progn
     ;;---
     ;; Top Level Commands...
     ;;---
     (general-define-key ,@args
                         :infix "t"
                         "" (list nil :which-key "Taskspace...") ;; Infix Title

                         "t" (list #'taskspace:create :which-key "New")
                         "v" (list #'taskspace:notes  :which-key "Visit")
                         "s" (list #'taskspace:shell  :which-key "Shell"))

     ;;---
     ;; 'Copy to Kill Ring' Functions:
     ;;---
     (general-define-key ,@args
                         :infix "t k"
                         "" (list nil :which-key "Copy/Kill...") ;; Infix Title

                         "k" (list #'taskspace:dwim:dir  :which-key "dir")
                         "n" (list #'taskspace:dwim:name :which-key "name"))

     ;;---
     ;; 'Open a Dired Buffer' Functions:
     ;;---
     (general-define-key ,@args
                         :infix "t d"
                         "" (list nil :which-key "Dired...") ;; Infix Title
                         "d" (list #'taskspace:dired:task :which-key "task's dired buffer")
                         "r" (list #'taskspace:dired:root :which-key "root's dired buffer"))))
;; (taskspace:keybind:general
;;     :prefix  "SPC n"
;;     :states  '(normal visual motion)
;;     :keymaps 'override)


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :taskspace 'keybinds)

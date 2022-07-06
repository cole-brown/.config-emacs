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

;; TODO: more options for making keybind:
;; (&optional orphaned description prefix)
(defun taskspace:keybind:doom ()
  "Create keybinds in Doom, or raise an error if not in Doom.

Create a keymap; insert into doom/evil or vanilla Emacs as
appropriate/parameters say.

Creates the taskspace keymap under the doom leader key (default SPC)
TODO: unless ORPHANED is not nil.

TODO: Uses DESCRIPTION if it is a string, else the description is 'taskspace'.

TODO: Uses PREFIX as the keymap prefix if not nil, else tries to use:
TODO:   - doom/evil emacs: doom-leader(?) n T
TODO:   - vanilla emacs:   TODO
TODO:
TODO: PREFIX for doom/evil should be either:
TODO:   1) a key string:
TODO:      - \"t\"
TODO:      - \"n t\"
TODO:        - \"nt\" is equivalent
TODO:   2) a cons of (key-string . description-str):
TODO:      - (\"t\" . \"taskspace\")
TODO:      - (\"n t\" . \"taskspace\")
TODO:   3) a list of 1 or 2 for setting into a sub-map:
TODO:      - (\"t\" (\"n\" (\"t\" . \"taskspace\")))"
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
;; TODO: prefix: ;; Put it under the prefix(es).
;; TODO: prefix: (cond
;; TODO: prefix:  ;;---
;; TODO: prefix:  ;; NIL
;; TODO: prefix:  ;;---
;; TODO: prefix:  ;; Return just the keybinds.
;; TODO: prefix:  ((null prefix)
;; TODO: prefix:   (int<taskspace>:keybind/doom))
;; TODO: prefix:
;; TODO: prefix:  ;;---
;; TODO: prefix:  ;; STRING
;; TODO: prefix:  ;;---
;; TODO: prefix:  ;; Put keybinds under the prefix str.
;; TODO: prefix:  ((stringp prefix)
;; TODO: prefix:   (list prefix (int<taskspace>:keybind/doom)))
;; TODO: prefix:
;; TODO: prefix:  ;;---
;; TODO: prefix:  ;; CONS
;; TODO: prefix:  ;;---
;; TODO: prefix:  ;; Put keybinds under the prefix /CONS/.
;; TODO: prefix:  ;; ONLY CONS!!!
;; TODO: prefix:  ;; No lists.
;; TODO: prefix:  ;; Just cons.
;; TODO: prefix:  ;; https://emacs.stackexchange.com/questions/10489/predicate-function-for-dotted-pairs
;; TODO: prefix:  ((and (cdr var) (atom (cdr var)))
;; TODO: prefix:   ;; Create list with description, key, and our keybids.
;; TODO: prefix:   (list
;; TODO: prefix:    :desc (nth 1 prefix)
;; TODO: prefix:    (nth 0 prefix)
;; TODO: prefix:    (int<taskspace>:keybind/doom)))
;; TODO: prefix:
;; TODO: prefix:  ;;---
;; TODO: prefix:  ;; LIST
;; TODO: prefix:  ;;---
;; TODO: prefix:  ((listp prefix)
;; TODO: prefix:   ;; Build first layer and do a recursion...
;; TODO: prefix:   (-let [(this-prefix . rest) prefix]
;; TODO: prefix:     (
;; TODO: prefix:      )))
;; TODO: prefix:
;; TODO: prefix:   (list
;; TODO: prefix:    :desc (nth 1 prefix)
;; TODO: prefix:    (nth 0 prefix)
;; TODO: prefix:
;; TODO: prefix:    prefix (int<taskspace>:keybind/doom)))
;; TODO: prefix:
;; TODO: prefix:
;; TODO: prefix: (defun int<taskspace>:no-error-predicate (func value)
;; TODO: prefix:   "Checks if VALUE fulfills FUNC function predicate.
;; TODO: prefix:
;; TODO: prefix: Wraps check in a `condition-case-unless-debug', returns nil if error signal caught."
;; TODO: prefix:   (condition-case-unless-debug nil
;; TODO: prefix:       (funcall func value)
;; TODO: prefix:     (error nil)))
;; TODO: prefix:
;; TODO: prefix:
;; TODO: prefix: (defun int<taskspace>:keybind/doom.level (key description rest)
;; TODO: prefix:   "Creates a level of a doom `map!'.
;; TODO: prefix:
;; TODO: prefix: KEY must be a string for the key(s) to use for this level of the keybind tree.
;; TODO: prefix:
;; TODO: prefix: DESCRIPTION must be a (short) description string or nil.
;; TODO: prefix:
;; TODO: prefix: REST must be a function to call, or a deeper (child) level of the keybind tree."
;; TODO: prefix:   ;; Error check our inputs...
;; TODO: prefix:   (cond ((not (int<taskspace>:no-error-predicate #'stringp key))
;; TODO: prefix:          (error "%s: Key must be a string: %S"
;; TODO: prefix:                 "int<taskspace>:keybind/doom.level" key))
;; TODO: prefix:
;; TODO: prefix:         ((and (not (null description))
;; TODO: prefix:               (not (int<taskspace>:no-error-predicate #'stringp description)))
;; TODO: prefix:          (error "%s: Description must be nil or a string: %S"
;; TODO: prefix:                 "int<taskspace>:keybind/doom.level" description))
;; TODO: prefix:
;; TODO: prefix:         ((null rest)
;; TODO: prefix:          (error "%s: Must have something to bind: %S"
;; TODO: prefix:                 "int<taskspace>:keybind/doom.level" rest))
;; TODO: prefix:
;; TODO: prefix:         ;;---
;; TODO: prefix:         ;; Success - make the level.
;; TODO: prefix:         ;;---
;; TODO: prefix:         (t
;; TODO: prefix:          (if (null description)
;; TODO: prefix:              (list key rest)
;; TODO: prefix:            (list :desc description
;; TODO: prefix:                  key
;; TODO: prefix:                  rest)))))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :taskspace 'keybinds)

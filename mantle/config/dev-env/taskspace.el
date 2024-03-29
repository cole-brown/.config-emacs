;;; mantle/config/dev-env/taskspace.el --- Config Taskspace -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2022-11-08
;; Timestamp:  2023-07-21
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;  Config Taskspace
;;
;;; Code:


;; TODO: `taskspace' isn't a real package yet so require that it's already loaded.
(imp:require :taskspace)
(imp:require :datetime 'format)


;;------------------------------------------------------------------------------
;; Taskspace
;;------------------------------------------------------------------------------

(imp:use-package taskspace
  ;; This is my own package, so...
  ;;   1. Don't try to install.
  :ensure nil
  ;; TODO: Probably need `taskspace' to be a real package?
  ;;   2. Here's where it is; add this dir to the `load-path'.
  ;; :load-path innit:path:package:mis

  ;;------------------------------
  :init
  ;;------------------------------

  ;;---
  ;; General (Non-Per-Domain) Init...
  ;;---
  (defun mantle:user:taskspace:generate (group taskname taskpath filepath)
    "NOTE: Could be redefined later for more work-specific details, so check
e.g. 'finalize-domain-secret.el' for a redef. Or 'C-h f my/taskspace/generate'
and see what file it's defined in."
    ;; Break these formats up a bit for easier maintenance?
    (mapconcat
     #'identity
     (list
      ;;---
      ;; Header (as of [2023-07-11], just a copy of `yasnippet' "/header")
      ;;---
      (format (mapconcat #'identity
                         '("#+TITLE:     %s"
                           "#+AUTHOR:    %s"
                           "#+EMAIL:     %s"
                           "#+DATE:      %s"
                           "#+TIMESTAMP: 0000-00-00") ; Auto-filled by Emacs' `time-stamp' feature.
                         "\n")
              ;; TITLE:
              (let ((filename/no-ext (file-name-nondirectory (file-name-sans-extension filepath))))
                ;; Is filename "<something>.notes.*"? We already stripped the (actual file
                ;; extension) ".*" so now we can check if the "extension" is ".notes".
                (if (string= (file-name-extension filename/no-ext) "notes")
                    ;; Reverse "something.notes" into "Notes.Something".
                    (concat
                     (capitalize (file-name-extension filename/no-ext))
                     "."
                     (mapconcat #'capitalize
                                (split-string (file-name-sans-extension filename/no-ext) "_")
                                "_"))
                  (capitalize filename/no-ext)))
              ;; AUTHOR:
              (signature:string 'id 'name :default (user-full-name))
              ;; EMAIL:
              (signature:get 'id 'email :namespace (jerky:namespace:get) :default (message-user-mail-address))
              ;; DATE:
              (datetime:format 'rfc-3339 'date))

      ;;---
      ;; Blank
      ;;---
      ""

      ;;---
      ;; Task Name & Path
      ;;---
      (format (mapconcat #'identity
                         '("#+TASKSPACE: %s" ; taskpath
                           "#+TASKSPACE: %s" ; translated taskpath
                           "%s"              ; taskname
                           ""
                           "%s")             ; mkdir cmd for remote servers
                         "\n")
              taskpath
              (path:translate :auto :auto taskpath)
              taskname
              (format "mkdir %s" taskname))

      ;;---
      ;; Blank
      ;;---
      ""

      ;;---
      ;; Fancy Header / Notes Separator
      ;;---
      "     ┌┬┬┬──────────────────────────────────────────────────────────────┬┬┬┐"
      "     ├┼┼┤                             ...                              ├┼┼┤"
      "     └┴┴┴──────────────────────────────────────────────────────────────┴┴┴┘"

      ;;---
      ;; Blank
      ;;---
      ""
      ""
      "* Description"
      ""
      ""
      )
     "\n"))
  ;; (mantle:user:taskspace:generate :test "test_task" "~/path/to/taskspace/test_task.notes.ext" "~/notes/test_task.notes.ext")

  ;;---
  ;; "Home" Domain
  ;;---

  (let* ((group     :home) ; taskspace "group" == jerky "namespace"
         (group-str (str:normalize:symbol group))
         (group-fn  (intern (concat "mantle:user:taskspace:generate/" group-str))))
    (when (and (system:secret:has)
               ;; Jerky should have had the paths set during secrets init.
               (jerky:has 'path 'taskspace 'notes :namespace group)
               (jerky:has 'path 'taskspace 'root :namespace group))
      ;; Could create a totally different one, but the generic one from above is just fine.
      (defalias group-fn 'mantle:user:taskspace:generate)

      ;; And create our group custom settings.
      (push (list group
                  (format "%S Taskspace" (str:case/string:to:title group-str))
                  `((:type/notes        :noteless)
                    (:format/datetime  ,(datetime:format/get 'rfc-3339 'date))
                    (:dir/tasks        ,(jerky:get 'path 'taskspace 'root :namespace group))
                    (:dir/notes        ,(jerky:get 'path 'taskspace 'notes :namespace group))
                    (:file/new/generate
                     ;; Empty projectile file.
                     ((".projectile" "")
                      ;; notes.org: setup with org header snippet ready to go
                      ((int<taskspace>:config ,group :file/notes)
                       ,group-fn)))))
            taskspace:groups)

      ;; DLVs should have been set during secrets init via `system:multiplexer:dlv:add'.
      ))


  ;;---
  ;; "Work" Domain
  ;;---
  (let* ((group     :work) ; taskspace "group" == jerky "namespace"
         (group-str (str:normalize:symbol group))
         (group-fn  (intern (concat "mantle:user:taskspace:generate/" group-str))))
    (when (and (system:secret:has)
               ;; Jerky should have had the paths set during secrets init.
               (jerky:has 'path 'taskspace 'notes :namespace group)
               (jerky:has 'path 'taskspace 'root :namespace group))
      ;; Could create a totally different one, but the generic one from above is just fine.
      (defalias group-fn 'mantle:user:taskspace:generate)

      ;; And create our group custom settings.
      (push (list group
                  (format "%S Taskspace" (str:case/string:to:title group-str))
                  `((:type/notes        :noteless)
                    (:format/datetime  ,(datetime:format/get 'rfc-3339 'date))
                    (:dir/tasks        ,(jerky:get 'path 'taskspace 'root :namespace group))
                    (:dir/notes        ,(jerky:get 'path 'taskspace 'notes :namespace group))
                    (:file/new/generate
                     ;; Empty projectile file.
                     ((".projectile" "")
                      ;; notes.org: setup with org header snippet ready to go
                      ((int<taskspace>:config ,group :file/notes)
                       ,group-fn)))))
            taskspace:groups)

      ;; DLVs should have been set during secrets init via `system:multiplexer:dlv:add'.
      ))

  ;; ;;------------------------------
  ;; :custom
  ;; ;;------------------------------
  ;;
  ;; This doesn't work:
  ;;   (taskspace:groups mantle:user:taskspace:groups)
  ;;
  ;; 1. Building settings into a variable in `:init' doesn't work as it runs
  ;;    after `:custom'?
  ;; 2. Building settings into a variable in `:preface' doesn't work as I end up
  ;;    with doubled entries in `taskspace:groups'?!
  ;;
  ;; I am :confused:, but ok whatever; pushing directly into `taskspace:groups'
  ;; during `:init' works.
  )


;;------------------------------
;; Keybinds : Meow
;;------------------------------

(imp:use-package taskspace
  ;; This is my own package, so...
  ;;   1) Don't try to install.
  :ensure nil
  ;; TODO: Probably need `taskspace' to be a real package?
  ;;   2) Here's where it is; add this dir to the `load-path'.
  ;; :load-path innit:path:package:mis

  ;; Defer loading; we'll load in a hook if we want this.
  :defer t

  ;; Only load if meowing...
  :when  (imp:flag? :keybinds +meow)
  :after meow

  :config
  ;;------------------------------
  ;; Actually Create Keybinds:
  ;;------------------------------
  (if (imp:provided? :keybinds 'general 'meow)
      ;;---
      ;; Use `general':
      ;;---
      ;; Will put Taskspace stuff under infix "t".
      (taskspace:keybind:general :prefix  (keybind:prefix :global "n")
                                 :keymaps keybinds:meow:keymaps/leader)

    ;;---
    ;; Use `transient':
    ;;---
    ;; Define the transient - will be `taskspace:keybind:transient'.
    (taskspace:keybind:transient/def)
    ;; Put into the "notes" menu/transient instead of directly in meow leader.
    (transient-append-suffix 'mantle:meow/transient:notes
      '(0 -1) ; Append after last group/suffix in the first group.
      ["Taskspace"
       ("t" "Taskspace..." taskspace:keybind:transient)])
    ;; (meow-leader-define-key '("t" . taskspace:keybind:transient))
    ))


;;------------------------------
;; Keybinds : Evil
;;------------------------------

(imp:use-package taskspace
  ;; This is my own package, so...
  ;;   1) Don't try to install.
  :ensure nil
  ;; TODO: Probably need `taskspace' to be a real package?
  ;;   2) Here's where it is; add this dir to the `load-path'.
  ;; :load-path innit:path:package:mis

  ;; Defer loading; we'll load in a hook if we want this.
  :defer t

  ;; Only load if very evil...
  :when  (imp:flag? :keybinds +evil)
  :after (:and python evil evil-collection)

  ;;------------------------------
  :config
  ;;------------------------------

  (taskspace:keybind:general :prefix  (keybind:prefix :global "n")
                             :states  keybind:leader/global:states
                             :keymaps keybind:leader/global:keymaps))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'config 'dev-env 'taskspace)

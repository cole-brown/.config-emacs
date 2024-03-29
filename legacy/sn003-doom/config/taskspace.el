;;; config/spy:taskspace.el -*- lexical-binding: t; -*-


;;------------------------------------------------------------------------------
;; Jerky Directory Local Variables
;;------------------------------------------------------------------------------

(require 'taskspace)
(imp:require :path)


;;------------------------------------------------------------------------------
;; Taskspace Helper Functions
;;------------------------------------------------------------------------------

(defun sss:taskspace/generate (group taskname taskpath)
  "NOTE: Could be redefined later for more group-specific
details, so check e.g. secrets init for a redef. Or 'C-h f
sss:taskspace/generate' and see what file it's defined in."
  ;; Format:
  ;; header snippet key
  ;;
  ;; taskname
  ;; taskpath
  ;;
  ;; 'mkdir cmd'
  ;;
  ;; fancy box to separate this stuff from start of normal notes
  (format (concat "%s\n" ;; header
                  "\n"
                  "#+TASKSPACE: %s\n" ;; taskpath
                  "             %s\n" ;; translated path
                  "%s\n" ;; taskname
                  "\n"
                  "%s\n" ;; mkdir
                  "\n"
                  "%s\n" ;; fancy box top
                  "%s\n" ;; fancy box middle
                  "%s\n" ;; fancy box bottom
                  "\n\n")
          "header" ;; yasnippet
          taskpath
          (path:translate :windows :wsl taskpath)
          taskname
          (format "mkdir ~/00-cole-temp/%s" taskname)
          "     ┌┬┬┬──────────────────────────────────────────────────────────────┬┬┬┐"
          "     ├┼┼┤                             ...                              ├┼┼┤"
          "     └┴┴┴──────────────────────────────────────────────────────────────┴┴┴┘"
          ))
;; (spydez/taskspace/generate "" "")


;;------------------------------------------------------------------------------
;; Taskspace Config
;;------------------------------------------------------------------------------

(after! taskspace
  ;; Ignore unless we have a bare minimum to even try configuring.
  (let ((available-taskspace-groups '((:default "Defaults" taskspace/group/default))))

    ;;------------------------------
    ;; Configuration
    ;;------------------------------

    ;;---
    ;; General (Non-Per-Domain) Config...
    ;;---

    (taskspace/keybind/doom)

    ;;---
    ;; "Home" Domain
    ;;---
    (when (jerky/get 'path 'lily :namespace :home)
      (let* ((group :home) ; taskspace "group" == jerky "namespace"
             (group-str (str:normalize:symbol group))
             group-fn)

        ;; Alias our function here in case we want to redefine it later.
        ;; We'll just have to redefine it instead of edit the taskspace settings again.
        (defalias 'sss:taskspace/generate.home 'sss:taskspace/generate)

        (spy:secret/if "config/taskspace.el"
            '(:skip "Skipping some `taskspace' :home configuration..."
              :eval "")
          ;; Add taskspace paths to jerky for info.
          (jerky/set 'path 'taskspace 'notes
                     :namespace group
                     :value (path:abs:dir
                             (jerky/get 'path 'lily :namespace group)
                             "taskspace"
                             (str:normalize:symbol group))
                     :docstr (format "directory for %s taskspace notes" group-str))
          (jerky/set 'path 'taskspace 'root
                     :namespace group
                     :value (jerky/get 'path 'taskspace :namespace group)
                     :docstr (format "directory for %s taskspace data/files" group-str))

          ;; And create our `:home' group custom settings.
          (defvar sss:taskspace/custom.home
            `((:type/notes      :noteless)
              (:format/datetime (spy:datetime/format.get 'iso-8601 'short))
              (:dir/tasks (jerky/get 'path 'taskspace 'root :namespace ,group))
              (:dir/notes (jerky/get 'path 'taskspace 'notes :namespace ,group))
              (:file/new/generate ((".projectile" "") ;; projectile: empty file
                                   ;; notes.org: setup with org header snippet
                                   ;; ready to go
                                   ((-t//config :group :file/notes)
                                    sss:taskspace/generate.home))))
            "Custom settings for my `:home' taskspace group.")

          ;; Set an over-arching taskspace DLV for whole work dir.
          (taskspace/group/dlv group
                               (jerky/get 'path 'taskspace 'root :namespace group))
          ;; Set a dir-local-var for home taskspace folders.
          (taskspace/group/dlv group
                               (jerky/get 'path 'taskspace 'notes :namespace group))
          (taskspace/group/dlv group
                               (jerky/get 'path 'org 'journal :namespace group)))

        (push (list group group-str group-fn) available-taskspace-groups)))
    ;; /"Home" Domain


    ;;---
    ;; "Work" Domain
    ;;---
    (when (jerky/get 'path 'lily :namespace :work)
      (let* ((group :work) ; taskspace "group" == jerky "namespace"
             (group-str (str:normalize:symbol group))
             group-fn)

        ;; Alias our function here in case we want to redefine it later.
        ;; We'll just have to redefine it instead of edit the taskspace settings again.
        (defalias 'sss:taskspace/generate.work 'sss:taskspace/generate)

        (spy:secret/if "config/taskspace.el"
            '(:skip "Skipping some `taskspace' :work configuration..."
              :eval "")
          ;; Add taskspace paths to jerky for info.
          (jerky/set 'path 'taskspace 'notes
                     :namespace group
                     :value (path:abs:dir
                             (jerky/get 'path 'lily :namespace group)
                             "taskspace"
                             (str:normalize:symbol group))
                     :docstr (format "directory for %s taskspace notes" group-str))
          (jerky/set 'path 'taskspace 'root
                     :namespace group
                     :value (jerky/get 'path 'taskspace :namespace group)
                     :docstr (format "directory for %s taskspace data/files" group-str))

          ;; And create our `:work' group custom settings.
          (defvar sss:taskspace/custom.work
            `((:type/notes      :noteless)
              (:format/datetime (spy:datetime/format.get 'iso-8601 'short))
              (:dir/tasks (jerky/get 'path 'taskspace 'root :namespace ,group))
              (:dir/notes (jerky/get 'path 'taskspace 'notes :namespace ,group))
              (:file/new/generate ((".projectile" "") ;; projectile: empty file
                                   ;; notes.org: setup with org header snippet
                                   ;; ready to go
                                   ((-t//config :group :file/notes)
                                    sss:taskspace/generate.work))))
            "Custom settings for my `:work' taskspace group.")
          (setq group-fn #'sss:taskspace/custom.work)

          ;; Set a dir-local-var for home taskspace folders.
          (taskspace/group/dlv group
                               (jerky/get 'path 'taskspace 'root :namespace group))
          (taskspace/group/dlv group
                               (jerky/get 'path 'taskspace 'notes :namespace group))
          (taskspace/group/dlv group
                               (jerky/get 'path 'org 'journal :namespace group)))

      (push (list group group-str group-fn) available-taskspace-groups)))


      ;;------------------------------
      ;; Groups
      ;;------------------------------

      ;; Add default to the available groups.
      (customize-set-variable 'taskspace/groups available-taskspace-groups)))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :dot-emacs 'config 'taskspace)

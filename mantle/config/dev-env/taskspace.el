;;; mantle/config/dev-env/taskspace.el --- Config Taskspace -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2022-11-08
;; Modified:   2022-11-08
;; URL:        https://github.com/cole-brown/.config-emacs
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
  ;;   1) Don't try to install.
  :ensure nil
  ;; TODO: Probably need `taskspace' to be a real package?
  ;;   2) Here's where it is; add this dir to the `load-path'.
  ;; :load-path innit:path:package:mis

  ;;------------------------------
  :init
  ;;------------------------------

  ;;---
  ;; General (Non-Per-Domain) Init...
  ;;---
  (defun mantle:user:taskspace:generate (taskname taskpath)
    "NOTE: Could be redefined later for more work-specific details, so check
e.g. 'finalize-domain-secret.el' for a redef. Or 'C-h f my/taskspace/generate'
and see what file it's defined in."
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
                    "#+TASKSPACE: %s\n" ;; translated taskpath
                    "%s\n" ;; taskname
                    "\n"
                    "%s\n" ;; mkdir cmd for remote servers
                    "\n"
                    "%s\n" ;; fancy box top
                    "%s\n" ;; fancy box middle
                    "%s\n" ;; fancy box bottom
                    "\n\n")
            "/header" ;; yasnippet
            taskpath
            (path:translate :auto :auto taskpath)
            taskname
            (format "mkdir %s" taskname)
            "     ┌┬┬┬──────────────────────────────────────────────────────────────┬┬┬┐"
            "     ├┼┼┤                             ...                              ├┼┼┤"
            "     └┴┴┴──────────────────────────────────────────────────────────────┴┴┴┘"))

  (defvar mantle:user:taskspace:groups
   '((:default "Defaults"       taskspace:group:default))
    "Accumulator for known taskspace groups.")

  ;;---
  ;; "Home" Domain
  ;;---

  (let* ((group     :home) ; taskspace "group" == jerky "namespace"
         (group-str (str:normalize:symbol->string group))
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
            mantle:user:taskspace:groups)

      ;; DLVs should have been set during secrets init via `system:multiplexer:dlv:add'.
      ))


  ;;---
  ;; "Work" Domain
  ;;---
  (let* ((group     :work) ; taskspace "group" == jerky "namespace"
         (group-str (str:normalize:symbol->string group))
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
            mantle:user:taskspace:groups)

      ;; DLVs should have been set during secrets init via `system:multiplexer:dlv:add'.
      ))

  ;;------------------------------
  :custom
  ;;------------------------------

  (taskspace:groups mantle:user:taskspace:groups)


  ;;------------------------------
  :config
  ;;------------------------------

  (taskspace:keybind:general #'keybind:leader/global:def))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'config 'user 'dev-env 'taskspace)

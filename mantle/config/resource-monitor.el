;;; mantle/config/resource-monitor.el --- Monitor Emacs' Resource Usage -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2023-06-15
;; Timestamp:  2023-06-21
;; URL:        https://github.com/cole-brown/.config-emacs
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Monitor Emacs' Resource Usage
;;
;;; Code:


;;--------------------------------------------------------------------------------
;; Keybinds : Meow
;;--------------------------------------------------------------------------------
;; Keybinds for resource monitoring of all sorts: `memory-usage', default Emacs
;; functions, etc.

(imp:use-package emacs
  :when  (imp:flag? :keybinds +meow)
  :after meow


  ;;------------------------------
  :config
  ;;------------------------------

  (keybind:leader/global:def
    :infix (keybind:infix "<f1>")   ; F1 by itself is (by default) the help menu
    "" '(nil :which-key "Resource Monitor...")) ; infix title

  (keybind:leader/global:def
    :infix (keybind:infix "<f1><f1>")
    "" '(nil :which-key "Emacs...") ; infix title
    "<f1>" (list #'memory-usage :which-key "Memory Usage")
    "<f2>" (list #'list-processes :which-key "Processes")

    ;; "p" : used by `profiler'
    )

  (keybind:leader/global:def
    :infix (keybind:infix "<f1><f2>")
    "" '(nil :which-key "Host OS...") ; infix title
    "p" (list #'proced :which-key "Processes")))


;;------------------------------------------------------------------------------
;; Memory Usage
;;------------------------------------------------------------------------------

(imp:use-package memory-usage)


;;--------------------------------------------------------------------------------
;; Proced (Top/Glances/Htop Sorta Thing)
;;--------------------------------------------------------------------------------

(imp:use-package proced
  :ensure nil ; This is an Emacs built-in feature.

  ;;------------------------------
  :custom
  ;;------------------------------

  ;; Default to auto-updating the `proced' buffer.every
  ;; `proced-auto-update-interval' seconds. Can toggle w/ `proced-toggle-auto-update'.
  (proced-auto-update-flag t)


  ;; TODO:proced: Rebind movement keys, or does `meow' override?
  ;; TODO:proced: Could rebind a lot more... See `proced-mode-map'.
  ;; ;;------------------------------
  ;; :bind
  ;; ;;------------------------------
  ;; (:map proced-mode-map
  ;;  ("RET" . newline))


  ;;------------------------------
  :config
  ;;------------------------------

  ;;------------------------------
  ;; Local (`proced-mode') Keybinds
  ;;------------------------------
  (keybind:meow:leader/local:bind-keys
      'proced-mode-map
    "u" (list #'proced-toggle-auto-update :which-key "Toggle: Auto-Update")
    "t" (list #'proced-toggle-auto-update :which-key "Toggle: Process Trees")
    "s" (list #'proced-sort-interactive   :which-key "Sort...")
    "r" (list #'proced-refine             :which-key "Refine...")
    "f" (list #'proced-filter-interactive :which-key "Filter...")
    "d" (list #'proced-format-interactive :which-key "Format...")))


;;--------------------------------------------------------------------------------
;; Profiler
;;--------------------------------------------------------------------------------

;; TODO:profiler: There is also the `elp' library for profiling spcific Elisp
;; functions. Add keybinds for doing that if needed?

(imp:use-package profiler
  :ensure nil ; This is an Emacs built-in feature.
  :after  pretty-hydra

  ;; TODO:hydra: `:pretty-hydra' didn't get recognized, but 'pretty-hydra' says
  ;; it's a 'use-package' keyword?
  ;; ;;------------------------------
  ;; :pretty-hydra
  ;; ;;------------------------------
  ;;
  ;; ;; (pretty-hydra-define int<profiler>:hydra ; for debugging
  ;; (int<profiler>:hydra
  ;;  (:quit-key "g"
  ;;   :color blue ;; Default to `:exit t' for all heads.
  ;;   :title "Profiler")
  ;;
  ;;  ("Toggle"
  ;;   (("c" (profiler-start 'cpu)     "Start: CPU"     :toggle (profiler-cpu-running-p))
  ;;    ("m" (profiler-start 'mem)     "Start: MEM"     :toggle (profiler-mem-running-p))
  ;;    ("s" (profiler-start 'cpu+mem) "Start: CPU+MEM" :toggle (profiler-running-p))
  ;;    ("t" profiler-stop "Stop")
  ;;    ("x" profiler-reset "Reset"))
  ;;
  ;;   "Report"
  ;;   (("r" profiler-report "Report"))))
  ;; ;; (int<profiler>:hydra/body)
  ;; ;; (profiler-running-p)


  ;;------------------------------
  :config
  ;;------------------------------

  (pretty-hydra-define int<profiler>:hydra ; for debugging
   (:quit-key "g"
    :color blue ;; Default to `:exit t' for all heads.
    :title "Profiler")

   ("Toggle"
    (("c" (profiler-start 'cpu)     "Start: CPU"     :toggle (profiler-cpu-running-p))
     ("m" (profiler-start 'mem)     "Start: MEM"     :toggle (profiler-mem-running-p))
     ("s" (profiler-start 'cpu+mem) "Start: CPU+MEM" :toggle (profiler-running-p))
     ("t" profiler-stop "Stop")
     ("x" profiler-reset "Reset"))

    "Report"
    (("r" profiler-report "Report"))))
  ;; (int<profiler>:hydra/body)
  ;; (profiler-running-p)

  (keybind:leader/global:def
    :infix (keybind:infix "<f1>")
    "p" (list #'int<profiler>:hydra/body :which-key "Profiler...")))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'config 'user 'resource-monitor)

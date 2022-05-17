;;; jerky/+dlv.el -*- lexical-binding: t; -*-

;; Jerky Directory Local Variable support for auto-namespaces.

(imp:require :dlv)
(imp:require :nub)


;;------------------------------------------------------------------------------
;; Variables
;;------------------------------------------------------------------------------

;;------------------------------
;; Auto-Namespacing
;;------------------------------

(defvar int<jerky>:dlv:namespace/local nil
  "Directory Local Variable for holding the directory's namespace.")

;; Mark our DLV variable as safe for DLV use.
(dlv:var:safe/predicate 'int<jerky>:dlv:namespace/local #'int<jerky>:namespace:valid)
;; (get 'int<jerky>:dlv:namespace/local 'safe-local-variable)


;;------------------------------------------------------------------------------
;; Functions
;;------------------------------------------------------------------------------

(defun jerky:dlv:namespace/set (directory namespace)
  "Set Jerky's local NAMESPACE for the DIRECTORY."
  (nub:debug:func/start :jerky
                        "jerky:dlv:namespace/set"
                        '(:dlv)
                        (list (cons 'directory . directory)
                              (cons 'namespace . namespace)))

  (if (not (jerky:namespace:has namespace))
      (error "jerky:dlv:namespace/set: No known namespace called '%s'" namespace)

    (int<jerky>:debug "int<jerky>:dlv:namespace/set" "  -> `int<jerky>:dlv:namespace/local'")
    (dlv:set directory
             nil ;; global mode
             (list 'int<jerky>:dlv:namespace/local
                   namespace
                   :safe)))

  (nub:debug:func/end :jerky
                      "jerky:dlv:namespace/set"
                      '(:dlv)))
;; (jerky:get 'path 'org 'journal :namespace :work)
;;   -> "d:/home/main/.org.d/logbook/work/"
;; (jerky:dlv:namespace/set "d:/home/main/.org.d/logbook/work/" :work)


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide:with-emacs :jerky '+dlv)

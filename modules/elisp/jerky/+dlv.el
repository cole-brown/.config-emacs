;;; jerky/+dlv.el -*- lexical-binding: t; -*-

;; Jerky Directory Local Variable support for auto-namespaces.

(imp:require :dlv)

(imp:require :nub)
(imp:require :jerky 'debug)


;;------------------------------------------------------------------------------
;; Variables
;;------------------------------------------------------------------------------

;;------------------------------
;; Auto-Namespacing
;;------------------------------

(defvar int<jerky>:dlv:namespace/local nil
  "Directory Local Variable for holding the directory's namespace.")

;; Mark our DLV variable as safe for DLV use.
(dlv:var:safe/predicate 'int<jerky>:dlv:namespace/local
                        #'int<jerky>:namespace:valid?)
;; (get 'int<jerky>:dlv:namespace/local 'safe-local-variable)


;;------------------------------------------------------------------------------
;; Functions
;;------------------------------------------------------------------------------

(defun jerky:dlv:namespace/set (directory namespace)
  "Set Jerky's local NAMESPACE for the DIRECTORY."
  (let ((func/name "jerky:dlv:namespace/set")
        (func/tags '(:dlv)))
    (nub:debug:func/start :jerky
                          func/name
                          func/tags
                          (list (cons 'directory directory)
                                (cons 'namespace namespace)))

    (if (not (jerky:namespace:has namespace))
        (error "%s: No known namespace called '%s'"
               func/name
               namespace)

      (dlv:set directory
               nil ;; global mode
               (list 'int<jerky>:dlv:namespace/local
                     namespace
                     :safe)))

    (nub:debug:func/end :jerky
                        func/name
                        func/tags)))
;; (jerky:get 'path 'org 'journal :namespace :work)
;;   -> "d:/home/main/.org.d/logbook/work/"
;; (jerky:dlv:namespace/set "d:/home/main/.org.d/logbook/work/" :work)


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :jerky '+dlv)

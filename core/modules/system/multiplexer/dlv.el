;;; system/multiplexer/dlv.el -*- lexical-binding: t; -*-


(imp:require :dlv)


;;------------------------------------------------------------------------------
;; System DLV Hook
;;------------------------------------------------------------------------------

(defvar system:multiplexer:dlv/functions nil
  "Hook variable for 'abnormal hooks' dealing with Directory Local Variables.

Hook functions will set Dir Local Variables for DOMAIN-KEYWORD in DIR-PATH.

Hook functions should be registered with `add-hook'.
  (add-hook 'system:multiplexer:dlv/functions #'func-name)

Function signature must be:
  (defun func-name (DIR-PATH DOMAIN-KEYWORD)
    ...)

  DIR-PATH will be an absolute path string.
    - example: \"/path/to/directory/\"
  DOMAIN-KEYWORD will be a keyword of the system's domain.
    - examples: `:work', `:home'")


(defun system:multiplexer:dlv:domain (dir-path domain-keyword)
  "Run multiplexer DLV hooks to set DIR-PATH to a DOMAIN-KEYWORD.

Hook functions should be registered to `system:multiplexer:dlv/functions' with
`add-hook'.

DIR-PATH will be an absolute path string.
  - example: \"/path/to/directory/\"

DOMAIN-KEYWORD will be a keyword of the system's domain.
  - examples: `:work', `:home'

NOTE: This must be run:
  1) By user, and/or
  2) after user's config
so that user has a chance to add functions to the hook variable."
  (run-hook-with-args system:multiplexer:dlv/functions
                      dir-path
                      domain-keyword))


;;------------------------------------------------------------------------------
;; Registered System DLV Paths
;;------------------------------------------------------------------------------

(defcustom system:multiplexer:dlv/args nil
  "Hook arguments to use for `system:multiplexer:dlv:domain/all'.

This should be a list of list of args to `system:multiplexer:dlv:domain'."
  :group 'system:multiplexer:group
  :type  '(repeat (list string (restricted-sexp :match-alternatives (keywordp)))))


(defun system:multiplexer:dlv:domain/all ()
  "Set DLV system domain for all registered functions & args lists.

Run `system:multiplexer:dlv/functions' for each list of args in
`system:multiplexer:dlv/args'.

NOTE: This must be run after user's config so that user has a chance to add
functions to the hook variable & args to the arg variable."
  (dolist (args system:multiplexer:dlv/args)
    (apply system:multiplexer:dlv/args args)))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :system 'multiplexer 'dlv)
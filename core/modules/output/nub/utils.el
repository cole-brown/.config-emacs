;;; core/modules/output/nub/utils.el --- Utility Nubs -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2021-10-22
;; Timestamp:  2023-08-15
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Utility Functions for Nub
;;
;;; Code:

(imp:require :nub 'internal)
(imp:require :nub 'alist)


;;------------------------------------------------------------------------------
;; Utility Functions & Such That Don't Really Fit Elsewhere
;;------------------------------------------------------------------------------

(defun nub:format:callers (caller &optional parents)
  "Build a 'caller' string.

Builds from CALLER (string) and PARENTS (string or nil).

Returns a string."
  ;;------------------------------
  ;; Error Checks:
  ;;------------------------------
  ;; CALLER must be a string.
  (cond ((not (stringp caller))
         (int<nub>:error "int<nub>:format:callers"
                         "Invalid CALLER param. Expected a string; got: caller: %S, parents: %S"
                         caller parents))
        ;; PARENTS must be a string if not nil.
        ((and parents
              (not (stringp parents)))
         (int<nub>:error "int<nub>:format:callers"
                         "Invalid CALLER param. Expected a string; got: parents: %S, caller: %S"
                         parents caller))

        ;;------------------------------
        ;; Valid: Concat caller w/ parents.
        ;;------------------------------
        (t
         (if parents
             (concat caller " <-via- " parents)
           caller))))
;; (nub:format:callers "bob" nil)
;; (nub:format:callers "bob" "alice")
;; (nub:format:callers "C" (nub:format:callers "B" "A"))
;; (nub:format:callers nil nil)


;;------------------------------------------------------------------------------
;; Normalization
;;------------------------------------------------------------------------------

(defun int<nub>:normalize->string (input)
  "Normalize INPUT to a layout string.

If INPUT is:
  - String:  Remove \"+layout/\" prefix,
             (then remove \":\" prefix if exists), and return.
  - Keyword: Get symbol name, remove \":\" prefix, and return.
  - Symbol:  Get symbol name, remove \"+layout/\" prefix,
             (then remove \":\" prefix if exists), and return.
E.g.
  1) \"dvorak\" -> \"dvorak\"
  2) `:dvorak' -> \"dvorak\"
  3) `+layout/dvorak' -> \"dvorak\""
  (replace-regexp-in-string
   ;; Must match start of string.
   (rx string-start
       ;; ':' from keywords (`:dvorak').
       ;; '+' from module flag (`+layout/dvorak').
       (zero-or-one (any ":" "+"))
       ;; 'layout/' from module flag (`+layout/dvorak') or directory path ('layout/+dvorak').
       (zero-or-one "layout/")
       ;; '+' from directory path ('layout/+dvorak').
       (zero-or-one "+"))
   ;; Replace with nothing.
   ""
   ;; Make sure we've got a string (or we've errored out on invalid input type.
   (if (stringp input)
       input
     (symbol-name input))))
;; (int<nub>:normalize->string '+layout/spydez)
;; (int<nub>:normalize->string :spydez)
;; (int<nub>:normalize->string "spydez")
;; (int<nub>:normalize->string "+spydez")


(defun int<nub>:normalize->keyword (input)
  "Convert INPUT to a keyboard layout keyword.

If INPUT is `nil', `nil' will be returned (allows for default args).

Otherwise INPUT is normalized to a string and then converted to a keyword.
  - Uses `int<nub>:normalize->string'.

E.g. `+layout/dvorak' -> `:dvorak'."
  (if (null input)
      nil
    (intern (concat ":"
                    (int<nub>:normalize->string input)))))
;; (int<nub>:normalize->keyword '+layout/spydez)
;; (int<nub>:normalize->keyword :spydez)
;; (int<nub>:normalize->keyword "spydez")
;; (int<nub>:normalize->keyword nil)


;;------------------------------------------------------------------------------
;; The End
;;------------------------------------------------------------------------------
(imp:provide :nub 'utils)

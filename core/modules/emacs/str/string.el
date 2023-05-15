;;; emacs/str/string.el --- Useful String Functions -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <code@brown.dev>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2023-04-17
;; Modified:   2023-04-17
;;
;;; Commentary:
;;
;; String Functions for Strings
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Utils
;;------------------------------------------------------------------------------

(defun str:contains? (needle haystack)
  "Return t/nil for whether HAYSTACK string contains NEEDLE string."
  (not (null (string-match-p (regexp-quote needle) haystack))))


(defun str:trim (str &rest args)
  "Remove whitespace from STR.

ARGS can contain:
  - `:left', 'left'
  - `:right', `right'
  - `:full', `full', `:both', `both'
    - default"
  (declare (pure t) (side-effect-free t))
  (let (trim/left
        trim/right)

    (dolist (arg args)
      (pcase arg
        ((or :left 'left)
         (setq trim/left t))
        ((or :right 'right)
         (setq trim/right t))
        ((or :full 'full :both 'both)
         (setq trim/right t
               trim/left  t))))

    (cond ((and trim/left
		(not trim/right))
	   (string-trim-left str))
	  ((and trim/right
		(not trim/left))
	   (string-trim-right str))
	  (t ; Either `:full', `:both', `:left' & `:right', or 'the paragraph does not say'. 
	   (string-trim str)))))
;; (str:trim " foo ")
;; (str:trim " foo " :left)
;; (str:trim " foo " :left :right)
;; (str:trim " foo " :left :both)


(defun str:empty? (str &optional trim-args)
  "Return non-nil if STR is nil or empty.

See `str:trim' for TRIM-ARGS. TL;DR:
  - `:left', 'left'
  - `:right', `right'
  - `:full', `full', `:both', `both'
  - nil (no trimming)
    - default"
  (or (null str)
      (string= ""
               (if trim-args
                   (str:trim str)
                 str))))
;; (str:empty? nil)
;; (str:empty? "")
;; (str:empty? " ")
;; (str:empty? " " :full)


;;------------------------------------------------------------------------------
;; Split / Join
;;------------------------------------------------------------------------------

(defun str:split (regex string)
  "Split STRING based on REGEX separators; returns a list of strings
with nils filtered out.

NOTE: Obeys `case-fold-search'! Set `case-fold-search' appropriately for your
needs, or use `str:rx:with/case.sensitive' macro if you want to have
case sensitivity!"
  (declare (pure t) (side-effect-free t))

  ;; Uses character ?\0 (ASCII 0; null) as a marker in the string while working on it.
  (let ((separator (string ?\0)))
    (split-string (save-match-data
                    (replace-regexp-in-string regex
                                              separator
                                              string))
                  separator :omit-nulls)))
;; (str:split (rx (one-or-more (not alphanumeric))) "hello:there")


(defun str:join (separator &rest strings)
  "Join STRINGS with SEPARATOR between them."
  (declare (pure t) (side-effect-free t))
  (string-join strings separator))


;;------------------------------------------------------------------------------
;; To String
;;------------------------------------------------------------------------------

(defun int<str>:str:print->str (func &rest args)
  "Calls FUNC with ARGS, returns `standard-output' as a string."
  (with-output-to-string
    (apply func args)))


(defun int<str>:str:insert->str (func &rest args)
  "Calls FUNC with ARGS in a temp buffer, then returns the temp buffer's
contents as a string."
  (with-temp-buffer
    (apply func args)
    (buffer-string)))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :str 'string)

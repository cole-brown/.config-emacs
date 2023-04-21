;;; emacs/str/propertize.el --- Pretty Strings -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <code@brown.dev>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2023-04-17
;; Modified:   2023-04-17
;;
;;; Commentary:
;;
;; Pretty Strings
;;
;;; Code:


(require 'cl-lib)


;;------------------------------------------------------------------------------
;; Svelte Sexy Strings
;;------------------------------------------------------------------------------

(defun str:propertize (string &rest properties)
  "Return a copy of STRING with text PROPERTIES added.

First argument is the string to copy.
Remaining arguments form a sequence of PROPERTY VALUE pairs for text
properties to add to the result.

See Info node `(elisp) Text Properties' for more information.

Other relevant functions are documented in the text-properties group."
  (apply #'propertize string properties))


(cl-defun str:propertize:face/add (string &key start end face append)
  "Add the FACE property from START to END of STRING.

FACE specifies the face to add.  It should be a valid value of the
face property (typically a face name or a plist of face attributes
and values).

If any text in the region already has a non-nil face property, those
face(s) are retained.  This is done by setting the face property to
a list of faces, with FACE as the first element (by default) and the
pre-existing faces as the remaining elements.

If APPEND is non-nil, append FACE to the end of the face list instead."
  (let ((func/name "str:propertize:face/add"))
    ;;------------------------------
    ;; Sanity Checks
    ;;------------------------------
    (cond ((null string)
           ;; Specifically `nil'? Just do nothing; probably supposed to be an empty string?
           nil)
          ((not (stringp string))
           (error "%s: STRING must be a string. Got '%S': %S"
                  func/name
                  (type-of string)
                  string))

          ((and start
                (not (natnump start)))
           (error "%s: START must be nil or a positive integer. Got '%S': %S"
                  func/name
                  (type-of start)
                  start))

          ((and end
                (not (natnump end)))
           (error "%s: END must be nil or a positive integer. Got '%S': %S"
                  func/name
                  (type-of end)
                  end)))

    (if (or (null string)
            (null face))
        ;; Nothing to do or can't do anything, so do nothing.
        string

      ;;------------------------------
      ;; Normalize Inputs
      ;;------------------------------
      (let* ((len   (length string))
             (start (min (or start 0) len))
             (end   (min (or end len) len)))
        ;;------------------------------
        ;; Propertize String
        ;;------------------------------
        (add-face-text-property start
                                end
                                face
                                append
                                string)
        string))))
;; (str:propertize:face/add (str:propertize "there" 'face 'underline) :face 'bold)
;; (str:propertize:face/add (concat "hello " (str:propertize "there" 'face 'underline)) :face 'bold)


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :str 'propertize)

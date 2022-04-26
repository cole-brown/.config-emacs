;;; feature.el --- Adding features to the mantle. -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2022-04-26
;; Modified:   2022-04-26
;; URL:        https://github.com/cole-brown/.config-emacs
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;  Adding features to the mantle.
;;
;;  Not the core innit features...
;;  And not the "crust" of actual user features/packages...
;;  But something in between.
;;  TODO: Will I even use this?
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Mantle Variables
;;------------------------------------------------------------------------------

(defvar innit:features:mantle nil
  "List of `imp' feature lists to load after their core counterpart.

An element in the list would be either 1) just the keyword,
or 2) a specific sub-feature.
  1) '(:path)
  2) '(:path regex)

Each path in the list will optionally load a file (if it is present) during a
specific part of init:
  1) \"init.el\"
     - Just after core's init is finished, and before config.
  2) \"config.el\"
     - Just after core's config is finished, before completing start-up.

Paths should be absolute directory paths. \"init.el\" and \"config.el\" will be
appended to them for looking for the proper file to load.")


;;------------------------------------------------------------------------------
;; Mantle Functions
;;------------------------------------------------------------------------------


(defun innit:feature:mantle:add (caller &rest feature)
  "Add `imp' FEATURE to mantle init/config load sequence.

FEATURE should already have an `imp' root.

CALLER should be string of function or file name which called this."
  ;;------------------------------
  ;; Error Checks
  ;;------------------------------
  ;; TODO: is there an `imp' function to use for validation instead?
  (cond ((null feature)
         (error "[ERROR] innit:feature:mantle:add: %s: FEATURE cannot be nil, got: %S"
                caller
                feature))

        ((not (keywordp (nth 0 feature)))
         (error "[ERROR] innit:feature:mantle:add: %s: FEATURE must start with a keyword! Got: %S"
                caller
                (nth 0 feature)))

        ((not (seq-every-p #'symbolp feature))
         (error "[ERROR] innit:feature:mantle:add: %s: FEATURE must only be keywords/symbols! Got: %S"
                caller
                feature))

        ;;------------------------------
        ;; Valid; add to list.
        ;;------------------------------
        (t
         (push feature innit:features:mantle))))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :innit 'feature)

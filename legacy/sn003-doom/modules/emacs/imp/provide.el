;;; emacs/imp/provide.el -*- lexical-binding: t; -*-

;; imp requirements:
;;   - :imp 'debug
;;   - :imp 'error
;;   - :imp 'path


;;------------------------------------------------------------------------------
;; Public API: Provide
;;------------------------------------------------------------------------------

;; TODO: platform-smart way of figuring out if file-names equal.
;;   - Windows is case-insensitive; forget if macOS is - don't think so.

(defalias 'imp:feature? 'imp:provided?
  "Checks for FEATURE in `imp:features'.")
(defalias 'imp:featurep 'imp:provided?
  "Checks for FEATURE in `imp:features'.")
(defalias 'imp:providedp 'imp:provided?
  "Checks for FEATURE in `imp:features'.")


(defun imp:provide:loading? (&optional file-name)
  "Returns true if loading file.

If FILE-NAME is nil, returns true if loading any file.
If FILE-NAME is a string, returns true if loading that exact
(full path to) file name."
  (if file-name
      ;; Exactly that file loading?
      (and load-in-progress
           (string= load-file-name file-name))
    ;; Just anything loading?
    load-in-progress))


(defun imp:provided? (&rest feature)
  "Checks for FEATURE in `imp:features'."
  (int<imp>:tree:contains? feature imp:features))
;; (imp:provided? :imp)
;; (imp:providedp :imp)
;; (imp:feature? :imp)
;; (imp:featurep :imp)


(defun imp:provide (&rest feature)
  "Record FEATURE in `imp:features' as having been provided.

If you want to provide the feature to emacs as well, you can either:
  1. Use `imp:provide:with-emacs' instead of this to have it automatically
     happen.
     - imp will translate the FEATURE symbol chain via `int<imp>:feature:normalize:imp->emacs'.
  2. Do it yourself by also calling Emacs' `provide' with a symbol of your
     choosing."
  (int<imp>:debug "imp:provide" "Providing feature '%S'..."
                  feature)
  (int<imp>:feature:add feature))
;; (imp:provide :package 'module 'submodule 'feature)


(defun imp:provide:with-emacs (&rest feature)
  "Record FEATURE in `imp:features' and in Emacs' `features' (via
Emacs' `provide') as having been provided.

imp will translate the FEATURE symbol chain via `int<imp>:feature:normalize:imp->emacs' and use
the result for the call to Emacs' `provide'."
  (apply #'imp:provide feature)
  (let ((feature/emacs (int<imp>:feature:normalize:imp->emacs feature)))
    (int<imp>:debug "imp:provide:with-emacs" "Providing to emacs as '%S'..."
                    feature/emacs)
    (provide feature/emacs)))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide:with-emacs :imp 'provide)

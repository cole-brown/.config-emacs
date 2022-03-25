;;; core/boot/10-early/10-bootstrap/90-version.el -*- lexical-binding: t; -*-


;;------------------------------------------------------------------------------
;; Versioning & Early-Init Flag
;;------------------------------------------------------------------------------

;; "init.el" will check that version exists as a check that early-init happened.
(defconst int<spy>:emacs:version
  '(:major 4
    :minor 0
    ;; Can be ISO-8601 or RFC-3339 date or datetime.
    ;; Will get smashed down to just digits.
    :revision "2022-03-23")
  "Plist version data for this Emacs config.")

(defun int<spy>:emacs:version ()
  "Create SemVer string from version plist."
  (format "%d.%d.%s"
          (plist-get int<spy>:emacs:version :major)
          (plist-get int<spy>:emacs:version :minor)
          (replace-regexp-in-string (rx (not digit))
                                    ""
                                    (plist-get int<spy>:emacs:version :revision))))

(defconst spy:emacs:version (int<spy>:emacs:version)
  "Semantic Version string of this Emacs config.")

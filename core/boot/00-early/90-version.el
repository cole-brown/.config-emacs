;;; core/boot/10-early/10-bootstrap/90-version.el -*- lexical-binding: t; -*-


;;------------------------------------------------------------------------------
;; Versioning & Early-Init Flag
;;------------------------------------------------------------------------------

;; "init.el" will check that version exists as a check that early-init happened.
(defconst int<innit>:emacs:version
  '(:major 4
    :minor 0
    ;; Can be ISO-8601 or RFC-3339 date or datetime.
    ;; Will get smashed down to just digits.
    :revision "2022-04-11"
    ;; (spy:datetime/string.get 'iso-8601 'short)
    ;; (spy:datetime/string.get 'yyyymmdd)
    )
  "Plist version data for this Emacs config.")


(defun int<innit>:emacs:version ()
  "Create SemVer string from version plist."
  (format "%d.%d.%s"
          (plist-get int<innit>:emacs:version :major)
          (plist-get int<innit>:emacs:version :minor)
          (replace-regexp-in-string (rx (not digit))
                                    ""
                                    (plist-get int<innit>:emacs:version :revision))))


(defconst innit:emacs:version (int<innit>:emacs:version)
  "Semantic Version string of this Emacs config.")

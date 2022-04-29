;;; vars.el --- It's full of variables, innit? -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2022-04-13
;; Modified:   2022-04-25
;; URL:        https://github.com/cole-brown/.config-emacs
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;  It's full of variables, innit?
;;
;;; Code:


(imp:require :path)


;;------------------------------------------------------------------------------
;; Constants
;;------------------------------------------------------------------------------

;; We already called the core "core", so... The next layer is "mantle", I guess?
;; And a third layer would be called "crust"?

(defconst innit:path:core/boot (path:join user-emacs-directory "core/boot/")
  "Absolute path to the \"core/boot\" subdirectory.")


(defconst innit:path:core/modules (path:join user-emacs-directory "core/modules/")
  "Absolute path to the \"core/modules\" subdirectory.")


(defconst innit:filenames:mantle
  '(:init   "init.el"
    :config "config.el")
  "Names of files to look for in `innit:paths:mantle' for loading.")


(defconst innit:rx:filename
  (rx string-start
      (one-or-more printing) ".el"
      string-end)
  "Base filename must match to be loaded by `innit:load:files:ordered'.")


(defconst innit:interactive? (not noninteractive)
  "If non-nil, Emacs is in interactive mode.")


;; TODO: If none of these used, get rid of (or move to somewhere later/more appropriate).
;; (defconst innit:emacs:28+  (> emacs-major-version 27))
;; (defconst innit:emacs:29+  (> emacs-major-version 28))
;; (defconst innit:os:mac     (eq system-type 'darwin))
;; (defconst innit:os:linux   (eq system-type 'gnu/linux))
;; (defconst innit:os:windows (memq system-type '(cygwin windows-nt ms-dos)))
;; (defconst innit:os:bsd     (or IS-MAC (eq system-type 'berkeley-unix)))


;;------------------------------------------------------------------------------
;; Versioning & Early-Init Flag
;;------------------------------------------------------------------------------

;; "init.el" will check that version exists as a check that early-init happened.
(defconst int<innit>:version
  '(:major 4
    :minor 0
    ;; Can be ISO-8601 or RFC-3339 date or datetime.
    ;; Will get smashed down to just digits.
    :revision "2022-04-27"
    ;; (spy:datetime/string.get 'iso-8601 'short)
    ;; (spy:datetime/string.get 'yyyymmdd)
    )
  "Plist version data for this Emacs config.")


(defun int<innit>:version ()
  "Create SemVer string from version plist."
  (format "%d.%d.%s"
          (plist-get int<innit>:version :major)
          (plist-get int<innit>:version :minor)
          (replace-regexp-in-string (rx (not digit))
                                    ""
                                    (plist-get int<innit>:version :revision))))


(defconst innit:version (int<innit>:version)
  "Semantic Version string of this Emacs config.")


;;------------------------------------------------------------------------------
;; Non-`innit' Settings
;;------------------------------------------------------------------------------

(defmacro innit:settings:optional (settings &rest body)
  "Safely handle running BODY on SETTINGS which may or may not exist.

SETTINGS should be a symbol or list of symbols. Will check that they all are
bound before running BODY."
  (cond ((symbolp settings)
         (when (boundp settings)
           `(progn
             ,@body)))

        ((listp settings)
         (let ((valid :init))
           (dolist (setting settings)
             (setq valid (and valid
                              (symbolp setting)
                              (boundp setting))))
           (when valid
             `(progn
                ,@body))))

        (t
         ;; Just ignore anything else?
         nil)))
;; (innit:settings:optional inhibit-redisplay (message "print me"))
;; (innit:settings:optional innit:test:dne (message "don't print me"))
;; (innit:settings:optional (inhibit-redisplay track-mouse) (message "print me"))
;; (innit:settings:optional (innit:test:dne) (message "don't print me"))
;; (innit:settings:optional "ignored?" (message "don't print me"))


;;------------------------------------------------------------------------------
;; Customs
;;------------------------------------------------------------------------------

(defgroup innit:group nil
  "An Emacs framework for running similar inits on one or more systems."
  :prefix "innit:"
  :group 'tools
  :link '(url-link "https://github.com/cole-brown/.config-emacs"))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :innit 'vars)
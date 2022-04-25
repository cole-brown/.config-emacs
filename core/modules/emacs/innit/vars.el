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


;;------------------------------------------------------------------------------
;; Variables
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


(defvar innit:status nil
  "Alist of innit sequence keyword to status.")


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

;;; nub.el --- innit's nub's settings & such -*- lexical-binding: t; -*-
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
;;  innit's nub's settings & such
;;
;;; Code:


(imp:require :nub)
(imp:require :innit 'vars)


;;------------------------------------------------------------------------------
;; Customs
;;------------------------------------------------------------------------------

(defcustom innit:nub:pop-to-buffer t
  "Allow `nub' to pop to the `innit' output buffer every time a message is output.

Only used in `innit:nub:sink-fn'."
  :group 'innit:group
  :type  '(boolean))


(defcustom innit:nub:buffer-name "ⓘ-innit-ⓘ"
  "Buffer name for `nub` messages to `innit` default output sink.

Only used in `innit:nub:sink-fn'."
  :group 'innit:group
  :type  '(string))


(defcustom innit:nub:levels:prefixes nil
  "`nub` message prefix strings per output level.

nil means use nub's defaults."
  :group 'innit:group
  :type  '(alist :key-type (choice (const :error)
                                   (const :warning)
                                   (const :info)
                                   (const :debug)
                                   (const nil))
                 :value-type string))


(defcustom innit:nub:levels:enabled nil
  "`nub` output levels enabled/disabled.

nil means use nub's defaults."
  :group 'innit:group
  :type  '(alist :key-type (choice (const :error)
                                   (const :warning)
                                   (const :info)
                                   (const :debug)
                                   (const nil))
                :value-type boolean))


;; An output sink function for nub.
(defvar innit:nub:sink-fn (nub:output:sink :innit
                                           innit:nub:buffer-name
                                           innit:nub:pop-to-buffer)
  "`nub` output level sink function for `innit:nub:sinks'.")


(defcustom innit:nub:sinks
  (list (cons :error   (list innit:nub:sink-fn :default))
        (cons :warning (list innit:nub:sink-fn :default))
        (cons :info    (list innit:nub:sink-fn :default))
        (cons :debug   (list innit:nub:sink-fn :default)))
  "`nub` output level sink functions.

Add our sink to all so that they get collected there as well as output by
default funcs.

nil means use nub's defaults."
  :group 'innit:group
  :type  '(alist :key-type (choice (const :error)
                                   (const :warning)
                                   (const :info)
                                   (const :debug))
                 :value-type (repeat function
                                     (const :default))))


;; TODO-00: Does `nub' even support tags (yet)?
;; TODO-00:   - Don't see a way to tag `nub' messages...
;; TODO-00:   - But I do see a lot of vars/funcs for nub tags so... ¯\_(ツ)_/¯
;;
;; TODO-01: s/defvar/defcustom/ & add some default tags & tag innit's nub messages once I figure out nub tags?
(defvar innit:nub:debug-tags:interactive nil
  "Default debug tags to show as auto-complete hints for `nub'."
  ;; :group 'innit:group
  ;; :type  (restricted-sexp :match-alternatives
  ;;                         (keywordp 'nil))
  )


;;------------------------------------------------------------------------------
;; Initialize `nub`...
;;------------------------------------------------------------------------------

(defun innit:nub:init ()
  "Initialize `innit' user & settings in `nub'."
  (nub:vars:init :innit
                 innit:nub:debug-tags:interactive ; common debug tags (for interactive toggling auto-complete help)
                 innit:nub:levels:prefixes ; output message prefixes
                 innit:nub:levels:enabled  ; default enabled/disabled per output levels
                 innit:nub:sinks)) ; output sinks by level
;; (innit:nub:init)


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :innit 'nub)

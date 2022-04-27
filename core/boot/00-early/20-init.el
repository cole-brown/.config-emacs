;;; 20-init.el --- Early Init -*- lexical-binding: t; -*-
;;
;; Author: Cole Brown <code@brown.dev>
;; URL:    https://github.com/cole-brown/.config-emacs
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;; Code:




;;------------------------------------------------------------------------------
;; [Speed]: Interactive vs Non-interactive
;;------------------------------------------------------------------------------

;; TODO: Safe to do this or not? We're not pre-compiling everything like Doom is.
;;
;; ;; In noninteractive sessions, prioritize non-byte-compiled source files to
;; ;; prevent the use of stale byte-code. Otherwise, it saves us a little IO time
;; ;; to skip the mtime checks on every *.elc file.
;; (setq load-prefer-newer noninteractive)

;; Run these unless Emacs is in:
;;   - daemon (service) mode
;;   - noninteractive (batch/script) mode
;;   - Any sort of debug mode.
(when (innit:optimize?)
  ;; Save & empty `file-name-handler-alist' to speed up opening files (e.g. for
  ;; loading elisp) during start-up. Reverts by merging original
  ;; `file-name-handler-alist' with any additions to the empty list that
  ;; happened during start-up.
  (innit:optimize:file-name-handler-alist:inhibit)

  ;; Speed up start-up a bit by inhibiting redrawing display, writing messages.
  (innit:optimize:display:inhibit)
  (innit:optimize:load-message:inhibit))


;; Always set up our Emacs start-up timing benchmark.
(innit:time:init)


;;------------------------------
;; UTF-8
;;------------------------------

;; Contrary to what many Emacs users have in their configs, you don't need
;; more than this to make UTF-8 the default coding system:
(set-language-environment "UTF-8")

;; set-language-enviornment sets default-input-method, which is unwanted
(setq default-input-method nil)


;;------------------------------------------------------------------------------
;; The End
;;------------------------------------------------------------------------------

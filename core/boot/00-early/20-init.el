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
  (let ((file-name-handler-alist:orig file-name-handler-alist))
    ;; `file-name-handler-alist' is consulted on each `require', `load' and
    ;; various path/io functions. You get a minor speed up by unsetting this.
    ;; Some warning, however: this could cause problems on builds of Emacs where
    ;; its site lisp files aren't byte-compiled and we're forced to load the
    ;; *.el.gz files (e.g. on Alpine).
    (setq-default file-name-handler-alist nil)

    ;; ...but restore `file-name-handler-alist' later, because it is needed for
    ;; handling encrypted or compressed files, among other things.
    (defun innit:hook:file-handler-alist:reset ()
      "Merge original `file-handler-alist' with any additions during Emacs start-up."
      (setq file-name-handler-alist
            ;; Merge instead of overwrite because there may have bene changes to
            ;; `file-name-handler-alist' since startup we want to preserve.
            (delete-dups (append file-name-handler-alist
                                 file-name-handler-alist:orig))))
    (add-hook 'emacs-startup-hook #'innit:hook:file-handler-alist:reset 101))

  ;; Premature redisplays can substantially affect startup times and produce
  ;; ugly flashes of unstyled Emacs.
  (setq-default inhibit-redisplay t)

  (unless innit:display:messages?
    ;; Writing to the *Messages* buffer & minibuffer slows down startup as well.
    (setq-default inhibit-message t))

  (defun innit:hook:inhibit-display:reset ()
    "Re-enable display, messages."
    (setq-default inhibit-redisplay nil
                  inhibit-message nil)
    (redisplay))
  (add-hook 'window-setup-hook #'innit:hook:inhibit-display:reset)


  (unless innit:display:load-file
    ;; Site files tend to use `load-file', which emits "Loading X..." messages in
    ;; the echo area, which in turn triggers a redisplay. Redisplays can have a
    ;; substantial effect on startup times and in this case happens so early that
    ;; Emacs may flash white while starting up.
    (define-advice load-file (:override (file) innit-mute)
      (load file nil 'nomessage))

    ;; Undo our `load-file' advice above, to limit the scope of any edge cases it
    ;; may introduce down the road.
    (define-advice startup--load-user-init-file (:before (&rest _) innit-unmute)
      (advice-remove #'load-file #'load-file@innit-mute))))


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

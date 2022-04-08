;;; core/boot/00-early/00-bootstrap.el -*- lexical-binding: t; -*-

;;------------------------------------------------------------------------------
;; Bootstrap
;;------------------------------------------------------------------------------

;;------------------------------
;; [Speed]: Garbage Collection
;;------------------------------

;; A big contributor to startup times is garbage collection. We up the gc
;; threshold to temporarily prevent it from running, then reset it later by
;; enabling `gcmh-mode'. Not resetting it will cause stuttering/freezes.
;; See GCMH: https://github.com/emacsmirror/gcmh
(setq gc-cons-threshold most-positive-fixnum)


;;------------------------------
;; [Init]: Packages
;;------------------------------

;; In Emacs 27+, package initialization occurs before `user-init-file' is
;; loaded, but after `early-init-file'. So if we want to handle package init
;; ourselves, set this:
;; (setq package-enable-at-startup nil)


;;------------------------------
;; [Speed]: Interactive vs Non-interactive
;;------------------------------

;; TODO: Safe to do this or not? We're not pre-compiling everything like Doom is.
;;
;; ;; In noninteractive sessions, prioritize non-byte-compiled source files to
;; ;; prevent the use of stale byte-code. Otherwise, it saves us a little IO time
;; ;; to skip the mtime checks on every *.elc file.
;; (setq load-prefer-newer noninteractive)

(unless (or (daemonp)
            noninteractive
            init-file-debug)
  (let ((old-file-name-handler-alist file-name-handler-alist))
    ;; `file-name-handler-alist' is consulted on each `require', `load' and
    ;; various path/io functions. You get a minor speed up by unsetting this.
    ;; Some warning, however: this could cause problems on builds of Emacs where
    ;; its site lisp files aren't byte-compiled and we're forced to load the
    ;; *.el.gz files (e.g. on Alpine).
    (setq-default file-name-handler-alist nil)

    ;; ...but restore `file-name-handler-alist' later, because it is needed for
    ;; handling encrypted or compressed files, among other things.
    (defun innit:file-handler-alist:reset/hook ()
      "Merge original `file-handler-alist' with any additions during Emacs start-up."
      (setq file-name-handler-alist
            ;; Merge instead of overwrite because there may have bene changes to
            ;; `file-name-handler-alist' since startup we want to preserve.
            (delete-dups (append file-name-handler-alist
                                 old-file-name-handler-alist))))
    (add-hook 'emacs-startup-hook #'innit:file-handler-alist:reset/hook 101))

  ;; Premature redisplays can substantially affect startup times and produce
  ;; ugly flashes of unstyled Emacs.
  (setq-default inhibit-redisplay t
                inhibit-message t)
  (defun innit:inhibit-display:reset/hook ()
    "Re-enable display, messages."
    (setq-default inhibit-redisplay nil
                  inhibit-message nil)
    (redisplay))
  (add-hook 'window-setup-hook #'innit:inhibit-display:reset/hook)

  ;; Site files tend to use `load-file', which emits "Loading X..." messages in
  ;; the echo area, which in turn triggers a redisplay. Redisplays can have a
  ;; substantial effect on startup times and in this case happens so early that
  ;; Emacs may flash white while starting up.
  (define-advice load-file (:override (file) innit-mute)
    (load file nil 'nomessage))

  ;; Undo our `load-file' advice above, to limit the scope of any edge cases it
  ;; may introduce down the road.
  (define-advice startup--load-user-init-file (:before (&rest _) innit-unmute)
    (advice-remove #'load-file #'load-file@innit-mute)))


;;------------------------------
;; UTF-8
;;------------------------------

;; Contrary to what many Emacs users have in their configs, you don't need
;; more than this to make UTF-8 the default coding system:
(set-language-environment "UTF-8")

;; set-language-enviornment sets default-input-method, which is unwanted
(setq default-input-method nil)


;;------------------------------
;; `user-emacs-directory'
;;------------------------------

;; Ensure the rest of Emacs init is running out of this file's directory.
;; Chemacs2 or something could have handed off to us.
(setq user-emacs-directory (file-name-directory load-file-name))


;;------------------------------------------------------------------------------
;; The End
;;------------------------------------------------------------------------------

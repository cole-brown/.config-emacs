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
;; Variables
;;------------------------------------------------------------------------------

(defconst innit:interactive? (not noninteractive)
  "If non-nil, Emacs is in interactive mode.")


(defvar innit:time nil
  "The time it took, in seconds, for Emacs & `innit' to finish set-up.")


;; TODO: If none of these used, get rid of (or move to somewhere later/more appropriate).
;; (defconst innit:emacs:28+  (> emacs-major-version 27))
;; (defconst innit:emacs:29+  (> emacs-major-version 28))
;; (defconst innit:os:mac     (eq system-type 'darwin))
;; (defconst innit:os:linux   (eq system-type 'gnu/linux))
;; (defconst innit:os:windows (memq system-type '(cygwin windows-nt ms-dos)))
;; (defconst innit:os:bsd     (or IS-MAC (eq system-type 'berkeley-unix)))


;;------------------------------------------------------------------------------
;; Verbosity / Output Variables
;;------------------------------------------------------------------------------

(defconst innit:verbosity:valid '(:critical
                                  :error
                                  :warning
                                  :info
                                  :debug
                                  t
                                  nil)
  "Valid verbosity tags.

1) keywords: specific verbosity tag
2) t:        enable all verbosity tags
3) nil:      none/disabled")


(defvar innit:verbosity (if (innit:debug?)
                            '(t)
                          nil)
  "List of allowed verbosity tags of `innit'.

See `innit:verbosity:valid' for all valid values.

Defaults to:
  - `t' (all) if debugging
  - `nil' (none) if not")


(defvar innit:display:messages? nil
  "Allow output to *Messages* buffer during init?

Default to no messages (nil).")


(defvar innit:display:load-file nil
  "Allow `load-file' to output its message(s) during init?

Default to no (nil).")


;;------------------------------------------------------------------------------
;; Time
;;------------------------------------------------------------------------------

(defun innit:hook:benchmark (&optional string?)
  "Display a benchmark including number of packages and modules loaded.

If string?, return the message as a string instead of displaying it."

  ;; TODO: A message for innit?
  ;;   - Will be more complicated than this...
  ;;   - There's stuff that `innit' loaded that may not have any `provide'.
  ;;   - There's stuff `imp' loaded that is only in `imp'.
  ;;   - There's stuff `imp' loaded that is also in Emacs' feature list.
  ;; (funcall (if string? #'format #'message)
  ;;          "`innit' loaded %d packages across %d modules in %.03fs"
  ;;          (- (length load-path) (length (get 'load-path 'initial-value)))
  ;;          (if doom-modules (hash-table-count doom-modules) 0)
  ;;          (or doom-init-time
  ;;              (setq doom-init-time
  ;;                    (float-time (time-subtract (current-time) before-init-time)))))
  (funcall (if string? #'format #'message)
           "`innit' loaded x things across y places in %.03fs"
           (or doom-init-time
               (setq doom-init-time
                     (float-time (time-subtract (current-time) before-init-time))))))
;; (innit:hook:benchmark)


;;------------------------------------------------------------------------------
;; Custom Error Types
;;------------------------------------------------------------------------------

;; TODO: Trim down to just what I'm using?
(define-error 'innit:error          "Error in Innit Emacs core")
(define-error 'innit:error:hook     "Error in an Innit startup hook"  'innit:error)
(define-error 'innit:error:autoload "Error in Innit's autoloads file" 'innit:error)
(define-error 'innit:error:module   "Error in an Innit module"        'innit:error)
(define-error 'innit:error:private  "Error in private config"         'innit:error)
(define-error 'innit:error:package  "Error with packages"             'innit:error)


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
(unless (or (daemonp)
            noninteractive
            (innit:debug? :any))
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
  ;; Writing to the *Messages* buffer & minibuffer slows down startup as well.
  (unless innit:display:messages?
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

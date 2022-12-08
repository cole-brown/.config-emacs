;;; core/modules/emacs/buffer/type.el --- Buffer Types -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2022-12-01
;; Modified:   2022-12-01
;; URL:        https://github.com/cole-brown/.config-emacs
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Buffer Types
;;
;; Special       - Buffer name starts with "*".
;; Dired         - Buffer mode is Dired.
;; Temp          - Buffer name starts with " ".
;; Visible       - Buffer is displayed in a window.
;; Buried        - Not visible.
;; File-Visiting - Buffer has a backing file.
;; "Real"        - One of:
;;    1. A non-nil value for the buffer-local variable `buffer:type:real?' variable.
;;    2. Any function in `buffer:type:functions/real' returns non-nil
;;    3. Any function in `buffer:type:functions/unreal' returns nil.
;; "Unreal"      - Not "real".
;;
;;; Code:


(require 'cl-lib)


;;------------------------------------------------------------------------------
;; Variables
;;------------------------------------------------------------------------------

;;;###autoload
(defvar-local buffer:type:real? nil
  "If non-nil, this buffer should be considered \"real\" no matter what.

See function `buffer:type:real?' for more information.

Via Doom's `doom-real-buffer-p' in \"core/autoload/buffers.el\".")


;;;###autoload
(defvar buffer:type:functions/real
  (list #'buffer:type:dired?)
  "A list of predicate functions to determine if a buffer is real.

Functions should accept one argument: the buffer to be tested. E.g.:
  (defun my:buffer:unreal? (buffer)
    ...)

Should any of its function returns non-nil, the rest of the functions are
ignored and the buffer is considered real.

See `buffer:type:real?' for more information.

Via Doom's `doom-real-buffer-functions' in \"core/autoload/buffers.el\".")


;;;###autoload
(defvar buffer:type:functions/unreal
  (list #'minibufferp
        #'buffer:type:special?
        #'buffer:type:non-file-visiting?)
  "A list of predicate functions to determine if a buffer is *not* real.

Inverse of `buffer:type:functions/real'.

Functions should accept one argument: the buffer to be tested. E.g.:
  (defun my:buffer:unreal? (buffer)
    ...)

Should any of these functions return non-nil, the rest of the functions are
ignored and the buffer is considered unreal.

See `buffer:type:real?' for more information.

Via Doom's `doom-unreal-buffer-functions' in \"core/autoload/buffers.el\".")


;;------------------------------------------------------------------------------
;; Type Predicates
;;------------------------------------------------------------------------------

;;;###autoload
(defun buffer:type:dired? (buffer)
  "Return non-nil if BUFFER is a Dired buffer.

Via Doom's `doom-dired-buffer-p' in \"core/autoload/buffers.el\"."
  (provided-mode-derived-p (buffer-local-value 'major-mode buffer)
                           'dired-mode))

;;;###autoload
(defun buffer:type:special? (buffer)
  "Return non-nil if BUFFER's name starts and ends with \"*\".

Via Doom's `doom-special-buffer-p' in \"core/autoload/buffers.el\"."
  (equal (substring (buffer-name buffer) 0 1) "*"))


;;;###autoload
(defun buffer:type:temp? (buffer)
  "Returns non-nil if BUFFER is temporary.

Via Doom's `doom-temp-buffer-p' in \"core/autoload/buffers.el\"."
  (equal (substring (buffer-name buffer) 0 1) " "))


;;;###autoload
(defun buffer:type:visible? (buffer)
  "Return non-nil if BUFFER is visible.

Via Doom's `doom-visible-buffer-p' in \"core/autoload/buffers.el\"."
  (get-buffer-window buffer))


;;;###autoload
(defun buffer:type:buried? (buffer)
  "Return non-nil if BUFFER is not visible.

Via Doom's `doom-buried-buffer-p' in \"core/autoload/buffers.el\"."
  (not (buffer:type:visible? buffer)))


;;;###autoload
(defun buffer:type:file-visiting? (buffer)
  "Return non-nil if BUFFER does have a value from function `buffer-file-name'.

Bastardization of Doom's `doom-non-file-visiting-buffer-p' in
\"core/autoload/buffers.el\"."
  (buffer-file-name buffer))


;;;###autoload
(defun buffer:type:non-file-visiting? (buffer)
  "Return non-nil if BUFFER does have a value from function `buffer-file-name'."
  (not (buffer:type:file-visiting? buffer)))


;;;###autoload
(defun buffer:type:real? (buffer-or-name)
  "Return non-nil if BUFFER-OR-NAME is a \"real\" buffer.

A real buffer is a useful buffer (e.g. one you might want to save in your
perspective/tabs/whatever for your next Emacs session). Real buffers should get
special treatment, because we will be spending most of our time in them. Unreal
ones should be low-profile and easy to cast aside, so we can focus on real ones.

A \"real buffer\" must be one of (in order of priority):
  1. A non-nil value for the buffer-local variable `buffer:type:real?' variable.
  2. Any function in `buffer:type:functions/real' returns non-nil
  3. Any function in `buffer:type:functions/unreal' returns nil.

If BUFFER-OR-NAME is omitted or nil, the current buffer is tested.

Via Doom's `doom-real-buffer-p' in \"core/autoload/buffers.el\"."
  ;;------------------------------
  ;; Error Check
  ;;------------------------------
  (or (bufferp buffer-or-name)
      (stringp buffer-or-name)
      (signal 'wrong-type-argument (list '(bufferp stringp) buffer-or-name)))
  ;;------------------------------
  ;; Real Buffer?
  ;;------------------------------
  (when-let ((buffer (get-buffer buffer-or-name)))
    ;; If there is a base buffer, we want that instead.
    (when-let ((base-buffer (buffer-base-buffer buffer)))
      (setq buffer base-buffer))
    ;; Is it really real?
    (and (buffer-live-p buffer)
         (not (buffer:type:temp? buffer))
         (or (buffer-local-value 'buffer:type:real? buffer)
             (run-hook-with-args-until-success 'buffer:type:functions/real buffer)
             (not (run-hook-with-args-until-success 'buffer:type:functions/unreal buffer))))))


;;;###autoload
(defun buffer:type:unreal? (buffer-or-name)
  "Return non-nil if BUFFER-OR-NAME is an \"unreal\" buffer.

See `buffer:type:real?' for details on what that means.

Via Doom's `doom-unreal-buffer-p' in \"core/autoload/buffers.el\"."
  (not (buffer:type:real? buffer-or-name)))


;;------------------------------------------------------------------------------
;; Buffer List
;;------------------------------------------------------------------------------

(defun buffer:list (&optional perspective)
  "Get a list of buffer objects for PERSPECTIVE.

PERSPECTIVE can be a string (name of a perspective) or a `persp-mode'
perspective (satisfies `perspective-p'). If nil or omitted, it defaults to the
current workspace.

If `persp-mode' is enabled, use its list. Else use `buffer-list'."
  ;; Hide all the `persp-mode' stuff behind a check for it.
  (if (bound-and-true-p persp-mode)
      ;; Get list of PERSPECTIVE's buffers.
      (let ((perspective (or perspective (get-current-persp))))
        (unless (perspective-p perspective)
          (user-error "Not in a valid perspective (%s)" perspective))
        (persp-buffers perspective))

    ;; No `persp-mode'; give the list of everything.

    ;; NOTE: Sending PERSPECTIVE as FRAME arg of `buffer-list'. It's...
    ;; what Doom does with `doom-buffer-list' / `+workspace-buffer-list', except
    ;; that's done with defalias & override advice, so it's very implied.
    (buffer-list perspective)))


;;;###autoload
(defun buffer:type:real/list (&optional buffer-list)
  "Return a list of buffers that satisfy `buffer:type:real?'.

If BUFFER-LIST is nil, use return value of `buffer:list'.

Via Doom's `doom-real-buffer-list' in \"core/autoload/buffers.el\"."
  (cl-remove-if-not #'buffer:type:real? (or buffer-list (buffer:list))))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :buffer 'type)

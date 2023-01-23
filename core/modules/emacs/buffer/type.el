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

(imp:require :elisp 'utils 'functions)


;;------------------------------------------------------------------------------
;; Types
;;------------------------------------------------------------------------------

(defconst buffer:types
  (list (cons nil                 #'buffer:type:any?)
        (cons :any                #'buffer:type:any?)
        (cons :dired              #'buffer:type:dired?)
        (cons :special            #'buffer:type:special?)
        (cons :temp               #'buffer:type:temp?)
        (cons :visible            #'buffer:type:visible?)
        (cons :buried             #'buffer:type:buried?)
        (cons :file-visiting      #'buffer:type:file-visiting?)
        (cons :non-file-visiting  #'buffer:type:non-file-visiting?)
        (cons :real               #'buffer:type:real?)
        (cons :unreal             #'buffer:type:unreal?))
  "Alist of buffer type keywords to buffer type predicate/filter functions.")


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
(defun buffer:type:any? (buffer)
  "Return non-nil if BUFFER is... any... type of... buffer?

Return `bufferp'"
  (bufferp buffer))


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

(cl-defun buffer:list (&key (perspective 'all) type)
  "Get a list of buffer objects.

PERSPECTIVE can be a string (name of a perspective) or a `persp-mode'
perspective (satisfies `perspective-p').
  - If nil or `current', it defaults to the current workspace.
  - If not supplied or `all', it defaults to the full, non-`persp-mode'-aware
    buffer list.

TYPE should be nil or a keyword from `buffer:types'."
  (let ((func/name "buffer:list") ; TODO-nub: Use `nub:error' in this
        persp/obj)

  ;;------------------------------
  ;; Filter list down to TYPE?
  ;;------------------------------
  (unless (assoc type buffer:types)
    (error "%s: `%S' is not a valid buffer type! Valid (see `buffer:types'): (%S)"
           func/name
           type
           (seq-map #'car buffer:types)))

  (seq-map (alist-get type buffer:types)
           ;;------------------------------
           ;; Get complete (perspective's?) buffer list.
           ;;------------------------------
           (if (or (null perspective)
                   (eq perspective 'all))
               ;;------------------------------
    ;; No `persp-mode'; give the list of everything.
               ;;------------------------------

               ;; NOTE: ...Could send PERSPECTIVE as FRAME arg of `buffer-list'. It's
    ;; what Doom does with `doom-buffer-list' / `+workspace-buffer-list', except
    ;; that's done with defalias & override advice, so it's very implied.
               ;; But I don't know why anyone would do that since
               ;; `persp-buffers' doesn't have a FRAME arg so they cannot be
               ;; equivalent?
               (buffer-list)

             ;;------------------------------
             ;; `persp-mode'-aware list.
             ;;------------------------------
             ;;---
             ;; No `persp-mode'? -> Error!
             ;;---
             (unless (bound-and-true-p persp-mode)
               (when (not (boundp persp-mode))
                 (error "%s: `persp-mode' not installed or loaded; cannot get a buffer list! perspective: %S"
                        func/name
                        perspective))
               ;; else: (not persp-mode)
               (error "%s: `persp-mode' not enabled; cannot get a buffer list for perspective: %S"
                      func/name
                      perspective))

             ;;---
             ;; `persp-mode'
             ;;---
             ;; Get persp-mode's perspective object.
             (cond ((eq perspective 'current)
                    (setq persp/obj (get-current-persp)))

                   ((stringp perspective)
                    (setq persp/obj (persp-get-by-name perspective))
                    (when (eq persp/obj :nil) ; `persp-get-by-name' returns keyword `:nil' instead of nil.
                      (error "%s: PERSPECTIVE (%s) is not a valid perspective name. Valid: %s"
                             func/name
                             perspective
                             (persp-names-sorted))))

                   (t
                    (error "%s: PERSPECTIVE must be: nil, `current', `all', or a string. Got: %S"
                              func/name
                              perspective)))

             ;; Get list of buffers.
             (persp-buffers persp/obj)))))
;; (buffer:list)
;; (buffer:list :perspective nil)
;; (buffer:list :perspective "emacs")


;; TODO: Combine this with `buffer:list' too (add `:mode' keyword param?)?
;;;###autoload
(defun buffer:list:mode (modes &optional buffer-list derived-p)
  "Return a list of buffers whose `major-mode' is `eq' to MODES.

MODES must be a symbol or list of symbols.

BUFFER-LIST, if nil, will be return value of function `buffer:list', which is
perspective-aware if `persp-mode'. For all buffers, use function `buffer-list'
instead.

If DERIVED-P, test with `derived-mode-p', otherwise use `eq'."
  (let ((modes (elisp:list:listify modes)))
    ;; Filter list down to just...
    (cl-remove-if-not (if derived-p
                          ;; Mode and derived modes:
                          (lambda (buffer)
                            (apply #'provided-mode-derived-p
                                   (buffer-local-value 'major-mode buffer)
                                   modes))
                        ;; Mode exactly:
                        (lambda (buffer)
                          (memq (buffer-local-value 'major-mode buffer) modes)))
                      (or buffer-list (buffer:list)))))


;;------------------------------------------------------------------------------
;; Fallback Buffer
;;------------------------------------------------------------------------------

(defcustom buffer:fallback:name "*scratch*"
  "Buffer name string for `buffer:get-or-create:fallback'.")


(defun buffer:fallback:get-or-create ()
  "Return the fallback buffer, creating it if necessary.

See `buffer:fallback:name' for its name.

From Doom's `doom-fallback-buffer' in \"core/autoload/buffers.el\"."
  (let (buffer-list-update-hook)
    (get-buffer-create buffer:fallback:name)))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :buffer 'type)

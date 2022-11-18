;;; emacs/buffer/name.el -*- lexical-binding: t; -*-


(require 'cl-lib)


;;----------------------------------Buffers------------------------------------
;;--                    And Things to Make Them Better.                      --
;;-----------------------------------------------------------------------------

(defcustom buffer:format:bookend/normal
  '("§-" "-§")
  "Start/end strings for special-name formats."
  :group 'buffer:group
  :type '(list string string))


(defcustom buffer:format:bookend/high
  '("§!" "!§")
  "Start/end strings for special-name formats."
  :group 'buffer:group
  :type '(list string string))


(defcustom buffer:format:bookend/info
  '("ⓘ-" "-ⓘ")
  "Start/end strings for special-name formats."
  :group 'buffer:group
  :type '(list string string))


(defcustom buffer:format:priorities
  '((:low    . buffer:format:bookend/normal) ;; no actual low right now
    (:medium . buffer:format:bookend/normal)
    (:high   . buffer:format:bookend/high)

    ;; un-normal priority levels
    (:info   . buffer:format:bookend/info))
  "Priority (for `buffer:special-name') to bookend consts."
  :group 'buffer:group
  :type '(alist :key-type symbol :value-type symbol))


(defcustom buffer:regex:bookend
  (rx
   ;; Start Bookend
   ;; Will need to update (or make smarter) if get more actual priority levels.
   (or (eval (nth 0 buffer:format:bookend/normal))
       (eval (nth 0 buffer:format:bookend/high))
       (eval (nth 0 buffer:format:bookend/info)))

   ;; Actual Buffer Name
   (one-or-more printing)

   ;; End Bookend
   ;; Will need to update (or make smarter) if get more actual priority levels.
   (or (eval (nth 1 buffer:format:bookend/normal))
       (eval (nth 1 buffer:format:bookend/high))
       (eval (nth 1 buffer:format:bookend/info))))

  "Regex for matching a bookended buffer name string.
Will need to update (or make smarter) if get more actual priority levels."
  :group 'buffer:group
  :type 'regexp)


(defcustom buffer:regex:specials
  (rx
   ;;---
   ;; Begin
   ;;---
   line-start
   (or
    ;;---
    ;; Emacs
    ;;---
    ;; Special buffers start with "*", optionally with leading space.
    (group
     (optional " ")
     "*" ;; literal asterisk
     (one-or-more printing)
     "*")

    ;;---
    ;; :emacs/buffer
    ;;---
    ;; Bookended Buffer Names are special
    (group
     (optional " ") ;; Allow optional make-less-visible leading space.
     ;; Start Bookend
     (or (eval (nth 0 buffer:format:bookend/normal))
         (eval (nth 0 buffer:format:bookend/high))
         (eval (nth 0 buffer:format:bookend/info)))

     ;; Actual Buffer Name
     (one-or-more printing)

     ;; End Bookend
     (or (eval (nth 1 buffer:format:bookend/normal))
         (eval (nth 1 buffer:format:bookend/high))
         (eval (nth 1 buffer:format:bookend/info)))))
   ;;---
   ;; Done
   ;;---
   line-end)
  "Regexp for matching a special buffer name string. Special buffers are:
  - Emacs' special, visible \"*<name>*\" buffers.
  - Emacs' special, less-visible \" *<name>*\" buffers (leading space).
  - :emacs/buffer's special bookended buffers: \"§- <name> -§\" (and other bookends)."
  :group 'buffer:group
  :type 'regexp)


;;-----------------------------------------------------------------------------
;; Naming Functions
;;-----------------------------------------------------------------------------

(defun buffer:name:special (title &optional desc priority)
  "Format TITLE and DESC strings for `buffer:name:special' with PRIORITY.

PRIORITIES can be: :low, :medium, or :high.

TITLE and DESC are formatted by bookending with
`buffer:format:priorities' bookends based on PRIORITY
setting, with nil being medium priority."

  ;; PRIORITY is either known or forced to medium
  (let ((priority (if (assoc priority buffer:format:priorities)
                      priority
                    :medium))
        ;; look for bookends in list, default if fail/nil
        (bookends (or (symbol-value
                       (cdr (assoc priority buffer:format:priorities)))
                      buffer:format:bookend/normal))
        ;; "title" or "title: desc"
        (inner-fmt (if (null desc) "%s" "%s: %s")))

    ;; inner format: "title" or "title: desc", as decided above by `inner-fmt'
    (format
     ;; outer format: "<bookend> <inner-format-%s> <bookend>"
     (format "%s %s %s"
             (nth 0 bookends)
             inner-fmt
             (nth 1 bookends))
     title desc)))
;; (buffer:name:special "jeff")
;; (buffer:name:special "jeff" "is here")
;; (buffer:name:special "jeff" nil :high)
;; (buffer:name:special "jeff" "is here" :high)


(defun buffer:special? (buffer-or-name)
  "Returns non-nil if BUFFER-OR-NAME is a specially named buffer.

Special buffers are:
  - Emacs' special, visible \"*<name>*\" buffers.
  - Emacs' special, less-visible \" *<name>*\" buffers (leading space).
  - :emacs/buffer's special bookended buffers: \"§- <name> -§\" (and other bookends)."
  (string-match-p
   buffer:regex:specials
   (if (bufferp buffer-or-name)
       (buffer-name buffer-or-name)
     buffer-or-name)))
;; (buffer:special? (buffer:name:special "jeff"))
;; (buffer:special? "*Messages*")
;; (buffer:special? "file.txt")


;;------------------------------------------------------------------------------
;; Copy Buffer File/Dir Name Functions
;;------------------------------------------------------------------------------

;; This (or similar (prelude-copy-file-name-to-clipboard)) used to be in Prelude
;; Emacs.
;;   https://github.com/bbatsov/prelude/issues/764
(defun buffer:cmd:clipboard:file-name ()
  "Copy the current buffer's file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      ;; Copy to the clipboard and kill ring so it's available outside Emacs.
      (with-temp-buffer
        (insert filename)
        (clipboard-kill-region (point-min) (point-max)))
      (message "Copied buffer file name '%s' to the clipboard." filename))))


;; This is more complex. Can do buffer file path or dired. C-u for folder
;; instead of file.
;;   http://ergoemacs.org/emacs/emacs_copy_file_path.html
;; Originally `xah-file-path'. Originally only used `kill-new' - modified to put
;; in clipboard too (`clipboard-kill-region').
(cl-defun buffer:clipboard:path (&key parent relative)
  "Copy the buffer's current path to `kill-ring'.

If in a file buffer, copy the file's path.

If in Dired buffer, copy the file/dir cursor is on, or marked files.

If a buffer is not file and not Dired, copy value of `default-directory' (which
is usually the \"current\" dir when that buffer was created).

Optional keys:
  - `:parent' (PARENT)
    - If non-nil, copy the buffer's parent directory path instead of the
      buffer's own path.
  - `:relative' (RELATIVE)
    - If non-nil, copy the buffer's path relative to the project root instead of
      the absolute path.

Return a path string."
  (let ((path
         (if (string-equal major-mode 'dired-mode)
             (progn
               (let ((result (mapconcat 'identity (dired-get-marked-files) "\n")))
                 (if (equal (length result) 0)
                     (progn default-directory )
                   (progn result))))
           (if (buffer-file-name)
               (buffer-file-name)
             (expand-file-name default-directory)))))
    (kill-new
     (if parent
         (progn
           (message "Directory path copied: 「%s」" (file-name-directory path))
           (file-name-directory path))
       (progn
         (message "File path copied: 「%s」" path)
         path)))))


(defun buffer:cmd:clipboard:path/absolute (&optional parent)
  "Copy the buffer's current path to `kill-ring'.

If in a file buffer, copy the file's path.

If in Dired buffer, copy the file/dir cursor is on, or marked files.

If a buffer is not file and not Dired, copy value of `default-directory' (which
is usually the \"current\" dir when that buffer was created).

If PARENT is non-nil (`universal-argument' is called first), copy the buffer's
parent directory path instead of the buffer's own path.

Return a path string."
  (interactive "P")
  (buffer:clipboard:path :parent parent))


(defun buffer:cmd:clipboard:path/relative (&optional parent)
  "Copy the buffer's current path to `kill-ring'.

If in a file buffer, copy the file's path.

If in Dired buffer, copy the file/dir cursor is on, or marked files.

If a buffer is not file and not Dired, copy value of `default-directory' (which
is usually the \"current\" dir when that buffer was created).

If PARENT is non-nil (`universal-argument' is called first), copy the buffer's
parent directory path instead of the buffer's own path.

Return a path string."
  (interactive "P")
  (buffer:clipboard:path :parent parent
                         :relative t))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :buffer 'name)

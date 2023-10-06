;;; core/modules/emacs/buffer/yank.el --- Cut, Copy, Paste, Yank, Kill -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2023-10-06
;; Timestamp:  2023-10-06
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Cut, Copy, Paste, Yank, Kill
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Yank
;;------------------------------------------------------------------------------

(defun yank:replace (start end)
  "Replace any selected region with yanked text.

START should be the start of the region (int or point) or nil.
END should be the end of the region (int or point) or nil."
  (interactive "*r")

  ;; Using meow and have a region selected?
  (if (and (bound-and-true-p meow-mode)
           (region-active-p))
      ;; Use meow's replace-with-yanked-text function.
      ;; NOTE: Errors if no region selected, which is why we hide behind
      ;; `region-active-p'.
      (meow-replace)

    ;; No region or no meow.
    (when-let ((s (string-trim-right (current-kill 0 t) "\n")))
      ;; Only delete region if we in fact have one.
      (when (and (integerp start) ; `integerp' is true for points as well as integers
                 (integerp end))
        ;; Be at start of region so that you yank into where region was after you
        ;; delete region if for some reason you aren't at/in your region when this
        ;; is called, which you should be unless you're not calling interactively?
        (goto-char (min start end))

        ;; `delete-region' does not add it to the kill ring.
        ;; `kill-region' does, but then you'd need to (yank 2).
        (delete-region start end))

      ;; ...and yank our (trimmed) text.
      (insert s))))


(defun yank/pop:replace (&optional start end prefix)
  "Replace any selected region with yanked text from kill ring like `yank-pop'.

Otherwise select string from the kill ring and insert it.

START should be the start of the region (int or point) or nil.
END should be the end of the region (int or point) or nil.

Use
See `yank-pop' for the meaning of PREFIX arg.

Uses `consult-yank-from-kill-ring' if `consult' is a feature."
  ;; `interactive' string can't do region _and_ prefix?
  ;; So do it ourselves:
  (interactive (list
                (when (region-active-p) (region-beginning))
                (when (region-active-p) (region-end))
                current-prefix-arg))

  ;; Not using `interactive' codes, so we can't use "*" to check for read-only
  ;; buffer, so check ourselves:
  (barf-if-buffer-read-only)

  ;;------------------------------
  ;; If we just yanked, shortcut into `yank-pop'.
  ;;------------------------------
  (cond ((memq last-command '(yank
                              ; `yank-pop' pretends to be `yank'; so no extra check for it.
                              yank:replace
                              meow-replace
                              meow-yank))
         (yank-pop (or prefix 1)))

        ;;------------------------------
        ;; In general, try to use the fancy `consult' yank function.
        ;;------------------------------
        ((and (featurep 'consult)
              (fboundp 'consult-yank-from-kill-ring))
         ;;---
         ;; First, delete the selected region?
         ;;---
         (when (and (integerp start) ; `integerp' is true for points as well as integers
                    (integerp end))
           ;; Be at start of region so that you yank into where region was after you
           ;; delete region if for some reason you aren't at/in your region when this
           ;; is called, which you should be unless you're not calling interactively?
           (goto-char (min start end))

           ;; `delete-region' does not add it to the kill ring.
           ;; `kill-region' does, but then you'd need to (yank 2).
           (delete-region start end))

         ;;---
         ;; Fancy Yank
         ;;---
         (call-interactively #'consult-yank-from-kill-ring))

        ;;------------------------------
        ;; Fallback: Just try to `yank-pop'?
        ;;------------------------------
        (t
         (yank-pop (or prefix 1)))))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :buffer 'yank)

;;; mode/org/link.el -*- lexical-binding: t; -*-


(imp:require :buffer 'region)


;;------------------------------------------------------------------------------
;; Commands
;;------------------------------------------------------------------------------

;; Easy paste of e.g. URLs.
(defun mode:cmd:org:here/yank (start end)
  "Insert item from kill ring as an org-mode link with description 'here'."
  (interactive "r")
  ;; (let ((text "here")
  ;;       (link (current-kill 0))) ; Like `yank' but returns a string instead of inserting.
  ;;   (if (buffer:region:active?)
  ;;       (progn
  ;;         (message "Not active")
  ;;         (setq text (or (buffer:region:get :properties? nil)
  ;;                        text))
  ;;         (replace-region-contents (buffer:region:min start end)
  ;;                                  (buffer:region:max start end)
  ;;                                  (lambda () (org-link-make-string link text))))
  ;;     (insert (org-link-make-string link text))))

  ;; TODO: Does this work well all the time? If so, delete above.
  (org-insert-link nil
                   (current-kill 0) ; Like `yank' but returns a string instead of inserting.
                   (or (buffer:region:get :properties? nil)
                       "here")))


(defun mode:cmd:org:here/link (start end)
  "Insert stored org link as an org-mode link with description 'here'."
  (interactive "r")
  (org-insert-link nil
                   (car (car org-stored-links))
                   (or (buffer:region:get :properties? nil)
                       "here")))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mode 'org 'link)

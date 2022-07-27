;;; autogit-commands.el --- Autogit Commands -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2020-08-28
;; Modified:   2022-07-27
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Commands for:
;;   1) Auto-committing changes to certain git repos.
;;      - E.g. notes repositories.
;;   2) Getting general status for certain other git repos.
;;      - E.g. get status for your notes repo(s) and your code repo(s) in one go.
;;
;;; Code:


(require 'subr-x)
;; TODO: Get deferred working for all commands.
;;   - Currently one of them, at least, isn't async...
(require 'deferred)
(require 'magit)

(require 'autogit-variables)
(require 'autogit-buffer)
(require 'autogit-output)
(require 'autogit-magit)


;;------------------------------------------------------------------------------
;; Buffer Functions
;;------------------------------------------------------------------------------

;;;###autoload
(defun autogit:buffers:bury ()
  "Hide and bury the autogit output buffers.

Will hide/bury the \"*Messages*\" buffer if any of the output buffer name
settings are `:messages'."
  (interactive)
  (dolist (name (list autogit:buffer:name/push
                      autogit:buffer:name/status))
    (when-let* ((name (if (and (keywordp name)
                               (eq name :messages))
                          "*Messages*"
                        name))
                (window (get-buffer-window name)))
      (with-selected-window window
        (bury-buffer)))))
;; (autogit:buffers:bury)


;;;###autoload
(defun autogit:buffers:kill ()
  "Kill the autogit output buffer(s).

If NAME is `:messages', kills the \"*Messages*\" buffer."
  (interactive)
  (dolist (name (list autogit:buffer:name/push
                      autogit:buffer:name/status))
    ;; Prevent "No buffer named <...>" messages.
    (when (get-buffer name)
      (kill-buffer name))))
;; (autogit:buffers:kill)


;;------------------------------------------------------------------------------
;; Commit & Push
;;------------------------------------------------------------------------------

;;;###autoload
(defun autogit:push (&optional dry-run)
  "Commit and push changes.

For each item in `autogit:repos:path/commit', use Magit to:
  1. add files
  2. commit
  3. and then push

If DRY-RUN is non-nil, does not execute the (Ma)git commands."
  (interactive "P")

  ;; Either have to require magit here, or set magit to ":demand t" in
  ;; use-package. Trying out requiring here as magit isn't the fastest to start.
  ;; TODO: Or we could figure out auto-loading and prereqs and shenanigans...
  (require 'magit)

  (if (null autogit:repos:path/commit)
      (message "Autogit Error: No paths in `autogit:repos:path/commit': %S"
               autogit:repos:path/commit)

    (let ((buffer autogit:buffer:name/push)
          (step 1)
          results
          (indent/commands '(6 8)))
      (deferred:$
        (deferred:next
          (lambda ()
            (int<autogit>:display:break buffer)))
        (deferred:nextc it
          (lambda (_)
            (int<autogit>:display:message buffer
                                          (list :prop :face:self
                                                :text autogit:text:name)
                                          " "
                                          (format-time-string autogit:datetime:format)
                                          ": "
                                          (list :prop :face:title
                                                :text "Commit & Push")
                                          " on "
                                          (list :prop :face:highlight
                                                :text (list "%d" (length autogit:repos:path/commit)))
                                          " locations...\n")))
        (deferred:nextc it
          (lambda (_)
            (int<autogit>:buffer:display buffer)))

        ;; Walk our list of auto-commit loctaions.
        (deferred:loop autogit:repos:path/commit
          (lambda (path)
            "Function to do the actual status, commit, etc for each repo."
            (int<autogit>:display:newline buffer)
            (int<autogit>:display:message buffer
                                          (list :prop :face:self
                                                :text autogit:text:name)
                                          "\n  repository: "
                                          (list :prop :face:path :text (file-name-nondirectory
                                                                        (directory-file-name path)))
                                          "\n  path:       "
                                          (list :prop :face:path :text path))

            ;; Change the default-directory just for this scope...
            (let* ((default-directory (if (file-directory-p path)
                                          ;; Already a 'directory' path; is ok.
                                          path
                                        ;; Needs a slash?
                                        (file-name-as-directory path)))
                   ;; Some silly little message.
                   (commit-message (format "autogit commit\n\n%s: %s triggered in %s by %s"
                                           (format-time-string autogit:datetime:format)
                                           "Auto-commit"
                                           "emacs/magit"
                                           "autogit:magit/auto-commit function."))
                   ;; Full changes.
                   (alist/changes (int<autogit>:changes:in-subdir default-directory))
                   ;; Filtered changes or "do-not-commit" reason keyword.
                   (changes/abs (int<autogit>:changes:commit-filter alist/changes)))

              ;; Pull & check first.
              (int<autogit>:display:newline buffer)
              (int<autogit>:display:message buffer
                                            (list :prop :face:self
                                                  :text autogit:text:name)
                                            ": Pull from upstream...")
              (int<autogit>:magit:fetch dry-run buffer indent/commands)

              (int<autogit>:display:newline buffer)
              (int<autogit>:display:message buffer
                                            (list :prop :face:self
                                                  :text autogit:text:name)
                                            ": Check for changes to commit...")
              ;; Not allowed to commit?
              (cond ((and (keywordp changes/abs)
                          (eq changes/abs :unmerged))
                     ;; Save that nothing happened.
                     (push (cons path "Blocked by unmerged files.") results)
                     ;; And say why.
                     (int<autogit>:display:message buffer
                                                   (list :prop :face:failure
                                                         :text "  Unmerged changes - cannot auto-commit!"))
                     ;; TODO: Display unmerged paths in alist/changes.
                     )

                    ;; Not necessary to commit?
                    ((and (keywordp changes/abs)
                          (eq changes/abs :no-op))
                     ;; Save that nothing happened.
                     (push (cons path "None.") results)
                     ;; Say why nothing happened.)
                     (int<autogit>:display:message buffer
                                                   "  No changes to auto-commit: "
                                                   (list :prop :face:path
                                                         :text default-directory)))

                    ;; Ok. Commit.
                    (t
                     (let* ((changes/rel
                             ;; Gather up all changed files, strip out dir prefix.
                             (mapcar (lambda (x)
                                       (string-remove-prefix default-directory x))
                                     changes/abs))
                            (prefix "\n    + ")
                            (changed-str (concat
                                          prefix
                                          (string-join changes/rel prefix))))
                       ;; Add!
                       (int<autogit>:display:message buffer
                                                     (list :prop :face:highlight
                                                           :text (list "  %d. " step))
                                                     "Adding changes found:"
                                                     (list :prop :face:path
                                                           :text changed-str))
                       (setq step (1+ step))

                       ;; "add <path>" or "add -A ." work to add untracked.
                       ;; "add -A ." == "add ." + "add -u ."
                       ;; "add ." only adds modified.
                       (int<autogit>:magit:git dry-run
                                               buffer
                                               indent/commands
                                               :args-as-msg
                                               "add" "-A" ".")
                       (setq step (1+ step))


                       ;; Commit!
                       (int<autogit>:display:newline buffer)
                       ;; TODO: Get list of staged for message? Currently trusting they are
                       ;; the same as the `changes/abs' - and they /should/ be.
                       (int<autogit>:display:message buffer
                                                     (list :prop :face:highlight
                                                           :text (list "  %d. " step))
                                                     "Committing changes:"
                                                     (list :prop :face:path
                                                           :text changed-str))
                       (setq step (1+ step))

                       ;; Don't 'commit all' ("commit -a"), so we can commit just whatever
                       ;; sub-folder we are in.
                       (int<autogit>:magit:git dry-run
                                               buffer
                                               indent/commands
                                               :args-as-msg
                                               "commit" "-m" commit-message)
                       (setq step (1+ step))

                       ;; Push?
                       (int<autogit>:display:newline buffer)
                       (int<autogit>:display:message buffer
                                                     (list :prop :face:highlight
                                                           :text (list "  %d. " step))
                                                     "Pushing changes:"
                                                     (list :prop :face:path
                                                           :text changed-str))
                       (int<autogit>:magit:git dry-run
                                               buffer
                                               indent/commands
                                               :args-as-msg
                                               "push")
                       (setq step (1+ step))

                       ;; Done. Until I find all the edge cases I guess.
                       ;; Like when push fails?
                       (int<autogit>:display:newline buffer)
                       (int<autogit>:display:message buffer
                                                     (list :prop :face:self
                                                           :text autogit:text:name)
                                                     ": "
                                                     "Committed and pushed (probably?): "
                                                     (list :prop :face:path
                                                           :text path)
                                                     (list :prop :face:path
                                                           :text changed-str))
                       (push (cons path (or changed-str "None.")) results)))))

            ;; Finished pushing commits on autogit locations. Give a rundown.
            (int<autogit>:display:newline buffer)
            (int<autogit>:display:message buffer
                                          (list :prop :face:self
                                                :text autogit:text:name)
                                          ": "
                                          (list :prop :face:success :text "Done")
                                          "; commit ran on "
                                          (list :prop :face:highlight :text (list "%d" (length autogit:repos:path/commit)))
                                          " locations:")
            (let ((first-result t))
              (dolist (result results)
                (if first-result
                    (setq first-result nil)
                  (int<autogit>:display:newline buffer))
                (int<autogit>:display:message buffer
                                              "  repository: "
                                              (list :prop :face:path :text (file-name-nondirectory
                                                                            (directory-file-name (car result))))
                                              "\n  path:       "
                                              (list :prop :face:path
                                                    :text (car result))
                                              "\n  changes:    "
                                              (list :prop :face:path
                                                    :text (cdr result)))))))

        ;; `deferred:nextc' so that this waits on the loop to finish before printing.
        (deferred:nextc it
          (lambda ()
            (int<autogit>:display:message buffer
                                          "\n"
                                          (list :prop :face:self :text autogit:text:name)
                                          " "
                                          (format-time-string autogit:datetime:format)
                                          ": "
                                          (list :prop :face:title :text "Commit & Push")
                                          " - "
                                          (list :prop :face:success :text "Done."))
            ;; Finally, switch to the buffer if settings dictate.
            (int<autogit>:buffer:switch buffer)))))))
;; (setq autogit:repos:path/commit nil)
;; (push "D:/home/spydez/.lily.d" autogit:repos:path/commit)
;; (autogit:push 'dry-run)


;;------------------------------------------------------------------------------
;; Status
;;------------------------------------------------------------------------------

;;;###autoload
(defun autogit:status ()
  "Display repo statuses.

For each item in `autogit:repos:path/watch', use (Ma)git to look for
uncommitted(/unpushed?) changes."
  (interactive)

  (let ((buffer autogit:buffer:name/status))
    (deferred:$
      ;;------------------------------
      ;; Title and stuff.
      ;;------------------------------
      ;; TODO: instead of section-break/auto, do int<autogit>:output:title function that
      ;; puts "Status of [...]" message inside of the ASCII box.
      (deferred:next
        (lambda ()
          (int<autogit>:display:break buffer)))
      (deferred:next
        (lambda ()
          (int<autogit>:display:message buffer
                                        (list :prop :face:self :text autogit:text:name)
                                        " "
                                        (format-time-string autogit:datetime:format)
                                        ": "
                                        (list :prop :face:title :text "Status")
                                        " of "
                                        (list :prop :face:highlight
                                              :text (list "%d" (length autogit:repos:path/watch)))
                                        " watch locations...")))
      (deferred:next
        (lambda ()
          (int<autogit>:buffer:display buffer)))
      (deferred:wait 1000) ; 1000 ms

      ;;------------------------------
      ;; Get status for each repo.
      ;;------------------------------
      (deferred:loop autogit:repos:path/watch
        (lambda (path)
          "Function to display path and status for each repo."
          ;; Get & display the alist of changes based on settings.
          (int<autogit>:display:message buffer
                                        "\n"
                                        (list :prop :face:self :text autogit:text:name)
                                        ": "
                                        ;; TODO: remove "Checking " if not much time between this output and status output.
                                        "Checking "
                                        (list :prop :face:path :text path)
                                        "...")
          (int<autogit>:display:status buffer (int<autogit>:changes:in-subdir path))))
      (deferred:error it
        (lambda (err)
          (message "`autogit:status' errored while looping on status repos: %S" err)))

      ;; TODO: if full status, maybe repeat the short status strings here?
      ;; [AUTOGIT]: Status Summary:
      ;;   <some/path-0>:    •00 +00 ¬00 ⊥00
      ;;   <another/path-1>: •00 +08 ¬25 ⊥00
      ;;   <etc.>:           [etc.]

      ;; `deferred:nextc' so that this waits on the loop to finish before printing.
      (deferred:nextc it
        (lambda ()
          (int<autogit>:display:message buffer
                                        "\n"
                                        (list :prop :face:self :text autogit:text:name)
                                        " "
                                        (format-time-string autogit:datetime:format)
                                        ": "
                                        (list :prop :face:title :text "Status")
                                        " - "
                                        (list :prop :face:success :text "Done."))
          ;; Finally, switch to the buffer if settings dictate.
          (int<autogit>:buffer:switch buffer))))))
;; (autogit:status)


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'autogit-commands)
;;; autogit-commands.el ends here

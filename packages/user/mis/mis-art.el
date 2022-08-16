;;; mis-art.el --- Lines and Pretty Things -*- lexical-binding: t; -*-
;;
;; Author: Cole Brown <code@brown.dev>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:  2019-10-23
;; Modified: 2022-08-11
;;
;;; Commentary:
;;
;;  Lines and Pretty Things
;;
;;; Code:

(require 'cl-lib)

(require 'mis-error)
(require 'mis-valid)


;;------------------------------------------------------------------------------
;; Lines
;;------------------------------------------------------------------------------

(defun mis:line (&rest args)
  "Create a line by repeating a character/string.

ARGS should be a plist of:
  - Any valid `mis:style' args (e.g. `:width' for a max line width).
  - `:string' - A string or character to repeat.
    - Or if no `:string', keyword, just the string/character value.

Examples:
  (mis:line :width 10 :string \"xX\")
  (mis:line \"-\")"
  (let* ((parsed (apply 'int<mis>:parse
                        'mis:line
                        '(:line :style)
                        args))
         (line (plist-get parsed :line))
         (string (plist-get line :string))
         (message (plist-get parsed :message))
         key
         value
         ast)

    ;;------------------------------
    ;; String Error Checking / Normalization
    ;;------------------------------
    ;; Have an explicit `:string' in `:line'?
    (cond ((not (null string))
           ;; `:message' not allowed in this case.
           (when message
             (int<mis>:error 'mis:line
                             "Invalid ARGS; cannot have both `:string' and `:message'."
                             "`:string': %S, `:message': %S, ARGS: %S"
                             string
                             message
                             args)))

          ;; No `:string' but do have `:message'? Convert `:message' to `:string'.
          ((and (null string)
                (not (null message)))
           ;; Only allow a message that is a char or string without extra args.
           (cond ((> (length message) 1)
                  (int<mis>:error 'mis:line
                                  '("Invalid ARGS; line string can only be a "
                                    "simple string, not a format string w/ args. "
                                    "Got: %S, ARGS: %S")
                                  message
                                  args))
                 ((and (not (stringp (car message)))
                       (not (characterp (car message))))
                  (int<mis>:error 'mis:line
                                  '("Invalid ARGS; line string can only be a "
                                    "string or character. "
                                    "Got %S %S, ARGS: %S")
                                  (type-of (car message))
                                  (car message)
                                  args))
                 (t
                  (setq string (car message)))))

          ;; No `:string', no `:message'...?
          (t
           (int<mis>:error 'mis:line
                           '("Invalid ARGS; line must have a string/character to repeat. "
                             "Found nothing in ARGS: %S -> %S")
                           args
                           parsed)))

    ;;------------------------------
    ;; Build & return normalized list.
    ;;------------------------------
    ;; Convert `:line' from parsing into `:format'.
    (setq ast (list :format (list :formatter 'repeat
                                  :string    string)))

    ;; Delete `:message' but leave `:style' and anything else as-is.
    (while parsed
      (setq key (pop parsed)
            value (pop parsed))

      (cond ((not (keywordp key))
             ;; Dunno; error?
             (int<mis>:error 'mis:line
                      "Unknown plist key: %S, value: %S"
                      key
                      value))

            ((memq key '(:line :message))
             ;; Already dealt with this parsed plist entry.
             nil)

            ;; Copy into output tree.
            (t
             (push value ast)
             (push key ast))))

    ast))
;; (mis:line :width 10 :string "hi")
;; (mis:line :string "hi")
;; (mis:line "-")


;; TODO: this?    (defun mis:box (&key width corner:top/left corner:top/right corner:bottom/left corner:bottom/right horizontal vertical)
;; TODO: or this? (defun mis:box (&key width corners horizontal vertical)
;; (mis:box :width 80
;;          :corner:top/left "┌"
;;          :corner:top/right "┐"
;;          :corner:bottom/left "└"
;;          :corner:bottom/right "┘"
;;          :horizontal "─"
;;          :vertical "│")
;; (mis:box :width 80
;;          :corners '("┌" "┐" "└" "┘")
;;          :horizontal "─"
;;          :vertical "│")
;; TODO: Actually I think this:
;; (mis:box :width 80 :border 'hypen) ;; or `ascii'?
;; (mis:box :width 80 :border 'line)
;; (mis:box :width 80 :border 'double)


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'mis-art)
;;; mis-art.el ends here

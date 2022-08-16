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
  - `:string' - A string or character to repeat."
  (let* ((parsed (apply 'int<mis>:parse
                        'mis:line
                        '(:line :style)
                        args))
         (line (plist-get parsed :line))
         (string (plist-get line :string))
         key
         value
         ast)

    ;; Must have a string to create the line.
    (unless string
      (int<mis>:error 'mis:line
                      "Invalid ARGS; no `:string' found! %S -> %S"
                      args
                      parsed))

    ;;------------------------------
    ;; Build & return normalized list.
    ;;------------------------------
    ;; Convert `:line' from parsing into `:format'.
    (setq ast (list :format (list :formatter 'repeat
                                  :string    string)))

    ;; Leave `:style' and anything else as-is.
    (while parsed
      (setq key (pop parsed)
            value (pop parsed))

      (cond ((not (keywordp key))
             ;; Dunno; error?
             (int<mis>:error 'mis:line
                      "Unknown plist key: %S, value: %S"
                      key
                      value))

            ((eq key :line)
             ;; Already dealt with this parsed plist entry.
             nil)

            ;; Copy into output tree.
            (t
             (push value ast)
             (push key ast))))

    ast))
;; (mis:line :width 10 :string "hi")


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

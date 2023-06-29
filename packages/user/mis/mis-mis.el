;;; mis-mis.el --- Mis Top-Level API -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <code@brown.dev>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2022-09-16
;; Timestamp:  2023-06-29
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;  Mis Top-Level API
;;
;;; Code:


(require 'mis-error)
(require 'mis-valid)
(require 'mis-tree-syntax)
(require 'mis-tree-output)
(require 'mis-parse)
(require 'mis-buffer)
(require 'mis-string)
(require 'mis-compile)
(require 'mis-style)


;;------------------------------------------------------------------------------
;; Output API
;;------------------------------------------------------------------------------

(defmacro mis (&rest args)
  "Output a message built from ARGS.

Top-Level Keywords (Optional):
  - `:output' - `:buffer', `buffer'  (default)
                - Insert Mis message into a buffer.
              - `:string', `string'
                - Return Mis message as a string.

  - `:buffer' - `:mis', `mis'  (default)
                - Insert into `mis:buffer:name' buffer.
              - `:messages', `messages', `:message', `message'
                - Insert into '*Messages*' buffer.
              - `:current', `current'
                - Insert into `current-buffer'.
              - A string.
                - Insert into the buffer name in the string.

Rest of the args: Probably Mis function calls.
Example:
  (mis
   :buffer 'current
   (mis:comment :width 80
                :padding \" \"
                :newlines t
                (mis:line \"-\")
                (mis:style :align 'center \"Hello there.\")
                (mis:line \"-\")))

  - TODO: Lots of documentation in here?"
  (let ((macro:keywords/expected '(:buffer :output))
        macro:parse/key
        macro:parse/key/output
        macro:parse/key/buffer
        macro:args/rest)

    ;;------------------------------
    ;; Parse for Top-Level Keywords
    ;;------------------------------
    (dolist (arg args)
      (if (memq arg macro:keywords/expected)
          (setq macro:parse/key arg) ; Save keyword.
        (if macro:parse/key
            (let ((macro:parse/value arg))
              ;; Allow both quoted and unquoted symbols for top-level args.
              ;; For example, either of these:
              ;;   (mis :output string ...)
              ;;   (mis :output 'string ...)
              (while (memq (car-safe macro:parse/value) '(quote function))
                (setq macro:parse/value (cadr macro:parse/value)))

              ;; Validate keyword's value & save into the correct variable.
              (cond ((eq macro:parse/key :buffer)
                     (setq macro:parse/key/buffer (int<mis>:buffer:name 'mis macro:parse/value)))

                    ((eq macro:parse/key :output)
                     (setq macro:parse/key/output (int<mis>:valid:member/normalize? 'mis
                                                                                    'macro:parse/value
                                                                                    macro:parse/value
                                                                                    int<mis>:valid:output/types)))

                    (t
                     (int<mis>:error 'mis
                                     "Unhandled top-level key/value: %S = %S"
                                     macro:parse/key
                                     macro:parse/value)))
              (setq macro:parse/key nil))
          ;; Else it's something else and we'll deal with it after sorting the args.
          (push arg macro:args/rest))))

    (setq macro:args/rest (nreverse macro:args/rest))

    ;;------------------------------
    ;; Finalize Params
    ;;------------------------------

    `(let ((macro:output ',macro:parse/key/output)
           (macro:buffer ,macro:parse/key/buffer)
           macro:metatree)

       (int<mis>:debug 'mis "output (parsed):   %S" macro:output)
       (unless macro:output
         (setq macro:output 'buffer))
       (int<mis>:debug 'mis "output (final):    %S" macro:output)

       ;; NOTE: Allow a `:buffer' param even when we're not outputting to a buffer.
       ;; That buffer will be where we parse & compile so that we are in the
       ;; correct context for building the output (e.g. `mis:comment' needs to know
       ;; about comment strings set by the buffer's major mode).

       (int<mis>:debug 'mis "buffer (parsed):   %S" macro:buffer)
       (setq macro:metatree (int<mis>:buffer:metadata 'mis macro:buffer nil)
             macro:buffer   (int<mis>:output/metadata:find 'mis
                                                           :buffer:object
                                                           macro:metatree))
       (int<mis>:debug 'mis "buffer (final):    %S" macro:buffer)

       (int<mis>:debug 'mis "metadata (buffer): %S" macro:metatree)
       ;; BUG HUNT! Good up til here...
       (setq macro:metatree (int<mis>:output:update/metadata 'mis
                                                             macro:metatree
                                                             (cons :output macro:output)))
       (int<mis>:debug 'mis "metadata (all):    %S" macro:metatree)
       ;; BUG HUNT! Bad now.

       ;;------------------------------
       ;; Parse, Compile, & Output
       ;;------------------------------

       ;; So far, none of the `mis:___' parser/compiler calls has actually been
       ;; evaluated and we got our top-level args like `buffer'. This is necessary
       ;; as we need to be in the buffer's context before evaluating anything so
       ;; that widths and styles and such are determined correctly.
       (with-current-buffer macro:buffer
         (save-mark-and-excursion
           ;; In our buffer now... So evalute and resolve things.
           (int<mis>:mis 'mis
                         macro:metatree
                         ;; Parsing happens when these are evaluated, before
                         ;; `int<mis>:mis' is called.
                         ,@macro:args/rest))))))
;; (mis
;;  (mis:style :width 80)
;;  (mis:line "-"))
;; (mis
;;  :buffer "*scratch*"
;;  (mis:style :width 80)
;;  (mis:line "-"))
;; (mis
;;  :output 'string
;;  (mis:style :width 80)
;;  (mis:line "-"))
;; (mis :buffer 'messages (mis:line "-"))
;; (mis :output 'string
;;      :buffer 'messages
;;      (mis:line "-"))


(defun int<mis>:mis (caller metatree &rest args)
  "Output a message built from ARGS to BUFFER.

Caller is expected to have switched to the context of BUFFER already.

ARGS should be Mis Output Trees. That is, it should have been Mis API calls
\(e.g. `mis:string') that have been evaluated already.

METATREE should be nil or a Mis Output Tree of top-level metadata like:
  - `:buffer:name'
  - `:buffer:object'
  - `:output'

BUFFER should be the buffer object.

CALLER should be calling function's name. It can be one of:
  - a string
  - a quoted symbol
  - a function-quoted symbol
  - a list of the above, most recent first
    - e.g. '(#'error-caller \"parent\" 'grandparent)"
  (let ((caller (list 'int<mis>:mis caller))
        styling
        content
        output)

    ;;------------------------------
    ;; Parsing & Validation
    ;;------------------------------
    ;; Check for validity, duplicate styles. Also look for any options that
    ;; should have already been skimmed off into METATREE.
    (while args
      (let ((arg (pop args)))
        ;; Is this non-nil and /only/ styling info? Then it's global styling for
        ;; use in all of the things here.
        (cond ((and arg
                    (int<mis>:valid:style/exclusively? caller arg))
               ;; Make sure it's valid...ish.
               (int<mis>:valid:syntax? caller 'arg arg)

               (if styling
                   ;; There can be only one.
                   ;;
                   ;; NOTE: We could allow more than one and merge, but then
                   ;; we'd have to do collision detection...
                   (int<mis>:error caller
                                   '("Only one Mis `:style' allowed per level in ARGS. "
                                     "have: %S, found: %S, args: %S")
                                   styling
                                   value
                                   args)
                 ;; Save the styling.
                 (setq styling arg)))

              ((listp arg)
               ;; Think this is a Mis syntax tree? Should be anyways; error if not.
               (int<mis>:valid:syntax? caller 'arg arg)
               ;; Save off this mis content type (messages, comments, arts...).
               (push arg content))

              ;; Top-level arg? Example: `:buffer'
              ;; Should have already been delt with and put into METATREE, so...
              ;; Error!
              ((keywordp arg)
               (let ((key arg)
                     (value (pop arg)))
                 (int<mis>:error caller
                                 '("Top-Level keywords should have already been parsed! "
                                   "found: %S = %S")
                                 key
                                 value)))

              ;; Fallthrough: Error!
              (t
               (int<mis>:error caller
                               '("Unknown/unhandled argument!"
                                 "unknown: %S, args: %S")
                               arg
                               args)))))

    ;;------------------------------
    ;; Compile Mis syntax into strings.
    ;;------------------------------
    ;; Compile each parsed Mis Syntax Tree in `content' in current (reverse)
    ;; order & push to compiled Mis Output Trees to `output' list. Final step
    ;; will then have `output' in forwards order.
    (while content
      (int<mis>:debug caller
                      "compile: %S"
                      (nth 0 content))
      ;; Get a syntax tree; turn into an output tree.
      (push (int<mis>:compile caller
                              (pop content)
                              styling)
            output))

    (int<mis>:debug caller
                    "compiled output: %S"
                    output)

    ;;------------------------------
    ;; Output
    ;;------------------------------
    ;; Now we should only have outputs left; print 'em.
    (int<mis>:print caller
                    ;; Our top-level metadata like `:buffer'.
                    metatree
                    ;; Our list of Mis Output Trees.
                    (nreverse output))))
;; (mis
;;  (mis:style :width 80)
;;  (mis:line "-"))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(provide 'mis-mis)
;;; mis-mis.el ends here

;;; output/nub/internal.el -*- lexical-binding: t; -*-

;;                                 ──────────                                 ;;
;; ╔════════════════════════════════════════════════════════════════════════╗ ;;
;; ║               What does the error lib use for erroring?                ║ ;;
;; ╚════════════════════════════════════════════════════════════════════════╝ ;;
;;                                   ──────                                   ;;
;;            At least it's only turtles a bit of the way down...             ;;
;;                                 ──────────                                 ;;


;;------------------------------------------------------------------------------
;; Formatting
;;------------------------------------------------------------------------------

(defun int<nub>:format:message (&rest msg)
  "Format MSG into a message formatting string.

MSG should be string(s), and maybe some keyword(s):

The acceptable keywords are:
  - `:line:each', `:eachline'
    + Concatenate all strings with newlines separator.
  - `:line:new', `:newline'
    + Replace this keyword with a newline character.

Replacements happen before concatenations."
  (let ((func/name "int<nub>:format:message")
        (keywords/valid/car '(:line:each :eachline
                              :line:new  :newline))
        (keywords/valid/cdr '(:line:new  :newline))
        msg/replaced)

    ;; Null MSG is valid; it formats to an empty string.
    (if (or (null msg)
            (and (null (car msg))
                 (= 1 (length msg))))
        (setq msg nil) ;; nil or '(nil) are both nil as far as we care.

      ;;------------------------------
      ;; Validation.
      ;;------------------------------

      ;; Validate first element of MSG.
      (cond ((keywordp (car msg))
             (unless (memq (car msg) keywords/valid/car)
               (error "%s: MSG starts with invalid keyword `%S'! Valid keywords are: %S"
                      func/name
                      (car msg)
                      keywords/valid/car)))

            ((stringp (car msg)))

            (t
             (error "%s: MSG should start with a string or keyword. Got a `%S': %S"
                    func/name
                    (type-of (car msg))
                    (car msg))))

      ;; Validate rest of MSG.
      (unless (seq-reduce (lambda (reduction element)
                            "Check for valid elements."
                            (and reduction
                                 (or (stringp element)
                                     (and (keywordp element)
                                          (memq element keywords/valid/cdr)))))
                          (cdr msg)
                          t)
        (error "%s: Invalid MSG element; MSG should contain strings or valid keywords `%S'. Got: %S"
               func/name
               keywords/valid/cdr
               (cdr msg)))

      ;;------------------------------
      ;; Replacements/substitutions.
      ;;------------------------------
      (dolist (element msg)
        ;; Replace `:line:new' with its string equivalent.
        (cond ((and (keywordp element)
                    (memq element '(:line:new :newline)))
               (push "\n" msg/replaced))

              ;; Keep everything else as-is.
              (t
               (push element msg/replaced))))

      ;; Get `msg/replaced' set up for formatting.
      (setq msg/replaced (nreverse msg/replaced)))

    ;;------------------------------
    ;; Format message parts into final message string.
    ;;------------------------------
    ;;  Null is valid; it formats to an empty string.
    (cond ((null msg/replaced)
           "")

          ;; 1) `:line:each' or `:eachline' first, then strings? Separate lines.
          ((memq (car msg/replaced) '(:line:each :eachline))
           ;; Concat strings w/ newline separator.
           (mapconcat #'identity
                      (cdr msg/replaced)
                      "\n"))

          ;; 2) String concat (no separator).
          (t
           ;; Glue all the strings together.
           (apply #'concat msg/replaced)))))
;; (int<nub>:format:message nil)
;; (int<nub>:format:message "hello there")
;; (int<nub>:format:message "hello, " "there")
;; (int<nub>:format:message :line:each "Hi." "  -> Line 2")
;; (int<nub>:format:message :line:new "All" "Line 2")
;; (int<nub>:format:message :line:each "Hi." :line:new "  -> Line 4")


;;------------------------------------------------------------------------------
;; Errors
;;------------------------------------------------------------------------------
;; nub can't use `nub:output', unless you're after 'output.el' in the load
;; ordering, like 'debug.el'...
;;
;; So here's an error function for our error library.

(defun int<nub>:error (caller msg &rest args)
  "Format an error message for CALLER and signal an error.

CALLER should be a string.

MSG should be:
  - a string
  - a list of strings to `concat'
  - a cons of: '(:newlines . (string-00 ...))
    + This will concat the list-of-strings with a newline between each string.

ARGS will be passed to `format' with the finalized message string."
  ;; Raise an error after figuring out MSG's formatting.
  (apply #'error (format "%s: %s"
                         caller
                         ;; Formatted message based on what we got passed in.
                         ;; Use `flatten-list' to make MSG a list if it's just a string.
                         ;; It also converts cons to lists, but that's ok here.
                         (apply #'int<nub>:format:message (flatten-list msg)))
         ;; Just pass ARGS directly to error - it will do final format.
         args))
;; (int<nub>:error "test-function-name" "hello there")
;; (int<nub>:error "test-function-name" '("hello, " "there"))
;; (int<nub>:error "test-function-name" '(:newlines . ("Hi." "  -> Line 2")))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :nub 'internal)

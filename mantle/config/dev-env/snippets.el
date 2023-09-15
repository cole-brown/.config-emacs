;;; mantle/config/dev-env/snippets.el --- Snippets, Templates, and $1 -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2022-08-05
;; Timestamp:  2023-09-15
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;  Snippets, Templates, and $1
;;
;;; Code:


(imp:require :nub)
(imp:require :elisp 'utils 'functions)
(imp:require :buffer 'region)


;;------------------------------------------------------------------------------
;; Snippet Minor Modes
;;------------------------------------------------------------------------------

(defmacro mantle:user:yasnippet:minor-mode/add (mode &optional hook-var)
  "Register minor MODE with yasnippet.

Register minor mode that you want to have their own snippets category (if a
folder exists for them in any of the snippet directories).

MODE should be a mode symbol.

HOOK-VAR should be the mode's hook variable. If nil, '<mode>-hook' will be used.

Originally stolen from Doom's `set-yas-minor-mode!' in
\"modules/editor/snippets/autoload/settings.el\"."
  (let ((macro<yasnippet>:func/name "mantle:user:yasnippet:minor-mode/add")
        (macro<yasnippet>:path/this (path:current:file)))

    ;;------------------------------
    ;; Error Checks
    ;;------------------------------
    ;; MODE must be a symbol (that is not `nil').
    (unless (and (not (null mode))
                 (symbolp mode))
      (nub:error
         :innit
         macro<yasnippet>:func/name
       "MODE must be a symbol; got %S: %S"
       (type-of mode)
       mode))

    ;; HOOK-VAR can be nil, else must be a symbol.
    (cond ((null hook-var)
           (setq hook-var (intern (format "%s-hook" mode))))

           ((symbolp hook-var)
            nil)

           (t
            (nub:error
               :innit
               macro<yasnippet>:func/name
             "MODE must be a symbol; got %S: %S"
             (type-of mode)
             mode)))

    ;;------------------------------
    ;; Add MODE to `yasnippet'.
    ;;------------------------------

    ;; Debugging and not using `yasnippet'? Complain.
    (cond ;; ((and (innit:debug?)
          ;;       (not (featurep 'yasnippet)))
          ;;  (nub:warning
          ;;     :innit
          ;;     macro<yasnippet>:func/name
          ;;   "`yasnippet' is not loaded; cannot add mode `%S'!"
          ;;   mode))

          ;; Just ignore if we aren't debugging and aren't using `yasnippet'.
          ((not (featurep 'yasnippet))
           nil)

          ;; Add the mode!
          (t
           ;; Auto-create HOOK-VAR symbol if not provided.
           (let ((macro<yasnippet>:hook/name (format "yasnippet:minor-mode/add:%S" mode))
                 (macro<yasnippet>:docstr    (format "Add minor mode `%S' to `yasnippet'." mode)))
             `(innit:hook:defun-and-add
                  ,hook-var
                  (:name    ,macro<yasnippet>:hook/name
                   :file    ,macro<yasnippet>:path/this
                   :docstr  ,macro<yasnippet>:docstr)
                ;; ...All that for this:
                (yas-activate-extra-mode ,mode)))))))
;; (mantle:user:yasnippet:minor-mode/add test-mode)


;;------------------------------------------------------------------------------
;; YASnippets
;;------------------------------------------------------------------------------

;; https://github.com/joaotavora/yasnippet
;; Tutorial/Doc: https://joaotavora.github.io/yasnippet/snippet-development.html
;; Reference: https://joaotavora.github.io/yasnippet/snippet-reference.html
(imp:use-package yasnippet
  :demand t

  ;;------------------------------
  :init
  ;;------------------------------

  ;;------------------------------
  ;; Snippet Helpers
  ;;------------------------------

  (defun int<mantle>:yas:prompt/format (prompt)
    "Format PROMPT into a prompt string.

Add \":\" and \" \" if not present at the end of PROMPT.

Return \"Choose: \" when PROMPT is not a string."
    (concat
     ;; Prompt itself:
     (if (not (stringp prompt))
         "Choose"
       (string-trim prompt))
     ;; Suffixes:
     (if (string-suffix-p ": " prompt)
         ""
       ":")
     " "))
  ;; (int<mantle>:yas:prompt/format nil)
  ;; (int<mantle>:yas:prompt/format "Language")
  ;; (int<mantle>:yas:prompt/format "Language ")
  ;; (int<mantle>:yas:prompt/format "Language")


  (defvar int<mantle>:yas:choose/no-match/history nil
    "History of recently chosen choices in `int<mantle>:yas:choose/no-match'.")


  (defvar mantle:yas:choose/no-match/history:src nil
    "History of recently chosen choices for `org-mode' snippet `src'.")


  (defun mantle:yas:choose/no-match/history (history &optional fallback)
    "Most recent choice from HISTORY.

If HISTORY is `:default', use default variable `mantle:yas:choose/no-match/history'

If FALLBACK is non-nil, return FALLBACK instead of nil."
    (or (car (if (eq history :default)
             int<mantle>:yas:choose/no-match/history
             history))
        fallback))
  ;; (mantle:yas:choose/no-match/history :default)
  ;; (mantle:yas:choose/no-match/history mantle:yas:choose/no-match/history:src)
  ;; (mantle:yas:choose/no-match/history mantle:yas:choose/no-match/history:src :dne)


  (defun mantle:yas:choose/no-match (prompt history &rest possibilities)
    "Prompt for a string in POSSIBILITIES and return it.

The last element of POSSIBILITIES may be a list of strings.

Like `yas-choose-value' except does not require a match.

If HISTORY is a symbol name, use that for the prompt's history.
Else use `int<mantle>:yas:choose/no-match/history'."
    (unless (or yas-moving-away-p
                yas-modified-p)
      ;; TODO:yasnippet: are `last-link' and/or `last-elem' important? Are they
      ;; `yasnippet' vars or something? Why is this here? Why is there no
      ;; comment?!
      (let* ((last-link (last possibilities))
             (last-elem (car last-link)))
        (when (listp last-elem)
          (setcar last-link (car last-elem))
          (setcdr last-link (cdr last-elem))))
      (completing-read (int<mantle>:yas:prompt/format prompt)
                       possibilities
                       nil
                       ;; TODO: `confirm' if `confirm-after-completion' is meh.
                       'confirm-after-completion
                       nil
                       (if (symbolp history)
                           history
                         'int<mantle>:yas:choose/no-match/history))))


  (defun mantle:yas:trim/lines (string)
    "Trim newlines and carriage returns from beginning/end of STRING.

Also remove STRING's properties.

If STRING is not a string, return nil."
    (if (stringp string)
        (substring-no-properties
         (string-trim string (rx (one-or-more (any "\n" "\r")))))
      nil))


  (defvar mantle:yas:text/saved nil
    "Save some text for later.
Intended for `yas-selected-text' and `mantle:yas:choose/options'.")


  (defun mantle:yas:text/save (text)
    "Save some TEXT for later.
Intended for `yas-selected-text' and `mantle:yas:choose/options'."
    (setq mantle:yas:text/saved text)
    ;; Don't return text or it'll be in the template.
    nil)


  (defvar-local mantle:yas:interaction/type ; aka `yas--interaction-type' in yas parlance.
    nil
    "How did the user first start interacting with this snippet?

Valid values:
  - nil     - no interaction yet
  - :insert - `yas-insert-snippet' or similar style
  - :expand - `yas-expand-from-trigger-key' or similar style")
(put 'path:uniquify:settings/local 'permanent-local t) ; Always a buffer-local variable.
;; (get 'path:uniquify:settings/local 'permanent-local)


  (defun mantle:yas:choose/options:interaction/insert (options)
    "Force choice to be saved text in `mantle:yas:text/saved'?

If `mantle:yas:interaction/type' is `:insert' and OPTIONS includes `:saved:preferred',
force choice to be text saved to `mantle:yas:text/saved'.
Else return nil."
    (when (and (eq mantle:yas:interaction/type :insert)
               (or (memq :saved:preferred options)
                   (memq 'saved:preferred options)))
      ;; Return whatever's saved; nil is a whatever- who cares? Maybe caller. ¯\_(ツ)_/¯
      mantle:yas:text/saved))
  ;; (let ((mantle:yas:interaction/type :insert)
  ;;       (mantle:yas:text/saved "foo"))
  ;;   (mantle:yas:choose/options:interaction/insert '(:expand :saved)))


  (defun mantle:yas:choose/option:string-or-nil (string)
    "Return STRING if a non-empty string, else nil."
    (when (and (stringp string)
               (not (string-empty-p string)))
      string))


  (defun mantle:yas:choose/options:default (&rest options)
    "From OPTIONS, choose first non-empty (or priority) string and return it.

If you want `yas-selected-text', use `:saved' and at some previous point in the
snippet, do:
  `(mantle:yas:text/save yas-selected-text)`

OPTIONS:
  - `:saved'           - Consider `mantle:yas:text/saved'
  - `:saved:preferred' - Return `mantle:yas:text/saved' if non-nil.
  - `:clipboard'       - Consider clipboard's text.
  - `:yank'            - Consider kill ring's text.
  - `:none'            - Consider empty text.
  - `:selected'        - Consider active region in current buffer."
    ;;------------------------------
    ;; Force a "choice" based on interaction type?
    ;;------------------------------
    (if-let ((forced-choice (mantle:yas:choose/options:interaction/insert options)))
        ;;------------------------------
        ;; Return forced choice.
        ;;------------------------------
        ;; Trim leading/trailing newlines.
        (mantle:yas:trim/lines forced-choice)

      ;;------------------------------
      ;; Choose first actual string from OPTIONS.
      ;;------------------------------
      (let (first-choice
            empty-choice?)
        (while options
          (when-let ((option (car options))
                     (choice
                      ;; TODO: share a helper function with `mantle:yas:choose/options' for this pcase?
                      (pcase (pop options)
                        ((or :saved 'saved)
                         (mantle:yas:choose/option:string-or-nil
                          mantle:yas:text/saved))

                        ((or :selected 'selected)
                         (when (buffer:region:active?)
                           (buffer:region:get)))

                        ((or :clipboard 'clipboard)
                         (mantle:yas:choose/option:string-or-nil
                          ;; `let' values stolen from `clipboard-yank':
                          (let ((select-enable-clipboard t)
                                ;; Ensure that we defeat the DWIM login in `gui-selection-value'.
                                (gui--last-selected-text-clipboard nil))
                            (current-kill 0 'do-not-move))))

                        ((or :yank 'yank)
                         (mantle:yas:choose/option:string-or-nil
                          (current-kill 0 'do-not-move)))

                        ((or :none 'none)
                         (setq empty-choice? t)
                         ;; `none'/empty-string should be a fallback, at best, so don't choose it now.
                         :not-a-string-yet))))
            (when (stringp choice)
              (setq first-choice choice  ; Choose this string...
                    options      nil)))) ; ...and we're done here.
        ;; Return whatever was picked.
        (cond ((stringp first-choice)
               first-choice)
              (empty-choice?
               "")
              (t
               nil)))))
  ;; (mantle:yas:choose/options:default :none :yank)


  (defun mantle:yas:choose/options (prompt &rest options)
    "Give keyword OPTIONS for what to use/choose from.

If you want `yas-selected-text', use `:saved' and at some previous point in the
snippet, do:
  `(mantle:yas:text/save yas-selected-text)`

OPTIONS:
  - `:saved'           - Add `mantle:yas:text/saved' to list presented.
  - `:saved:preferred' - Return `mantle:yas:text/saved' if non-nil.
  - `:clipboard'       - Add clipboard's text to list presented.
  - `:yank'            - Add kill ring's text to list presented.
  - `:none'            - Add empty text to list presented.
  - `:selected'        - Add active region in current buffer to list presented.

Build list of text choices and present to user (unless e.g. `:saved:preferred'
in which case maybe shortcut out). Return what user enters/selects.

NOTE: Text of options will be deduplicated before being presented."
    (unless (or yas-moving-away-p
                yas-modified-p)
      ;;------------------------------
      ;; Force a "choice" based on interaction type?
      ;;------------------------------
      (if-let ((forced-choice (mantle:yas:choose/options:interaction/insert options)))
          ;;------------------------------
          ;; Return forced choice.
          ;;------------------------------
          ;; Trim leading/trailing newlines.
          (mantle:yas:trim/lines forced-choice)

        ;;------------------------------
        ;; Actually let user choose based on available text from OPTIONS.
        ;;------------------------------
        (let (choices)
          (dolist (option (nreverse options))
            ;; Trim leading/trailing newlines.
            (push (mantle:yas:trim/lines ; Returns nil for non-strings, which is used.
                   ;; Get text from wherever it is.
                   ;; TODO: share a helper function with `mantle:yas:choose/options' for this pcase?
                   (pcase option
                     ((or :saved 'saved)
                      (mantle:yas:choose/option:string-or-nil
                       mantle:yas:text/saved))

                     ((or :selected 'selected)
                      (when (buffer:region:active?)
                        (buffer:region:get)))

                     ((or :clipboard 'clipboard)
                      (mantle:yas:choose/option:string-or-nil
                       ;; `let' values stolen from `clipboard-yank':
                       (let ((select-enable-clipboard t)
                             ;; Ensure that we defeat the DWIM login in `gui-selection-value'.
                             (gui--last-selected-text-clipboard nil))
                         (current-kill 0 'do-not-move))))

                     ((or :yank 'yank)
                      (mantle:yas:choose/option:string-or-nil
                       (current-kill 0 'do-not-move)))

                     ((or :none 'none)
                      ;; `none' should be an empty string; don't use `mantle:yas:choose/option:string-or-nil'.
                      "")))
                  choices))

          ;; Remove invalids and deduplicate.
          (setq choices (seq-uniq (seq-filter #'stringp choices) #'string=))

          ;; Prompt user for choice.
          (let* ((last-link (last choices))
                 (last-elem (car last-link)))
            (when (listp last-elem)
              (setcar last-link (car last-elem))
              (setcdr last-link (cdr last-elem))))
          (completing-read (int<mantle>:yas:prompt/format prompt)
                           (nreverse choices))))))
  ;; (mantle:yas:choose/options "prompt" :saved :selected :clipboard :yank :none)
  ;; (let ((mantle:yas:interaction/type :expand)
  ;;       (mantle:yas:text/saved "required choice"))
  ;;   (mantle:yas:choose/options "prompt" :saved:preferred :none))
  ;; (let ((mantle:yas:interaction/type :expand)
  ;;       (mantle:yas:text/saved nil))
  ;;   (mantle:yas:choose/options "prompt" :saved:preferred :none))


  ;;------------------------------
  ;; Default Snippets Location
  ;;------------------------------

  ;; Don't override if it was set in e.g. secrets init or system init or something.
  (when (null (jerky:get 'path 'dev-env 'snippets))
    (jerky:set 'path 'dev-env 'snippets
               :value (path:abs:dir user-emacs-directory "snippets")
               :docstr "Default path to snippets in `user-emacs-directory'."))


  ;;------------------------------
  ;; Advice
  ;;------------------------------

  (defun mantle:yas:indent-according-to-mode/safe ()
    "Call `indent-according-to-mode`, converting errors to messages."
    (condition-case err
        (indent-according-to-mode)
      (error
       (message "%s" (error-message-string err)))))


  (define-advice yas--indent-region (:around (fn &rest args) mantle:yas:indent-according-to-mode/safe)
    "Make `indent-according-to-mode' not raise errors.

If a snippet gets an error during its expansion, `yas' just dies there and your
snippet isn't useful - you don't get to fill in fields. Try to make it so that
errors don't ruin everything."
    (cl-letf (((symbol-function #'indent-according-to-mode) #'mantle:yas:indent-according-to-mode/safe))
      (apply fn args)))


  ;;------------------------------
  ;; Hooks
  ;;------------------------------

  (innit:hook:defun
      (:name    "yasnippet"
       :docstr  "Hook for yasnippet editting."
       :squelch t)
    ;; Clean up interaction/type if needed.
    (setq mantle:yas:interaction/type nil)

    ;; Normally we want a final newline in all files, so `require-final-newline'
    ;; is set to t. However, in yasnippet files, that means all snippets will
    ;; insert a "\n", and that's not desired.
    (setq require-final-newline nil))


  (innit:hook:defun
      (:name    "yasnippet:mode/exit"
       :docstr  "Hook for cleaning up when leaving yasnippet minor mode."
       :squelch t)
    ;; Only run on exit, because that is our name.
    (unless snippet-mode
      ;; Clean up interaction/type.
      (setq mantle:yas:interaction/type nil)))
  ;; (add-hook 'snippet-mode-hook #' mantle:hook:yasnippet:mode/exit)


  ;;------------------------------
  :hook
  ;;------------------------------
  ((snippet-mode-hook . mantle:hook:yasnippet) ; (innit:hook:func/name:symbol "yasnippet" nil)
   (snippet-mode-hook . mantle:hook:yasnippet:mode/exit))


  ;;------------------------------
  :custom
  ;;------------------------------

  ;; Allow snippetception: snippets inside of snippets.
  (yas-triggers-in-field t)

  ;;---
  ;; Indentation
  ;;---
  ;; `fixed': Indent the snippet to the current column;
  ;; `auto': Indent each line of the snippet with indent-according-to-mode
  (yas-indent-line 'auto)

  ;; These require `yas-indent-line' == `auto' to do anything.
  ;; TODO: Do I want either of these in prog modes? Don't /think/ I want in `org-mode'; see `src' indent issues.
  (yas-also-auto-indent-first-line nil)
  (yas-also-indent-empty-lines nil)

  ;; This modifies how yas looks for matching keys to expand into templates.
  ;;   - https://emacs.stackexchange.com/a/35603
  ;;   - Also check out its documentation at: =C-h v yas-key-syntaxes=
  ;; So if I have keys that don't feel like they're getting triggered right
  ;; from wherever my cursor is when I try, we can adjust this.
  ;; Don't see a need right now, though.
  ;; (yas-key-syntaxes '("w_" "w_." "^ "))


  ;;------------------------------
  :config
  ;;------------------------------

  ;;------------------------------
  ;; Advice
  ;;------------------------------

  (define-advice yas-insert-snippet (:before (&rest _) mantle:yas:interaction/type/insert)
    "Advice to set `mantle:yas:interaction/type' to `:insert'."
      (unless mantle:yas:interaction/type
        (setq mantle:yas:interaction/type :insert)))


  ;; This is also called when tabbing between fields in a snippet. Maybe only
  ;; because I have recursive snippets enabled? Anyway: beware.
  (define-advice yas-expand-from-trigger-key (:before (&rest _) mantle:yas:interaction/type/expand)
    "Advice to set `mantle:yas:interaction/type' to `:expand'."
    (unless mantle:yas:interaction/type
      (setq mantle:yas:interaction/type :expand)))

  ;; Dunno when this is called...
  (define-advice yas-expand-from-keymap (:before (&rest _) mantle:yas:interaction/type/expand)
    "Advice to set `mantle:yas:interaction/type' to `:expand'."
    (unless mantle:yas:interaction/type
      (setq mantle:yas:interaction/type :expand)))
  ;; (advice-remove 'yas-expand-from-keymap #'yas-expand-from-keymap@mantle:yas:interaction/type/expand)

  ;;------------------------------
  ;; Configuration
  ;;------------------------------

  ;; Suppress a warning about backquote behavior:
  ;;   > Warning (yasnippet): ‘/sec//: section <width>: prog-mode comment header section w/ settable width’ modified buffer in a backquote expression.
  ;;   > To hide this warning, add (yasnippet backquote-change) to ‘warning-suppress-types’.
  (imp:eval:after warnings
    (add-to-list 'warning-suppress-types '(yasnippet backquote-change)))

  ;;---
  ;; Snippet Paths
  ;;---
  ;; Want my snippets at the front of the list (ahead of yasnippet-snippets', if installed)
  ;; E.g.: mine, yasnippet's snippets dir, yasnippet-snippets' dir.

  ;; Always add innit's snippet path, if it exists.
  (when (path:exists? innit:path:snippet :dir)
    (add-to-list 'yas-snippet-dirs innit:path:snippet))

  ;; Also look for another snippet path in jerky.
  (when (and (jerky:get 'path 'dev-env 'snippets)
             (path:exists? (jerky:get 'path 'dev-env 'snippets) :dir))
    (add-to-list 'yas-snippet-dirs (jerky:get 'path 'dev-env 'snippets)))

  ;;---
  ;; Enable everywhere!
  ;;---
  (yas-global-mode +1))


;;------------------------------------------------------------------------------
;; Keybinds : Meow
;;------------------------------------------------------------------------------

(imp:use-package yasnippet
  :demand t
  :when  (imp:flag? :keybinds +meow)
  :after meow

  ;;------------------------------
  :bind ; meow
  ;;------------------------------

  ;; Bind something usable everywhere, like in meow insert mode.
  ("C-=" . yas-expand)


  ;;------------------------------
  :config
  ;;------------------------------

  ;;------------------------------
  ;; `General'
  ;;------------------------------

  (defun mantle:meow/keybind/general:snippets ()
    "Create the \"Snippets...\" keybinds in `general' for `meow'."
    (keybind:leader/global:def
      :infix (keybind:infix "i" "t")     ; insert -> templates
      "" '(nil :which-key "Templates & Snippets...") ; infix title

      "t" '(yas-insert-snippet :which-key "Insert Snippet...") ; generally we'll want to do this, so make it the 'primary' bind: "SPC i t t"
      "e" '(yas-expand         :which-key "Expand Snippet")))


  ;;------------------------------
  ;; `Transient'
  ;;------------------------------

  (defun mantle:meow/keybind/transient:snippets ()
    "Create the \"Snippets...\" keybinds in `transient' for `meow'."
    (transient-define-prefix mantle:meow/transient:dev-env:snippets ()
      "Snippet and template commands."
      ["Snippets..."
       [("=" "expand snippet" yas-expand)
        ("i" "insert snippet..." yas-insert-snippet)]])
    ;; (mantle:meow/transient:dev-env:snippets)

    ;; Snippets are quite common - put them outside the leader key.
    (meow-normal-define-key '("=" . mantle:meow/transient:dev-env:snippets)))


  ;;------------------------------
  ;; Actually Create Keybinds:
  ;;------------------------------

  (if (imp:provided? :keybinds 'general 'meow)
      (mantle:meow/keybind/general:snippets)
    (mantle:meow/keybind/transient:snippets)))


;;------------------------------------------------------------------------------
;; Keybinds : Evil
;;------------------------------------------------------------------------------

(imp:use-package yasnippet
  :demand t
  :when  (imp:flag? :keybinds +evil)
  :after (:and evil evil-collection)

  ;;------------------------------
  :general ; evil
  ;;------------------------------
  (keybind:global:def
    :states 'normal
  ;; Snippets are quite common - put them outside the leader key.
   :infix  "b"
   ;; Title
   "" '(nil :which-key "Snippets")

   ;;---
   ;; Snippet Menu
   ;;---
   "b" '(yas-expand         :which-key "Expand Snippet")
   "h" '(yas-insert-snippet :which-key "Insert Snippet...")))


;;------------------------------------------------------------------------------
;; Snippet Helper Functions
;;------------------------------------------------------------------------------

(defvar int<mantle/snippets>:number:cache
  nil
  "Prevent spamming for an input number during a snippet expansion.

Used by `mantle:snippet:number:parse'.")
;; TODO: this doesn't really work? `sec//' still asks more than once most of the time.


(defun mantle:snippet:number:cache/clear ()
  "Reset number cache for a new snippet or after a snippet is done with it."
  (setq int<mantle/snippets>:number:cache nil)
  ;; yasnippet will use whatever output is given, so give it an empty string.
  "")


(defun mantle:snippet:number:parse (parse &optional default prompt)
  "Return cached number or PARSE string as a number.

If PARSE is `:input' keyword, prompts for the string via `read-number'.
  - Will use PROMPT if it is a string (e.g. \"Column Width: \")

If DEFAULT is `numberp' and PARSE didn't parse to non-zero, returns DEFAULT.
  - Else returns zero.

If PARSE is not a string, returns DEFAULT/0.

Does some shenanigans with `int<mantle/snippets>:number:cache'
to prevent yas from calling too many times."
  (if int<mantle/snippets>:number:cache
      ;; User has already chosen - provide that choice.
      int<mantle/snippets>:number:cache

    ;; Get the number and save to prevent spamming for it again.
    (setq int<mantle/snippets>:number:cache
          (let ((fallback (if (numberp default)
                              default
                            0)))
            ;; Figure out what PARSE is.
            (cond ((eq parse :input)
                   (read-number (or prompt "Number: ")
                                ;; Use the fallback as default input if we have one.
                                (if (/= 0 fallback)
                                    fallback
                                  nil)))

                  ((numberp parse)
                   parse)

                  ((stringp parse)
                   (let ((parsed (string-to-number parse)))
                     ;; Return parsed result only if it succeeded.
                     (if (/= 0 parsed)
                         parsed
                       fallback)))

                  (t
                   fallback))))))
;; (mantle:snippet:number:parse "80")
;; (mantle:snippet:number:parse "jeff")
;; (mantle:snippet:number:parse "jeff" 80)
;; (mantle:snippet:number:parse :input 80)
;; (mantle:snippet:number:parse :input 80 "Width: ")


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'config 'dev-env 'snippets)

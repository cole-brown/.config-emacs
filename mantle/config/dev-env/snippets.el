;;; mantle/config/snippets.el --- Snippets, Templates, and $1 -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2022-08-05
;; Modified:   2022-08-05
;; URL:        https://github.com/cole-brown/.config-emacs
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


(imp:require :keybind)
(imp:require :elisp 'utils 'functions)


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
                   :docstr  ,macro<yasnippet>:docstr
                   ;; TODO: should I squelch this or no? If no, remove:
                   ;; :squelch t
                   )
                ;; ...All that for this:
                (yas-activate-extra-mode ,mode)))))))
;; (mantle:user:yasnippet:minor-mode/add test-mode)


;;------------------------------------------------------------------------------
;; YASnippets
;;------------------------------------------------------------------------------

(imp:use-package yasnippet
  :demand t

  ;;------------------------------
  :init
  ;;------------------------------

  ;;---
  ;; Default Snippets Location
  ;;---

  ;; Don't override if it was set in e.g. secrets init or system init or something.
  (when (null (jerky:get 'path 'dev-env 'snippets))
    (jerky:set 'path 'dev-env 'snippets
               :value (path:abs:dir user-emacs-directory "snippets")
               :docstr "Default path to snippets in `user-emacs-directory'."))

  ;;---
  ;; Hooks
  ;;---

  (innit:hook:defun
      (:name    "yasnippet"
       :file    macro<imp>:path/file
       :docstr  "Hook for yasnippet editting."
       :squelch t)
    ;; Normally we want a final newline in all files, so `require-final-newline'
    ;; is set to t. However, in yasnippet files, that means all snippets will
    ;; insert a "\n", and that's not desired.
    (setq require-final-newline nil))


  ;;------------------------------
  :hook
  ;;------------------------------
  (snippet-mode-hook . mantle:hook:yasnippet) ;; (innit:hook:func/name:symbol "yasnippet" nil)


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
  (yas-indent-line                 'auto)
  ;; Set these to true if `yas-indent-line' is non-nil.
  (yas-also-auto-indent-first-line (if yas-indent-line
                                       t
                                     yas-also-auto-indent-first-line))
  (yas-also-indent-empty-lines     (if yas-indent-line
                                       t
                                     yas-also-indent-empty-lines))

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
;; Keybinds
;;------------------------------------------------------------------------------

(imp:use-package yasnippet
  :demand t
  :after (:and evil evil-collection)

  ;;------------------------------
  :general ; evil
  ;;------------------------------
  ;; Snippets are quite common - put them outside the leader key.
  ;;---
  (:prefix  "b"
   ;; Title
   "" '(nil :which-key "Snippets")

   ;;---
   ;; Snippet Menu
   ;;---
   "b" '(yas-expand         :which-key "Expand Snippet")
   "h" '(yas-insert-snippet :which-key "Insert Snippet..."))

  ;; TODO-evil: Remove `evil-collection' keybinds or move into "b" prefix, or move
  ;; "b" prefix keybinds into `evil-collection' keybinds instead?

  ;; ;;------------------------------
  ;; :config
  ;; ;;------------------------------
  ;;
  ;; TODO-evil: This is from vanilla-Emacs-keybinds era, what needs done for Evil-keybinds era?
  ;; ;; Get rid of `yas-expand' binding on TAB. Annoyingly, cannot do this from the
  ;; ;; `:bind' section? And other annoyinglies as well. See sn-002 doc
  ;; ;; "yasnippet/unbind-tab.org" for more details.
  ;; (unbind-key "TAB"   yas-minor-mode-map)
  ;; (unbind-key "<tab>" yas-minor-mode-map)
  )



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
(imp:provide :mantle 'config 'user 'dev-env 'snippets)

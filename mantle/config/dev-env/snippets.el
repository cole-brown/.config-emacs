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


;;------------------------------------------------------------------------------
;; YASnippets
;;------------------------------------------------------------------------------

(imp:use-package yasnippet
  :demand t

  ;;--------------------
  :init
  ;;--------------------

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
      (list :name    "yasnippet"
            :file    (path:current:file)
            :docstr  "Hook for yasnippet editting."
            :squelch t
            :quiet   t)
    ;; Normally we want a final newline in all files, so `require-final-newline'
    ;; is set to t. However, in yasnippet files, that means all snippets will
    ;; insert a "\n", and that's not desired.
    (setq require-final-newline nil))


  ;;--------------------
  :hook
  ;;--------------------
  (snippet-mode . mantle:hook:yasnippet) ;; (innit:hook:func/name:symbol "yasnippet" nil)


  ;;--------------------
  :custom
  ;;--------------------

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


  ;;--------------------
  :general
  ;;--------------------
  ;; Snippets are quite common - put them outside the leader key.
  ;;---
  (:prefix  "b"
   :states  '(normal visual motion)
   :keymaps 'override
   ;; Title
   "" '(nil :which-key "Snippets")

   ;;---
   ;;
   ;;---
   "b" '(yas-expand         :which-key "Expand Snippet")
   "h" '(yas-insert-snippet :which-key "Insert Snippet..."))

  ;; TODO: Remove `evil-collection' keybinds or move into "b" prefix, or move
  ;; "b" prefix keybinds into `evil-collection' keybinds instead?

  ;; TODO: This is from vanilla-Emacs-keybinds era, what needs done for Evil-keybinds era?
  ;; ;; Get rid of `yas-expand' binding on TAB. Annoyingly, cannot do this from the
  ;; ;; `:bind' section? And other annoyinglies as well. See sn-002 doc
  ;; ;; "yasnippet/unbind-tab.org" for more details.
  ;; (unbind-key "TAB"   yas-minor-mode-map)
  ;; (unbind-key "<tab>" yas-minor-mode-map)


  ;;--------------------
  :config
  ;;--------------------

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
;; Snippet Helper Functions
;;------------------------------------------------------------------------------

;; TODO: move to mis, rename `mis:input//number-somethnig-or-other' or however private vars are...
(defvar sss:yas/number/parse-prevent-input-spam
  nil
  "For preventing yasnippet from spamming user too much during a snippet with `spy:yas/number/parse-or-default'.")


;; TODO: move to mis, rename `mis:input:clear' or something...
(defun spy:yas/number/clear ()
  "Resets `sss:yas/number/parse-prevent-input-spam' for a new snippet or after a snippet is done with it."
  (setq sss:yas/number/parse-prevent-input-spam nil)
  "")


;; TODO: move to mis, rename `mis:input:number'?
(defun spy:yas/number/parse-or-default (parse &optional default prompt)
  "Returns PARSE as a number if `string-to-number' returns non-zero.

If PARSE is `:input' keyword, prompts for the string via `read-number'.
  - Will use PROMPT if it is a string (e.g. \"Column Width: \")

If DEFAULT is non-nil and PARSE didn't parse to non-zero, returns DEFAULT.
  - Else returns zero.

If PARSE is not a string, returns DEFAULT/0.

Does some shenanigans with `sss:yas/number/parse-prevent-input-spam'
to prevent yas from calling too many times."
  (if sss:yas/number/parse-prevent-input-spam
      ;; User has already chosen - provide that choice.
      sss:yas/number/parse-prevent-input-spam

    ;; Get the number and save to prevent spamming for it again.
    (setq sss:yas/number/parse-prevent-input-spam
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
;; (spy:yas/number/parse-or-default "80")
;; (spy:yas/number/parse-or-default "jeff")
;; (spy:yas/number/parse-or-default "jeff" 80)
;; (spy:yas/number/parse-or-default :input 80)
;; (spy:yas/number/parse-or-default :input 80 "Width: ")


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'config 'user 'dev-env 'snippets)

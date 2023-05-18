;;; mantle/init/keybinds/+general-meow.el --- The General in Your Keybind Army -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2023-03-24
;; Modified:   2023-03-24
;; URL:        https://github.com/cole-brown/.config-emacs
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; The General in Your Keybind Army
;;
;;; Code:



(imp:require :nub)


(imp:use-package general
  :when  (imp:flag? :keybinds +meow)
  :after meow
  :demand t ;; Always load, if when/after/etc are valid.

  ;;------------------------------
  :init
  ;;------------------------------


  ;;------------------------------------------------------------------------------
  ;; General Constants
  ;;------------------------------------------------------------------------------

  (defconst keybinds:meow:keymaps/leader '(meow-normal-state-keymap meow-motion-state-keymap)
    "Keymaps for `meow' keybinds in the leader.

You probably don't want to use `override'! For example, you don't want keybind
't' to override the \"`self-insert' a letter 't' into the buffer here please\"
command.")


  (defconst keybinds:meow:keymaps/global '(meow-normal-state-keymap meow-motion-state-keymap)
    "Keymaps for global keybinds when using `meow'.

You probably don't want to use `override'! For example, you don't want keybind
't' to override the \"`self-insert' a letter 't' into the buffer here please\"
command.")


  ;;--------------------------------------------------------------------------------
  ;; Aliases
  ;;--------------------------------------------------------------------------------
  ;; Meow's 5 states: `normal', `insert', `motion', `keypad', and `beacon'.

  ;; Short name aliases for meow state/mode keymaps. These will take over the
  ;; pre-existing evil aliases if conflicted.
  (push '((n normal) . meow-normal-state-keymap) general-keymap-aliases)
  ;; (general--unalias 'normal)
  ;; (general--unalias 'n)
  (push '((b beacon) . meow-beacon-state-keymap) general-keymap-aliases)
  (push '((i insert) . meow-insert-state-keymap) general-keymap-aliases)
  (push '((k keypad) . meow-keypad-state-keymap) general-keymap-aliases)
  (push '((m motion) . meow-motion-state-keymap) general-keymap-aliases)

  ;; NOTE: Cannot alias multiple states? This doesn't work:
  ;; (push '(command . (meow-normal-state-keymap meow-motion-state-keymap)) general-keymap-aliases)


  ;;------------------------------------------------------------------------------
  ;; Leader Keys
  ;;------------------------------------------------------------------------------

  ;; TODO:meow:local: Need to figure out if we can use local leader with meow...
  ;; Might need some sort of function that dynamically figures out what keymap to invoke based on local major mode?

  ;; Leader keys and their definer functions created here.
  ;;
  ;; To prevent your global leader keybindings from ever being overridden (e.g. an
  ;; evil package may bind "SPC"), use ~:keymaps 'override~:
  ;;   (my-leader-def
  ;;     :states 'normal
  ;;     :keymaps 'override
  ;;     "a" 'org-agenda)
  ;;
  ;; or the compact version:
  ;;   (my-leader-def 'normal 'override
  ;;     "a" 'org-agenda)
  ;;
  ;; For local leaders, use ~:keymaps 'local~
  ;;
  ;; NOTE: By default, evil keybindings made with ~:keymaps 'override~ will override
  ;; even those made with ~:keymaps 'local~.
  ;;
  ;; https://github.com/noctuid/general.el#override-keymaps-and-buffer-local-keybindings
  ;;
  ;; For example, to ruin your `evil-next-line' keybind (assuming you use evil):
  ;;   (general-create-definer jay-def
  ;;     :prefix "j"
  ;;     :keymaps 'override)
  ;;   (general-def
  ;;    :states '(normal visual motion)
  ;;    "j" (lambda () (interactive) (message "hi")))
  ;;
  ;; Now "j j" prints "hi" to the *Messages* buffer (& minibuffer).


  ;;------------------------------------------------------------------------------
  ;; Leaderless / Global / Override
  ;;------------------------------------------------------------------------------
  ;; https://github.com/noctuid/general.el#evil-examples

  ;; This creates the macro `keybind:leader/global:def', which just calls
  ;; `general-def' with the arguments supplied here, which can be overridden by
  ;; callers.
  (general-create-definer keybind:global:def
    :keymaps keybinds:meow:keymaps/global)


  ;;------------------------------------------------------------------------------
  ;; Global Leader
  ;;------------------------------------------------------------------------------
  ;; https://github.com/noctuid/general.el#evil-examples

  ;; This creates the macro `keybind:leader/global:def', which just calls
  ;; `general-def' with the arguments supplied here, which can be overridden by
  ;; callers.
  (general-create-definer keybind:leader/global:def
    :prefix  keybind:leader/global:prefix
    ;; Make sure not to steal insert mode's `self-insert' " " keybind!
    :keymaps keybinds:meow:keymaps/leader)


  ;; Steal "SPC" for my own leader, give it its title, and I guess Meow's leader
  ;; can live inside "SPC"...
  (keybind:leader/global:def
    ;; Unbind the prefix and give it a title for which-key.
    "" '(nil :which-key "The Doyen of Keybind Leaders")

    ;; Usually bound as:
    ;;   `meow-motion-state-keymap SPC'
    ;;   `meow-normal-state-keymap SPC'
    ;; Rebind to be "SPC SPC":
    "SPC" (list #'meow-keypad :which-key "Meow Leader"))


  ;;------------------------------------------------------------------------------
  ;; Local Leader
  ;;------------------------------------------------------------------------------
  ;; https://github.com/noctuid/general.el#evil-examples

  (defun keybind:meow:leader/local:bind-keys (keymaps &rest args)
    "Bind ARGS in KEYMAPS under the `:personal' local leader prefix.

KEYMAPS must be a keymap or list of keymaps.

ARGS must be a sequence of key strings and funcs/lists/strings:
  - key: string for sending into `kbd'
  - func/list/string:
    - func: command to bind
    - list: list of: '(command-to-bind ...)
    - string: another key string to bind this key to
See `general' docs for details; however we must tweak the key string so this is
not the full `general-define-key' in that regard."
    (declare (indent 1))
    (let ((func/name "keybind:meow:leader/local:bind-keys")
          current/key
          current/bind
          binds
          ;; Normalize global keymap to nil.
          (keymaps/orig keymaps)
          (keymaps (int<keybind>:keymaps/normalize keymaps))) ; Normalize global MAP to nil.

      ;;------------------------------
      ;; KEYMAPS Error Checks
      ;;------------------------------
      (dolist (keymap keymaps)
        (unless (or (null keymap)
                    (keymapp keymap)
                    (and (symbolp keymap)
                         (keymapp (symbol-value keymap))))
          (nub:error
              :innit
              func/name
            '("Each keymap must be a keymap, or nil/`:global'/`global' for global keymap! "
             "%S -normalize-> %S: %S is invalid")
            keymaps/orig
            keymaps
            keymap)))

      (dolist (arg args)
        ;;------------------------------
        ;; ARGS Error Checks, Parsing
        ;;------------------------------
        ;; `nil' is a valid bind (unbinds the key), but we're ignoring it until we need it.
        (cond ((stringp arg)
               (if (null current/key)
                   (setq current/key (concat (keybind:leader/local:prefix :meow :emacs)
                                             " "
                                             arg))
                 (setq current/bind arg)))
              ((or (functionp arg)
                   (listp arg))
               (if (null current/bind)
                   (setq current/bind arg)
                 (nub:error
                     :innit
                     func/name
                   "Invalid arg '%S' in %S"
                   arg
                   args)))
              (t
               (nub:error
                   :innit
                   func/name
                 "Don't know how to parse ARGS. Got stuck on '%S' in %S"
                 arg
                 args)))

        ;;------------------------------
        ;; Save the keybind.
        ;;------------------------------
        (when (and current/key current/bind)
          (push current/bind binds)
          (push current/key  binds)
          (setq current/key  nil
                current/bind nil)))

      ;; Did we finish parsing cleanly?
      (when (or current/key current/bind)
        (nub:error
            :innit
            func/name
          "Incomplete parsing of args into keybinds. Left over from %S: key: %S bind: %S"
          args
          current/key
          current/bind))

      ;;------------------------------
      ;; ...Done parsing; create these binds.
      ;;------------------------------
      (apply #'general-define-key
             :keymaps keymaps
             binds)))
  ;; (keybind:meow:leader/local:bind-keys 'emacs-lisp-mode-map "x" (elisp:cmd (message "hello x")))
  ;; (keybind:meow:leader/local:bind-keys 'emacs-lisp-mode-map "x" (elisp:cmd (message "hello x")) "y" (elisp:cmd (message "hello y")))
  ;; (keybind:meow:leader/local:bind-keys nil "x" #'message "y" '(ignore :which-key "nothing"))


  (defvar int<keybind>:meow:leader/local:get/all:keymap nil
    "`which-key-show-keymap' needs a global symbol for what keymap to show.")


  (defvar keybind:meow:leader/local:get/all:exit
      (list (cons :no-commands (lambda () (message "Local Leader: No local keybinds for `%S'." major-mode)))
            (cons :commands    nil))
    "Functions to run in `keybind:meow:leader/local:get/all' when done.

Should be an alist of cons of: (keyword . function(symbol/lambda/etc))
Keywords are: `:no-commands', `:commands'")


  (defun keybind:meow:leader/local:get/all ()
    "Command to show local leader keybinds in `which-key' and set those keybinds as a transient map."
    (interactive)
    (let ((cmds (lookup-key (current-local-map) (kbd (keybind:leader/local:prefix :meow :emacs)))))
      (if (or (null cmds)     ; prefix is undefined in `current-local-map'
              (numberp cmds)) ; First bit of prefix exists, but at some point you walked off the edge of the world (of local keybinds).
          ;; No commands found; is there a func to run for that?
          (when-let ((func (alist-get :no-commands keybind:meow:leader/local:get/all:exit)))
            (funcall func))
        ;; Found some commands. Show them in `which-key' and set as the overriding keymap so we can call them.
        (setq int<keybind>:meow:leader/local:get/all:keymap cmds)
        (which-key-show-keymap 'int<keybind>:meow:leader/local:get/all:keymap t)
        (set-transient-map cmds
                           nil ; predicate for staying in the keymap
                           (alist-get :commands keybind:meow:leader/local:get/all:exit)))))
  ;; (keybind:meow:leader/local:get/all)


  ;; Create the local leader and its title.
  (general-define-key :prefix  keybind:leader/global:prefix
                      :keymaps keybinds:meow:keymaps/leader
                      ;; key in global leader:
                      (keybind:leader/local:prefix :meow :personal)
                      ;; Command that will just return all the local mode prefix binds:
                      '(keybind:meow:leader/local:get/all :which-key "Local Leader"))


  ;;------------------------------------------------------------------------------
  ;; Leader Infixes
  ;;------------------------------------------------------------------------------
  ;; Leader Keybinds used in multiple files in config should be created here so
  ;; that they are ready and existing for whoever happens to go in whatever
  ;; order.
  ;;------------------------------
  ;; NOTE: Defining the infix's title clears that key of any pre-existing binds,
  ;; which is the important reason for doing it this way. So... Do not include
  ;; that line elsewhere!
  ;; E.g.:
  ;; - Here:
  ;;     (keybind:leader/global:def
  ;;       :infix (keybind:infix "d")
  ;;       "" '(nil :which-key "Dev-Env, DevOps & Version Control...")) ; Infix's Title
  ;; - Elsewhere:
  ;;     (keybind:leader/global:def
  ;;       :infix (keybind:infix "d")
  ;;       ;; No Infix Title!
  ;;       ;; Now do keybinds:
  ;;       "a" (list #'command-goes-here :which-key "Command Title")
  ;;       "b" #'command-func-name-is-its-title
  ;;       "c" '(command-also-here :which-key "Also Title"))
  ;;------------------------------
  ;; NOTE: Keep in alphanumeric order, please!

  ;; Dev-Env / Devops
  (keybind:leader/global:def
    :infix (keybind:infix "d")
    "" '(nil :which-key "Dev-Env, DevOps & Version Control...")) ; Infix's Title

  ;; Insert
  (keybind:leader/global:def
    :infix "i"
    "" '(nil :which-key "Insert..."))

  ;; Apps & Stuff
  (keybind:leader/global:def
    :infix (keybind:infix "u")
    "" '(nil :which-key "Apps & Stuff"))


  ;;------------------------------------------------------------------------------
  ;; Feature for Definers, Etc.
  ;;------------------------------------------------------------------------------
  ;; TODO: Have imp provide all of everything to Emacs?
  ;;       - That is, replace `imp:provide' with `imp:provide:with-emacs' in imp.
  (imp:provide:with-emacs :keybinds 'user 'general 'meow))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'init 'keybinds 'general '+meow)

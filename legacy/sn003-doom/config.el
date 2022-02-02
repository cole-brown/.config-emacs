;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;;------------------------------------------------------------------------------
;;                                    DOOM
;;------------------------------------------------------------------------------
;;
;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!
;;   - You do, however, need to restart or do that command I forget to get
;;     the changes.


;;---------------------------------
;; DOOM INFO

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `imp:load' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; DOOM INFO
;;---------------------------------


;;------------------------------------------------------------------------------
;; Pre-Config Init, Includes
;;------------------------------------------------------------------------------

;; Step 00: Set the root path to our init files for Emacs.
(imp:path:root :dot-emacs
               ;;------------------------------
               ;; Figure out what kind of Emacs this is.
               ;;------------------------------
               ;; ".emacs.d", or ".doom.d", or ".config/emacs", or etc...
               (cond
                ;;---
                ;; Doom Emacs?
                ;;---
                ((boundp 'doom-private-dir)
                 doom-private-dir)

                ;;---
                ;; <Other Emacs Here>
                ;;---

                ;;---
                ;; Plain old vanilla Emacs
                ;;---
                (t
                 user-emacs-directory))
               ;;------------------------------
               ;; Init Files: init & locate features.
               ;;------------------------------
               ;; By default, these are:
               ;;   - `imp:path:filename:init'     -> "imp-init.el"
               ;;   - `imp:path:filename:features' -> "imp-features.el"
               ;; Could set here if I wanted different names/paths.
               )

;; Everything required before the config step is run.
(imp:load :feature  '(:dot-emacs init)
          :filename "init/init")


(defconst dot-emacs:path:config "config"
  "Relative path from `:dot-emacs' root directory to the config directory.")


;;------------------------------------------------------------------------------
;; NOTE: Function Naming
;;------------------------------------------------------------------------------

;; TODO: Rename (again (again (...)))?
;;   - spy:<category>:<func>
;;   - int<spy>:<category>:<func>
;;   - spy:cmd:<category>:<func>
;; NOTE: My functions are named thusly:
;;   - "spy:<category>/<func>": A *public* function in "category" namespace.
;;   - "sss:<category>/<func>": A *private* function in "category" namespace.
;;     + I don't want 'em all polluting the auto-complete, help, etc for "spy:".
;;   - "spy:cmd:<category>/<func>": aka "spy cmd"
;;     + A *public* and also /interactive/ function.


;; TODO: a readme...
;;   - func naming scheme


;;------------------------------------------------------------------------------
;; Secrets
;;------------------------------------------------------------------------------

;; Currently, need to configure my secrets before anything else.
;; TODO: move some stuff to init, use secrets config for /after/ non-secret
;; config is done?
(spy:secret/config)


;;------------------------------------------------------------------------------
;; Config Set-Up.
;;------------------------------------------------------------------------------

;; TODO: delete this
;; Our config files for different bits of emacs/doom/packages are in the
;; config sub-dir.
(defun spy:doom/find-user-root ()
  "Finds the user's base doom dir by walking down from this file's path."
  (let* ((file-path-this (if load-in-progress
                             (file-name-directory load-file-name)
                           (buffer-file-name)))
         (directory-path (directory-file-name
                          (file-name-directory file-path-this)))
         (directory-path-prev "")
         directory-doom)
    (while (and directory-path
                (not (string= directory-path directory-path-prev)))
      (let ((dirname (file-name-nondirectory directory-path)))
        (if (or (string= dirname ".doom.d") ;; for: ~/
                (string= dirname "doom"))   ;; for: ~/.config
            (setq directory-doom directory-path
                  directory-path nil)
          (setq directory-path-prev directory-path
                directory-path (directory-file-name
                                (file-name-directory directory-path))))))
    directory-doom))
;; (spy:doom/find-user-root)

;; TODO: delete this
(spy:config.root/set (path:join (spy:doom/find-user-root) "config"))


;;------------------------------------------------------------------------------
;; Emacs Set-Up.
;;------------------------------------------------------------------------------

(imp:load :feature  '(:dot-emacs config emacs)
          :path     dot-emacs:path:config
          :filename "emacs")

;; Speed up Emacs for files with long-ass lines.
(imp:load :feature  '(:dot-emacs config long-lines)
          :path     dot-emacs:path:config
          :filename "long-lines")

(imp:load :feature  '(:dot-emacs config daemons)
          :path     dot-emacs:path:config
          :filename "daemons")

(imp:load :feature  '(:dot-emacs config completion)
          :path     dot-emacs:path:config
          :filename "completion")

;; Get rid of some Doom annoying functionality with respect to parens...
(imp:load :feature  '(:dot-emacs config parenthesis)
          :path     dot-emacs:path:config
          :filename "parenthesis")

(imp:load :feature  '(:dot-emacs config search)
          :path     dot-emacs:path:config
          :filename "search")


;;------------------------------------------------------------------------------
;; Look & Feel
;;------------------------------------------------------------------------------

(imp:load :feature  '(:dot-emacs config theme config)
          :path     (imp:path:join dot-emacs:path:config "theme")
          :filename "config")

(imp:load :feature  '(:dot-emacs config ui)
          :path     dot-emacs:path:config
          :filename "ui")

(imp:load :feature  '(:dot-emacs config whitespace)
          :path     dot-emacs:path:config
          :filename "whitespace")


;;------------------------------------------------------------------------------
;; Cole Brown, Multi-pass.
;;------------------------------------------------------------------------------

(imp:load :feature  '(:dot-emacs config identity)
          :path     dot-emacs:path:config
          :filename "identity")


;; TODO: need to change whatever snippet doom uses for new .el files. My github
;; username is not my computer username.


;;------------------------------------------------------------------------------
;; Notes, Org-Mode and its Minions, etc.
;;------------------------------------------------------------------------------


(imp:load :feature  '(:dot-emacs config taskspace)
          :path     dot-emacs:path:config
          :filename "taskspace")

(imp:load :feature  '(:dot-emacs config org-mode)
          :path     dot-emacs:path:config
          :filename "org-mode")

(imp:load :feature  '(:dot-emacs config markdown)
          :path     dot-emacs:path:config
          :filename "markdown")


;;------------------------------------------------------------------------------
;; yasnippet
;;------------------------------------------------------------------------------

(imp:load :feature  '(:dot-emacs config yasnippet)
          :path     dot-emacs:path:config
          :filename "yasnippet")


;;------------------------------------------------------------------------------
;; Programming & Stuff
;;------------------------------------------------------------------------------

(imp:load :feature  '(:dot-emacs config code)
          :path     (imp:path:join dot-emacs:path:config "code")
          :filename "config")

(imp:load :feature  '(:dot-emacs config docker)
          :path     dot-emacs:path:config
          :filename "docker")

(imp:load :feature  '(:dot-emacs config treemacs)
          :path     dot-emacs:path:config
          :filename "treemacs")

;; vterm and friends
(imp:load :feature  '(:dot-emacs config terminal)
          :path     dot-emacs:path:config
          :filename "terminal")


;;------------------------------------------------------------------------------
;; Music & Entertainment
;;------------------------------------------------------------------------------

(imp:load :feature  '(:dot-emacs config spotify)
          :path     dot-emacs:path:config
          :filename "spotify")


;;------------------------------------------------------------------------------
;; Keybinds
;;------------------------------------------------------------------------------

;; Last so stuff doesn't get overwritten?
;; Not sure if that's actually a concern or not...

;;------------------------------
;; Input Method
;;------------------------------

;; Changes to Evil, Evil Settings, etc.
;;   - No changes to keybinds directly, but this is the most related section?
(imp:load :feature  '(:dot-emacs config evil)
          :path     dot-emacs:path:config
          :filename "evil")


;;------------------------------
;; Keyboard Layout
;;------------------------------

;; Fully controlled by '.doom.d/init.el'.
;;   - ':input/keyboard' module and its '+layout/spydez' flag.


;;------------------------------
;; Keybind Modifications
;;------------------------------

;; My additions to the overabundance of keybindings:
(imp:load :feature  '(:dot-emacs config keybinds spy-leader)
          :path     (imp:path:join dot-emacs:path:config "keybinds")
          :filename "spy-leader")

;; Specific things:
(imp:load :feature  '(:dot-emacs config keybinds org-mode)
          :path     (imp:path:join dot-emacs:path:config "keybinds")
          :filename "org-mode")

(imp:load :feature  '(:dot-emacs config keybinds search)
          :path     (imp:path:join dot-emacs:path:config "keybinds")
          :filename "search")

(imp:load :feature  '(:dot-emacs config keybinds spotify)
          :path     (imp:path:join dot-emacs:path:config "keybinds")
          :filename "spotify")

(imp:load :feature  '(:dot-emacs config keybinds treemacs)
          :path     (imp:path:join dot-emacs:path:config "keybinds")
          :filename "treemacs")

;; Whatever isn't big enough or important enough to warrent its own file.
(imp:load :feature  '(:dot-emacs config keybinds misc)
          :path     (imp:path:join dot-emacs:path:config "keybinds")
          :filename "misc")


;;------------------------------------------------------------------------------
;; The End
;;------------------------------------------------------------------------------

;; Show warnings if mis0 got any during init.
(mis0/init/complete 'show-warning)

(imp:provide :dot-emacs 'config)

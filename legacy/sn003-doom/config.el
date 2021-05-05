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
;; - `load!' for loading external *.el files relative to this one
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
;; Includes
;;------------------------------------------------------------------------------

(spy/require :spy 'jerky)
(spy/require :spy 'path)
(spy/require :spy 'config)
(spy/require :spy 'package)


;; NOTE: My functions are named thusly:
;;   - "spy/<name>": A "public" function.
;;   - "-s//<name>": A "private" function - I don't want 'em all polluting the
;;                   auto-complete, help, etc for "spy/".
;;   - "smd/<name>": aka "spy cmd"
;;                   A "public" and also /interactive/ function.


;; TODO: a readme...
;;   - func naming scheme

;;------------------------------------------------------------------------------
;; Config Set-Up.
;;------------------------------------------------------------------------------

;; Our config files for different bits of emacs/doom/packages are in the
;; config sub-dir.
(let* ((file-path-this (if load-in-progress
                          (file-name-directory load-file-name)
                        (buffer-file-name)))
      (directory-this (file-name-directory file-path-this)))
  (spy/config.root/set (spy/path/join directory-this "config")))


;;------------------------------------------------------------------------------
;; Emacs Set-Up.
;;------------------------------------------------------------------------------

(spy/config 'emacs)
(spy/config 'daemons)
(spy/config 'completion)


;;------------------------------------------------------------------------------
;; Look & Feel
;;------------------------------------------------------------------------------

(spy/config 'theme 'config)
(spy/config 'ui)
(spy/config 'whitespace)


;;------------------------------------------------------------------------------
;; Cole Brown, Multi-pass.
;;------------------------------------------------------------------------------

(spy/config 'identity)

;; TODO: need to change whatever snippet doom uses for new .el files. My github
;; username is not my computer username.


;;------------------------------------------------------------------------------
;; Keybinds
;;------------------------------------------------------------------------------

;; (spy/package 'hercules)

;; Changes to Emacs/Evil keybinds.
(spy/config 'keybinds)

;; Changes to Evil, Evil Settings, etc.
(spy/config 'evil)

;; My additions to a new entry in the SPC leader.
(spy/config 'spy 'keybinds)

;; Get rid of some Doom functionality...
(spy/config 'parenthesis)


;;------------------------------------------------------------------------------
;; Notes, Org-Mode and Legions, etc.
;;------------------------------------------------------------------------------

(spy/config 'taskspace)
(spy/config 'org-mode)


;;------------------------------------------------------------------------------
;; yasnippet
;;------------------------------------------------------------------------------

(spy/config 'yasnippet)


;;------------------------------------------------------------------------------
;; Programming & Stuff
;;------------------------------------------------------------------------------

(spy/config 'code)

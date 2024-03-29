;;; settings.el --- Settings for Innit -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2022-04-11
;; Timestamp:  2023-09-26
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Settings that the user needs ASAP (e.g. during "early-init.el" or before user
;; init/config) should go here.
;;
;; NOTE: Normal settings should go in a `use-package emacs' block.
;; Example at "$(git rev-parse --show-toplevel)/mantle/config/emacs.el"
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Debug
;;------------------------------------------------------------------------------

;; You should use the Emacs `--debug-init' flag on the command line...
;; (setq init-file-debug nil)
;; (setq debug-on-error nil)


;;------------------------------------------------------------------------------
;; `innit' Output
;;------------------------------------------------------------------------------

;; Allow messages during innit for debugging.
(innit:customize-set-variable innit:display:messages  t)
(innit:customize-set-variable innit:display:load-file t)


;;------------------------------------------------------------------------------
;; Feature Flags
;;------------------------------------------------------------------------------
;; Two ways to check for a feature:
;;   1. Desired:
;;      (when (imp:flag? :keybinds +evil)
;;        ...)
;;
;;   2. Existing:
;;      (when (imp:feature? 'evil)
;;        ...)
;;      ;; or
;;      (when (featurep 'evil)
;;        ...)
;;
;; If you are checking after package init (e.g. in a `use-package' `:config'
;; section), you can use #2.
;;
;; If you are checking earlier, use #1.
;;
;; And it's not terrible to use both:
;;   (imp:use-package general
;;     :when  (imp:flag? :keybinds +evil)
;;     :after (:and magit evil evil-collection)
;;     ...)


;;------------------------------
;; Input
;;------------------------------

;; (imp:flag :keyboard +qwerty)
(imp:flag :keyboard +dvorak)

;; Used to decide what, if any, input systems to configure.
;; (imp:flag :keybinds +evil) ; https://github.com/emacs-evil/evil
(imp:flag :keybinds +meow)    ; https://github.com/meow-edit/meow


;;------------------------------
;; Development
;;------------------------------

;;---
;; Language Server Protocol
;;---
(imp:flag :dev-env +lsp)
;; (imp:flag :dev-env +eglot)

;;---
;; Language Servers
;;---

;; So `:dev-env +lsp' or `:dev-env +eglot' are enabled, what languages should
;; have language servers enabled for them?
(imp:flag :lsp +csharp)
;; (imp:flag :lsp +python)
;; (imp:flag :lsp +rust)

;;---
;; Debug Adapter Protocol
;;---
(imp:flag :dev-env +dap)


;;------------------------------
;; Automatic
;;------------------------------
;;
;; (imp:flag :emacs +undo-tree)
;;   - Should be set by your `undo-tree' config in the `use-package' `:init' section.


;;------------------------------------------------------------------------------
;; Frame
;;------------------------------------------------------------------------------

;; TODO: Initial size / position should depend on what system we're on...

;;----------------------------
;; Initial Frame Size
;;----------------------------
;; Set `initial-frame-alist' for only the first frame created, or set
;; `default-frame-alist' for _all_ frames created.
;;
;; `fullscreen' options are:
;;   - `fullwidth'  - make the frame as wide as possible, don't touch the vertical
;;   - `fullheight' - make the frame as tall as possible, don't touch the horizontal
;;   - `fullboth'   - set height and width to the size of the screen
;;   - `maximized'  - make it, well, maximized
;; The difference between `fullboth' and `maximized' is that you can resize the
;; former with the mouse, while with the latter you cannot.

(add-to-list 'initial-frame-alist '(fullscreen . maximized))


;;----------------------------
;; Initial Frame Position
;;----------------------------
;; We also want to open it in the main/center monitor, which is... hacky?
;;  - https://stackoverflow.com/a/11460472
;;
;; 1. Move the frame where you want it.
;; 2. Use `frame-parameters' to get the top/left values. But it gives a
;;    ridiculously long line, so pretty print it?
;;    - (pp (frame-parameters))
;; 3. Find the `top' and `left' alist entries:
;;    (...
;;     (top . 26)
;;     (left . 1079)
;;     ...)
;;
;;---
;; NOTE: Negative `top'/`left' values:
;;---
;; If you have displays above or to the left of the main display, the top/left
;; values for windows on that display will be negative. Emacs's GUI support,
;; coming originally from X11, didn't have any concept of negative coordinates.
;; If you try to use negative values, Emacs interprets them as relative to the
;; bottom/right of the main display instead — probably not what you want.
;;
;; There's a hack in the frame parameter alist to handle this situation: when
;; storing negative top/left values, the corresponding list items are a list of
;; three items instead. For example:
;;
;; (... (top + -51) (left + -1674) ...)
;;
;; Normally you can provide these items directly to make-frame, but it doesn't
;; understand this hack. Instead, you can make the frame and immediately move it
;; by modifying its parameters. For example, I've got a 1680x1050 display to the
;; left and slightly above my main display, so this works to display a new frame
;; on that display:
;;
;; (modify-frame-parameters (make-frame) '((top + -51) (left + -1674)))

;;---
;; 3 x Dell U2713HM 27" at 1920x1080
;;   - Left:   Vertical   (1080x1920)
;;   - Center: Horizontal (1920x1080)
;;   - Center: Horizontal
;; Center Monitor is:
(add-to-list 'initial-frame-alist '(top . 26))
(add-to-list 'initial-frame-alist '(left . 1079))


;;------------------------------
;; Frame Resize
;;------------------------------

;; Resize based on pixels, not characters. Allows maximizing to actually
;; maximize.
(innit:customize-set-variable frame-resize-pixelwise t)

;; TODO: What is this set to by default?
;; But do not resize windows pixelwise, this can cause crashes in some cases
;; when resizing too many windows at once or rapidly.
;; (innit:customize-set-variable window-resize-pixelwise nil)


;;------------------------------
;; Frame UI Elements
;;------------------------------
;; Disable tool, menu, and scrollbars. Too much screen space and clutter when
;; you should be using the keyboard.

;; Menu Bar (File, Edit... Menus): No.
(when (bound-and-true-p menu-bar-mode)
  (menu-bar-mode -1))

;; Tool Bar (New, Open... Buttons): Go away.
(when (bound-and-true-p tool-bar-mode)
  (tool-bar-mode -1))

;; Scroll Bar: Hm..........
;; Not sure.
;;   - On the one hand, get rid of it to get a bit more screen real estate.
;;   - On the other hand, keep it for showing where in buffer we are and how big
;;     buffer is?
(when (bound-and-true-p scroll-bar-mode)
  (scroll-bar-mode -1))

;; Don't want popups for "help text". Put it in the echo area instead.
(when (bound-and-true-p tooltip-mode)
  (tooltip-mode -1))


;;--------------------------------------------------------------------------------
;; Font
;;--------------------------------------------------------------------------------

;;------------------------------
;; Dell U2713HM 27" @ 1920x1080:
;;------------------------------
;; Was just using the default OS font & size, which is set in Gnome Tweaks on
;; Ubuntu 20.04, and is:
;;   > Monospace Text: Cascadia Code PL Regular 8 pt
;; (set-face-attribute 'default
;;                     nil
;;                     :font "Cascadia Code PL"
;;                     ;; integers -> absolute font height in 1/10th font point (pt) units.
;;                     :height 110)


;;------------------------------
;; Gigabyte M28U 28" @ 4k (3840x2160):
;;------------------------------
;; 1. Set font family.
;; 2. Increase default font size for 4k monitor?
;;
;; NOTE: 150% "fractional scaling" in the display settings makes these font
;; sizes a bit weird after reboot... Setting to "110 height" aka "11 pt" make it
;; bigger than it is, and makes it about the size it used to be before the 4k
;; monitor joined the mix and before "fractional scaling" was enabled, but
;; according to Gnome Tweaks, the default monospaced font size is 8 pt, not 11
;; pt? ¯\_(ツ)_/¯
(set-face-attribute 'default
                    nil
                    :font "Cascadia Code PL"
                    ;; integers -> absolute font height in 1/10th font point (pt) units.
                    :height 110)


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :settings)

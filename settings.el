;;; settings.el --- Settings for Innit -*- lexical-binding: t; -*-
;;
;; Author: Cole Brown <code@brown.dev>
;; URL:    https://github.com/cole-brown/.config-emacs
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

;;------------------------------
;; Frame UI Elements
;;------------------------------

;; Menu Bar (File, Edit... Menus): No.
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))

;; Tool Bar (New, Open... Buttons): Go away.
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; Scroll Bar: Hm..........
;; Not sure.
;;   - On the one hand, get rid of it to get a bit more screen real estate.
;;   - On the other hand, keep it for showing where in buffer we are and how big
;;     buffer is?
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :settings)

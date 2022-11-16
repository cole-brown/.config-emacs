;;; mantle/config/keybinds/infixes.el --- Keybind Leader Sub-Menus -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2022-10-19
;; Modified:   2022-10-19
;; URL:        https://github.com/cole-brown/.config-emacs
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Define keybind leader sub-menus
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Global
;;------------------------------------------------------------------------------
;; NOTE: Keep in alphanumerical order!

(keybind:leader/global:def
 :infix (keybind:infix "i")        ; insert
 "" '(nil :which-key "Insert...")) ; infix's title


(keybind:leader/global:def
 :infix (keybind:infix "f")      ; files
 "" '(nil :which-key "File...")) ; infix's title


(keybind:leader/global:def
 :infix (keybind:infix "n")       ; notes
 "" '(nil :which-key "Notes...")) ; infix's title


(keybind:leader/global:def
 :infix (keybind:infix "t")      ; text
 "" '(nil :which-key "Text...")) ; infix's title


(keybind:leader/global:def
 :infix (keybind:infix "/")        ; search
 "" '(nil :which-key "Search...")) ; infix's title


;;-----------------------------------------------------------------------------
;; Local
;;-----------------------------------------------------------------------------



;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'config 'user 'keybinds 'infixes)

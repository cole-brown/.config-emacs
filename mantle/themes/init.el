;;; init.el --- Init User's Theme -*- lexical-binding: t; -*-
;;
;; Author: Cole Brown <code@brown.dev>
;; URL:    https://github.com/cole-brown/.config-emacs
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;; Code:


(nub:out :innit
         :debug
         (imp:file:current)
         "mantle/theme/init: Start")


;;------------------------------------------------------------------------------
;; Set Theme
;;------------------------------------------------------------------------------

(defcustom mantle:theme:path (imp:path:join (imp:path:current:dir) "theme")
  "Absolute path string to theme's directory (or file).

Used by `imp:load'.

`mantle:theme:path' and/or `mantle:theme:file' must be set in order to load
your theme."
  :group 'innit:group
  :type  'string)


(defcustom mantle:theme:file "zenburn"
  "String of either filename or absolute filepath to theme.

Used by `imp:load'.

`mantle:theme:path' and/or `mantle:theme:file' must be set in order to load
your theme."
  :group 'innit:group
  :type  'string)


;; Delibrately set `mantle:theme:file' to the filename sans extension so that we
;; can co-opt it here for the feature name, but could be just:
;;   '(:mantle theme zenburn)
;; ...which might be less confusing.
(defcustom mantle:theme:feature (list :mantle 'theme mantle:theme:file)
  "`imp' feature name for theme.

Used by `imp:load'.

Set to `nil' if you don't want `mantle' to load a theme for you."
  :group 'innit:group
  :type  '(restricted-sexp :match-alternatives (keywordp symbolp stringp)))


;;------------------------------------------------------------------------------
;; Load User's Theme(s)
;;------------------------------------------------------------------------------

(imp:load :feature  mantle:theme:feature
          :path     mantle:theme:path
          :filename mantle:theme:file)


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(nub:out :innit
         :debug
         (imp:file:current)
         "mantle/theme/init: End")
(imp:provide :mantle 'theme 'init)

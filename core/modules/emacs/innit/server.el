;;; core/modules/emacs/innit/server.el -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2022-03-22
;; Modified:   2023-05-04
;; URL:        https://github.com/cole-brown/.config-emacs
;; Copyright (C) 2022 Cole Brown
;;
;;; Commentary:
;;
;; `server-running-p' doesn't work.
;;   - http://emacshorrors.com/posts/determining-if-the-server-is-started-or-the-wonders-of-server-running-p.html
;;     -> http://lists.gnu.org/archive/html/bug-gnu-emacs/2018-06/msg00723.html
;; So this is actually not useful:
;;   (unless (server-running-p) (server-start))
;;
;; So these are some functions for figuring out if Emacs Server is running, so
;; you can actually do things like that. Then maybe Emacs will collect itself
;; into one instance, and then maybe drag-drop, send from Visual Studio, etc
;; will work as expected.
;;
;; For daemony things, see:
;;   https://www.emacswiki.org/emacs/EmacsAsDaemon
;;
;; Flavors Theoretically* Possible:
;;   - Daemons
;;   - Servers
;;   - Server Daemons
;;   - Serverless Cloud Daemons
;;   - Waitress Daemons
;;
;; Flavors Currently Possible:
;;   - Servers?
;;
;;; Code:


(require 'server)

(imp:require :path)


;;------------------------------------------------------------------------------
;; Emacs Server
;;------------------------------------------------------------------------------

(defun innit:emacs/server:path ()
  "Return absolute path to server file/socket if one exists, else return nil."
  ;; I can't see any useful `server-*' vars/funcs that will actually just tell
  ;; me which one (if any) is _in use_... So just check both.
  (let ((path/socket (path:join server-socket-dir server-name))
        (path/auth   (path:join server-auth-dir server-name)))
    ;; NOTE: Can be a socket, so don't use a TYPE param (e.g. `:file').
    (cond ((path:exists? path/socket)
           path/socket)
          ((path:exists? path/auth)
           path/auth)
          (t
           nil))))
;; (innit:emacs/server:path)


;; `server-running-p' doesn't work.
;;   - http://emacshorrors.com/posts/determining-if-the-server-is-started-or-the-wonders-of-server-running-p.html
;;     -> http://lists.gnu.org/archive/html/bug-gnu-emacs/2018-06/msg00723.html
;; (unless (server-running-p) (server-start))
(defun innit:emacs/server:process? ()
  "Return non-nil if this Emacs has a server process."
  (bound-and-true-p server-process))
;; (innit:emacs/server:process?)


(defun innit:emacs/server:running? ()
  "Return non-nil if this Emacs has a server process and a server file/socket."
  (and (innit:emacs/server:process?)
       (innit:emacs/server:path)))
;; (innit:emacs/server:running?)


;;------------------------------------------------------------------------------
;; The End!
;;------------------------------------------------------------------------------
(imp:provide :innit 'server)

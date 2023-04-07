;;; -*- lexical-binding: t; -*-

;;-------------------------------------dir--------------------------------------
;;--                          Directory Functions                             --
;;----------------------------/mnt/hello/there.txt------------------------------

(require 'cl-lib)


(imp:require :path 'path)


;;------------------------------------------------------------------------------
;; Dir Name
;;------------------------------------------------------------------------------

(defun dir:name (path &rest segment)
  "Return the directory name from PATH & SEGMENT(s).

Example:
  (dir:name \"/foo\" \"bar.tar.gz\")
  (dir:name \"/foo/bar.tar.gz\")
  (dir:name \"c:/foo/bar.tar.gz\")
    -> \"bar.tar.gz\""
  (file-name-nondirectory (apply #'path:canonicalize:file path segment)))
;; (dir:name "/foo" "bar.tar.gz")
;; (dir:name "/path/to/foo/")


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :path 'dir)

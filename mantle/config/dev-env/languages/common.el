;;; mantle/config/dev-env/languages/common.el --- Configure All Languages -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2022-08-05
;; Modified:   2022-12-09
;; URL:        https://github.com/cole-brown/.config-emacs
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;  Configure for All Languages
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Comments
;;------------------------------------------------------------------------------

;;------------------------------
;; Block Commenting Style
;;------------------------------

;; Only use for languages that have ending comments, like C's "/* */".
(innit:hook:defun
    (:name   'dev-env:languages/comments:block/align
     :file   (path:current:file)
     :docstr "Set comment style for start/end line comment languages (C, HTML, ...).")
  ;; `aligned' is like (default) `indent' but also aligns end-of-line comment chars if the language has them.
  (setq 'comment-style 'aligned))
;; NOTE: Need to add this hook (`mantle:hook:dev-env:languages/comments:block/align') to whatever modes want it.

;; TODO: Move to "languages/html.el"?
;; `html-mode-hook' (HTML mode) or `mhtml-mode-hook' (HTML+ mode (derived from HTML mode))?
(add-hook 'html-mode-hook 'mantle:hook:comments:block/align)

;; NOTE: C-mode hook added (or not) in "language/c-and-cpp.el".


;;------------------------------------------------------------------------------
;; Defaults for All Programming Modes
;;------------------------------------------------------------------------------

(innit:hook:defun-and-add prog-mode-hook
    '(:name "prog-mode/settings"
      :file (path:current:file)
      :docstr "Settings for all prog-mode derived modes. Non-LSP stuff.")

    ;; My 2560x1440 monitor can display around 152 columns when fullscreen with
    ;; two vertically-split windows, so we'll do 140 as our fill column?
    (setq fill-column 140))


;;------------------------------------------------------------------------------
;; Metasyntatic Variables
;;------------------------------------------------------------------------------

(defvar mantle:user:code:variables/metasyntatic
  '((:metasyntactic
     (foo (bar (baz (qux (quux (quuux (quuuux (quuuuux))))))
               (thud (grunt))
               (bletch)
               (fum)
               (bongo)
               (zot)))
     (bazola (ztesch))
     (fred (jim (sheila (barney))))
     (corge (grault (flarp)))
     (zxc (spqr (wombat)))
     (shme)
     (spam (eggs))
     (snork)
     (blarg (wibble))
     (toto (titi (tata (tutu))))
     (pippo (pluto (paperino)))
     (aap (noot (mies)))
     (oogle (foogle (boogle (zork (gork (bork)))))))

    (:pinky (narf (zort (poit (egad (troz (fiddely-posh))))))))
  "Alist of meaningless/placeholder variable name progressions.

See: http://www.catb.org/jargon/html/M/metasyntactic-variable.html")


;; (defun mantle:user:code:variable/metasyntatic (&optional type)
;;   "Insert a metasyntatic variable at point.
;;
;; If TYPE is a keyword, use that list in variable
;; `mantle:user:code:variable/metasyntatic'."
;;   (interactive)
;;   (let ((tree-of-meta (alist-get (or nil :metasyntactic) mantle:user:code:variables/metasyntatic)))
;;     ;; TODO: Would be nice to progress in the metavars usage but idk how to, easily.
;;     ;; So just... chose a random?
;;     ;; TODO: finish this?
;;     ))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'config 'user 'dev-env 'languages 'common)

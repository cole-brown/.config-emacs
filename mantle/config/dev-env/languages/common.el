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

;;------------------------------
;; Fill Columns
;;------------------------------
;;                                                                             80                                     120                 140       150

(jerky:set 'fill-column 'narrow
           :namespace :default
           :value 80
           :docstr "Ye olde default 80 columns.")
;; NOTE: Python likes to be weird - it defaults to wanting 79 columns.


(jerky:set 'fill-column 'wide
           :namespace :default
           :value 120
           :docstr "Current Year should be able to handle more than 80, right? How about 120 columns?")


(jerky:set 'fill-column 'maximized '1080px ; 1920x1080
           :namespace :default
           :value 150
           :docstr "Two side-by-side buffers can handle about 150 columns maximum (w/ current font, gutters, etc).")


(jerky:set 'fill-column 'maximized '1440px ; 2560x1440
           :namespace :default
           :value 150
           :docstr "Two side-by-side buffers can handle about 150 columns maximum (w/ current font, gutters, etc).")


(jerky:set 'fill-column 'maximized '1920px ; 1080x1920 aka sideways 1920x1080
           :namespace :default
           :value 170
           :docstr "One buffers can handle about 170 columns maximum (w/ current font, gutters, etc).")


(jerky:set 'fill-column 'maximized '2560px ; 1440x2560 aka sideways 2560x1440
           :namespace :default
           :value 170
           :docstr "One buffers can handle about 170 columns maximum (w/ current font, gutters, etc).")


;; TODO: Set `fill-column' for other modes too...
;;   Yes: org-mode, org-journal-mode
;;   No!: magit
;; Will have to move the `jerky:set' calls to earlier in config.
(innit:hook:defun-and-add prog-mode-hook
    (:name 'prog:settings
      :file (path:current:file)
      :docstr "Settings for all prog-mode derived modes. Non-LSP stuff.")

    ;; 'wide' is a decent default, probably?
  (setq fill-column (jerky:get 'fill-column 'wide)))


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
;; Which Function?
;;------------------------------------------------------------------------------

(imp:use-package which-function-mode
  ;;------------------------------
  :config
  ;;------------------------------
  ;; Show the current function name in the header line.
  (which-function-mode)
  (setq-default header-line-format
                '((which-func-mode ("" which-func-format " "))))

  (setq mode-line-misc-info
        ;; Remove the current function name from the mode line, because it's
        ;; mostly invisible here anyway.
        (assq-delete-all 'which-func-mode mode-line-misc-info)))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'config 'user 'dev-env 'languages 'common)

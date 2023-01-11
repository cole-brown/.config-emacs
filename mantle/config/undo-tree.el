;;; mantle/config/undo-tree.el --- undo-tree configuration -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2022-07-13
;; Modified:   2022-07-13
;; URL:        https://github.com/cole-brown/.config-emacs
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Configure Undo and its Tree.
;;
;; NOTE: Prereq for `evil'!
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Undo-Tree
;;------------------------------------------------------------------------------

(let ((file/this (imp:file:current)))
  (imp:use-package undo-tree
    :demand t ;; Always load.

    ;;--------------------
    :init
    ;;--------------------

    ;; Declare that we'll have `undo-tree' available as our undo feature.
    ;; Evil will need to knaw about it.
    ;;   https://github.com/emacs-evil/evil#dependencies
    (imp:flag :emacs +undo-tree)


    ;;--------------------
    :custom
    ;;--------------------

    (undo-tree-visualizer-diff t)
    (undo-tree-auto-save-history t)
    (undo-tree-enable-undo-in-region t)

    ;; Increase undo limits to avoid emacs prematurely truncating the undo
    ;; history and corrupting the tree. Think big.. `undo-tree' history trees
    ;; consume exponentially more space than linear undo histories, and then
    ;; some when `undo-tree-enable-undo-in-region' is involved. See
    ;; syl20bnr/spacemacs#12110
    (undo-limit        (*  16 1024 1024)) ;  16mb (default is 160kb)
    (undo-strong-limit (*  64 1024 1024)) ;  64mb (default is 240kb)
    (undo-outer-limit  (* 256 1024 1024)) ; 256mb (default is 24mb)


    ;;--------------------
    :general
    ;;--------------------
    ;; No prefix. Used very frequently.
    (:states  'normal
     :keymaps 'override
     "h" (list #'undo-tree-undo :which-key "undo")
     "n" (list #'undo-tree-redo :which-key "redo")
     "t" (list #'undo-tree-visualize :which-key "visualize")
     "c" (list #'undo-tree-switch-branch :which-key "switch undo branch")

     ;; TODO: Keybinds for these maps:
     ;; `undo-tree-visualizer-selection-mode-map'
     ;; `undo-tree-visualizer-mode-map'
     )


    ;;--------------------
    :config
    ;;--------------------

    ;; Prefer `zstd' for compressing history files.
    (cond ((executable-find "zstd")
           (nub:info
              :innit
              file/this
            "`undo-tree': Using compression `zstd' for undo history files.")
           (define-advice undo-tree-make-history-save-file-name (:filter-return (file) mantle:advice:compress/zstd)
             (concat file ".zst")))
          ((executable-find "gzip")
           (nub:info
              :innit
              file/this
            "`undo-tree': `zstd' not found; using compression `gzip' for undo history files.")
           (define-advice undo-tree-make-history-save-file-name (:filter-return (file) mantle:advice:compress/zstd)
             (concat file ".gz")))
          ;; Fallback: Just don't compress. :o
          (t
           (nub:info
              :innit
              file/this
            '("`undo-tree': No compression found for undo history files; install "
              "`zstd' (preferred) or `gzip' to enable compression of `undo-tree' "
              "history files."))
           nil))

    ;; Strip text properties from undo-tree data to stave off bloat. File size
    ;; isn't the concern here; undo cache files bloat easily, which can cause
    ;; freezing, crashes, GC-induced stuttering or delays when opening files.
    ;; TODO: Does `nil' for LAMBDA-LIST work for function without params?
    (define-advice undo-list-transfer-to-tree (:before nil mantle:advice:strip-text-props)
      (dolist (item buffer-undo-list)
        (and (consp item)
             (stringp (car item))
             (setcar item (substring-no-properties (car item))))))

    ;; Undo-tree is too chatty about saving its history files. This doesn't
    ;; totally suppress it logging to *Messages*, it only stops it from appearing
    ;; in the echo-area.
    (define-advice undo-tree-save-history (:around (fn &rest args) mantle:user:squelch)
      "Undo-tree is too chatty about saving its history files; prevent it from using minibuffer."
      (innit:squelch/unless :interactive? t
                            :allow-messages? t
                            (apply fn args)))

    (global-undo-tree-mode +1)))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'config 'user 'undo-tree)

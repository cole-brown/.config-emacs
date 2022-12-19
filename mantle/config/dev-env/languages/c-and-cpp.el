;;; mantle/config/dev-env/languages/c-and-cpp.el --- Config the C. -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2021-09-27
;; Modified:   2022-12-12
;; URL:        https://github.com/cole-brown/.config-emacs
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;  Config the C.
;;  And the C++.
;;
;;; Code:


(imp:require :keybind)
(imp:require :jerky)


;;------------------------------
;; NOTE:
;;------
;; C and C++ are exactly the same right now... so both are here together.
;;
;; If they start to diverge, split them up?
;;------------------------------


;;----------------------------------------------------------------------------
;; C / C++ Mode
;;----------------------------------------------------------------------------

;;------------------------------
;; NOTE:
;; -----
;; C Mode currently has block style ('/* */') commenting.
;;   - This can be toggled by `c-toggle-comment-style' command.
;;     + If ARG is positive: set to style block ('/* */').
;;     + If ARG is negative: set to line style ('//').
;;
;; If you want to change this, do something about the
;; `mantle:hook:dev-env:languages/comments:block/align' hook function as well
;; maybe?
;;------------------------------


;; TODO: Check this?
;;   https://github.com/dholm/dotemacs/blob/master/.emacs.d/lisp/modes/c-c%2B%2B.el
;;   CEDET and stuff...
(imp:use-package cc-mode
  :ensure nil ; This is an Emacs built-in feature.

  ;;--------------------
  :init
  ;;--------------------

  (innit:hook:defun
      (:name   'cc:settings
       :file   macro<imp>:path/file
       :docstr "Settings for C/C++ mode. Non-LSP stuff.")

    ;; Use BSD style in C, C++.
    (c-set-style "bsd")
    ;; Available C style:
    ;;   - "gnu":        The default style for GNU projects
    ;;   - "k&r":        What Kernighan and Ritchie, the authors of C used in their book
    ;;   - "bsd":        What BSD developers use, aka "Allman style" after Eric Allman.
    ;;   - "whitesmith": Popularized by the examples that came with Whitesmiths C, an early commercial C compiler.
    ;;   - "stroustrup": What Stroustrup, the author of C++ used in his book
    ;;   - "ellemtel":   Popular C++ coding standards as defined by "Programming in C++, Rules and Recommendations," Erik Nyquist and Mats Henricson, Ellemtel
    ;;   - "linux":      What the Linux developers use for kernel development
    ;;   - "python":     What Python developers use for extension modules
    ;;   - "java":       The default style for java-mode (see below)
    ;;   - "user":       When you want to define your own style
    ;;   - See `c-style-alist' for supported strings.
    ;;
    ;; See this for visuals: https://en.wikipedia.org/wiki/Indentation_style

    ;; 'wide' is a decent default, probably?
    (setq fill-column (jerky:get 'fill-column 'wide))

    (setq c-basic-offset (jerky:get 'code 'tab 'normal))

    (c-set-offset 'innamespace 0) ; Don't indent namespace - waste of indent level
    (c-set-offset 'case-label '+) ; indent case labels by c-indent-level, too

    ;; Separate CamelCase & snake_case into separate words?
    ;; (subword-mode t)
    ;; Too much work for forwards/back normally. Easy toggle would be nice?
    )

  ;;--------------------
  :hook
  ;;--------------------
  ((c++-mode . sss:hook/c-and-cpp/settings)
   (c-mode . sss:hook/c-and-cpp/settings)
   (c-mode . mantle:hook:dev-env:languages/comments:block/align))

  ;;--------------------
  :config
  ;;--------------------

  (add-to-list 'auto-mode-alist '("\\.cxx$" . c++-mode))
  (add-to-list 'auto-mode-alist '("\\.cpp$" . c++-mode))

  ;; Can setup auto-complete, company, flycheck, lots others here.
  ;; TODO: Lots more here maybe when I do some C/C++ work again?

  ;; TODO-LSP: LSP?
  )


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'config 'user 'dev-env 'c-and-cpp)

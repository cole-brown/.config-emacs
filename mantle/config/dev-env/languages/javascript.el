;;; mantle/config/dev-env/javascript.el --- Development Environment -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2023-01-05
;; Modified:   2023-01-05
;; URL:        https://github.com/cole-brown/.config-emacs
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;  Development Environment
;;
;;; Code:


(imp:require :keybind)


;;------------------------------------------------------------------------------
;; Check for Required Executables
;;------------------------------------------------------------------------------
;; Check that `node' and other exes required for JS dev are available on this system.

(unless (executable-find "node")
  (nub:warning
      :innit
      (imp:path:join (imp:path:current:dir/relative :mantle)
                     (imp:path:current:file))
    '("Could not find `node' executable. Is it installed? "
      "JavaScript config wants it.")))


(unless (executable-find "npm")
  (nub:warning
      :innit
      (imp:path:join (imp:path:current:dir/relative :mantle)
                     (imp:path:current:file))
    '("Could not find `npm' executable. Is it installed? "
      "JavaScript config wants it.")))


(unless (executable-find "yarn")
  (nub:warning
      :innit
      (imp:path:join (imp:path:current:dir/relative :mantle)
                     (imp:path:current:file))
    '("Could not find `yarn' executable. Is it installed? "
      "JavaScript config wants it.")))


;;------------------------------------------------------------------------------
;; Projects
;;------------------------------------------------------------------------------

(imp:eval:after projectile
  ;; 'package.json' is in the root of a JavaScript project that uses NPM.
  (unless (seq-contains-p projectile-project-root-files "package.json" #'string=)
    (push "package.json" projectile-project-root-files))

  ;; Ignore these dirs; nothing useful and can be giant.
  (unless (seq-contains-p projectile-globally-ignored-directories "^node_modules$" #'string=)
    (push "^node_modules$" projectile-globally-ignored-directories))
  (unless (seq-contains-p projectile-globally-ignored-directories "^flow-typed$" #'string=)
    (push "^flow-typed$" projectile-globally-ignored-directories)))


;;------------------------------------------------------------------------------
;; JavaScript
;;------------------------------------------------------------------------------

(imp:use-package js
  :ensure nil ; This is an Emacs built-in feature.

  :mode "\\.[mc]?js\\'"
  :mode "\\.es6\\'"
  :mode "\\.pac\\'"

  :interpreter "node"

  ;;------------------------------
  :init
  ;;------------------------------
  ;; Parse node stack traces in the compilation buffer.
  (imp:eval:after compile ; NOTE: Doom had this as `compilation', but it's `compile'... right?
    (add-to-list 'compilation-error-regexp-alist 'node)
    (add-to-list 'compilation-error-regexp-alist-alist
                 '(node "^[[:blank:]]*at \\(.*(\\|\\)\\(.+?\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\)"
                        2 3 4)))


  (innit:hook:defun
     (:name   'javascript:settings
      :file   macro<imp>:path/file
      :docstr "Settings for Javascript mode. Non-LSP stuff.")

   ;; Nothing currently, I guess?

   ;; Separate camel-case into separate words?
   ;; (subword-mode t)
   )


  ;;------------------------------
  :hook
  ;;------------------------------
  ((js-mode-hook . mantle:hook:javascript:settings)
   (js-mode-hook . rainbow-delimiters-mode))

  ;;------------------------------
  :custom
  ;;------------------------------

  ;; Indent multi-line chained calls:
  ;; let example = foo.function()
  ;;                  .indentedFunction()
  ;;                  .getExampleValue();
  (js-chain-indent t)

  ;; NOTE: See the rest (`js-.*-indent-level' variables) for more tweakability.
  (js-indent-level (jerky:get 'code 'tab 'standard)))


;;------------------------------------------------------------------------------
;; Node / NPM
;;------------------------------------------------------------------------------

(imp:use-package npm-mode
  ;;------------------------------
  :hook
  ;;------------------------------
  ((js-mode typescript-mode) . npm-mode)

  ;;------------------------------
  :general
  ;;------------------------------
  (:prefix  (keybind:prefix :local)
   :states  keybind:leader/local:states
   :keymaps (list 'npm-mode-keymap keybind:leader/local:keymaps)
   ;; TODO: ...Does this work? Binding a whole keymap to a key?
   "n" npm-mode-command-keymap))


;; TODO: A Node.js REPL? https://github.com/abicky/nodejs-repl.el


;;------------------------------------------------------------------------------
;; TypeScript
;;------------------------------------------------------------------------------

(imp:use-package typescript-mode
  ;;------------------------------
  :init
  ;;------------------------------

  ;; NOTE: From Doom; add if need (better?) TSX file support.
  ;; ;; REVIEW We associate TSX files with `typescript-tsx-mode' derived from
  ;; ;;        `web-mode' because `typescript-mode' does not officially support
  ;; ;;        JSX/TSX. See emacs-typescript/typescript.el#4
  ;; (when (featurep! :lang web)
  ;;   (autoload 'typescript-tsx-mode "typescript-mode" nil t))
  ;;
  ;; ;; REVIEW We associate TSX files with `typescript-tsx-mode' derived from
  ;; ;;        `web-mode' because `typescript-mode' does not officially support
  ;; ;;        JSX/TSX. See emacs-typescript/typescript.el#4
  ;; (add-to-list 'auto-mode-alist
  ;;              (cons "\\.tsx\\'"
  ;;                    (if (featurep! :lang web)
  ;;                        #'typescript-tsx-mode
  ;;                      #'typescript-mode)))

  ;; TODO-lsp: This from Doom?
  ;; (imp:eval:after flycheck
  ;;   (flycheck-add-mode 'javascript-eslint 'web-mode)
  ;;   (flycheck-add-mode 'javascript-eslint 'typescript-mode)
  ;;   (flycheck-add-mode 'javascript-eslint 'typescript-tsx-mode)
  ;;   (flycheck-add-mode 'typescript-tslint 'typescript-tsx-mode)
  ;;
  ;;   (add-hook! 'typescript-tsx-mode-hook
  ;;     (defun +javascript-disable-tide-checkers-h ()
  ;;       (pushnew! flycheck-disabled-checkers
  ;;                 'javascript-jshint
  ;;                 'tsx-tide
  ;;                 'jsx-tide))))

  (innit:hook:defun
      (:name   'typescript:settings
       :file   macro<imp>:path/file
       :docstr "Settings for Typescript mode. Non-LSP stuff.")

     ;; 'wide' is a decent default, probably?
     (setq fill-column (jerky:get 'fill-column 'wide))

     (setq tab-width (jerky:get 'code 'tab 'standard))

    ;; Separate camel-case into separate words?
    ;; (subword-mode t)

     ;; NOTE: Delete this if the line brakes act fucky.
     (setq comment-line-break-function #'js2-line-break)

     ;; Most projects use either eslint, prettier, .editorconfig, or tsf in order
     ;; to specify indent level and formatting. In the event that no
     ;; project-level config is specified (very rarely these days), the community
     ;; default is 2, not 4. However, respect what is in tsfmt.json if it is
     ;; present in the project
     (setq typescript-indent-level (or (and (bound-and-true-p tide-mode)
                                      (plist-get (tide-tsfmt-options) :indentSize))
                                 typescript-indent-level))

     ;; Doom Fix #5556: expand .x to className="x" instead of class="x", if
    ;; `emmet-mode' is used.
    (setq emmet-expand-jsx-className? t))


  ;;------------------------------
  :hook
  ;;------------------------------
  ((typescript-mode-hook . mantle:hook:typescript:settings)
   (typescript-mode-hook . rainbow-delimiters-mode)
   (typescript-tsx-mode-hook . rainbow-delimiters-mode))


  ;;------------------------------
  :config
  ;;------------------------------

  ;; NOTE: From Doom; add if need TSX file support.
  ;; (when (fboundp 'web-mode)
  ;;   (define-derived-mode typescript-tsx-mode web-mode "TypeScript-TSX")
  ;;   (when (featurep! +lsp)
  ;;     (after! lsp-mode
  ;;       (add-to-list 'lsp--formatting-indent-alist '(typescript-tsx-mode . typescript-indent-level)))))

    ;; HACK Fixes comment continuation on newline
  (autoload 'js2-line-break "js2-mode" nil t))


;;------------------------------------------------------------------------------
;; Other Packages?
;;------------------------------------------------------------------------------

;; Skewer: Live web development with Emacs
;;   - https://github.com/skeeto/skewer-mode
;; "Provides live interaction with JavaScript, CSS, and HTML in a web browser.
;; Expressions are sent on-the-fly from an editing buffer to be evaluated in
;; the browser, just like Emacs does with an inferior Lisp process in Lisp
;; modes."


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'config 'user 'dev-env 'languages 'javascript)

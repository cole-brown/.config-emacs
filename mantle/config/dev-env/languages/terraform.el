;;; mantle/config/dev-env/terraform.el --- Hardware as Code? -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2023-01-03
;; Modified:   2023-01-03
;; URL:        https://github.com/cole-brown/.config-emacs
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Hardware as Code?
;;
;; ...Except the code can't do a whole lot of code things.
;; Hardware as a markup language? Not really?..
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Syntax Highlighting
;;------------------------------------------------------------------------------

;; https://github.com/emacsorphanage/terraform-mode
(imp:use-package terraform-mode

  ;;------------------------------
  :init
  ;;------------------------------

  (defvar mantle:terraform:runner
    (if (executable-find "terragrunt")
        "terragrunt"
      "terraform")
  "The default runner - `terraform' or `terragrunt'")

  ;; Lodge a complaint if 'terraform' isn't installed on the system. But don't skip
  ;; the `use-package', since it only needs the exe for the compile stuff.
  (unless (executable-find mantle:terraform:runner)
    (nub:warning
        :innit
        (imp:path:join (imp:path:current:dir/relative :mantle)
                       (imp:path:current:file))
      '("Could not find 'ripgrep' (`rg') executable. Is it installed? "
        "`magit-todos' wants it.")))

  (innit:hook:defun
      (:name   'terraform:settings
       :file   macro<imp>:path/file
       :docstr "Settings for Terraform mode. Non-LSP stuff.")
    (setq compile-command mantle:terraform:runner))


  ;;------------------------------
  :hook
  ;;------------------------------
  (terraform-mode-hook . mantle:hook:json:settings))


;;------------------------------
;; Keybinds : Meow
;;------------------------------

;; (imp:use-package terraform-mode
;;   :when  (imp:flag? :keybinds +meow)
;;   :after meow

;;   ;;------------------------------
;;   :config
;;   ;;------------------------------
;;   (mantle:meow:leader/local:keys 'terraform-mode-map
;;                                  "a" ; terraform apply
;;                                  (elisp:cmd (compile (format "%s apply" mantle:terraform:runner) t))
;;                                  "i" ; terraform init
;;                                  (elisp:cmd (compile (format "%s init" mantle:terraform:runner)))
;;                                  "p" ; terraform plan
;;                                  (elisp:cmd (compile (format "%s plan" mantle:terraform:runner)))))


;;------------------------------
;; Keybinds : Evil
;;------------------------------

(imp:use-package terraform-mode
  :when  (imp:flag? :keybinds +evil)
  :after (:and evil evil-collection)

  ;;------------------------------
  :general ; evil
  ;;------------------------------
  (keybind:leader/local:def
    :map terraform-mode-map
    "a" (list (elisp:cmd (compile (format "%s apply" mantle:terraform:runner) t)) :which-key "apply")
    "i" (list (elisp:cmd (compile (format "%s init" mantle:terraform:runner)))    :which-key "init")
    "p" (list (elisp:cmd (compile (format "%s plan" mantle:terraform:runner)))    :which-key "plan")))


;; ;;------------------------------------------------------------------------------
;; ;; TODO: Documentation?
;; ;;------------------------------------------------------------------------------
;;
;; TODO: This package for docs? Or just LSP? Or what does this vs that do?
;; https://github.com/TxGVNN/terraform-doc


;; ;;------------------------------------------------------------------------------
;; ;; Completion
;; ;;------------------------------------------------------------------------------
;;
;; https://github.com/rafalcieslak/emacs-company-terraform
;; (imp:use-package company-terraform
;;   :when (modulep! :completion company)
;;   :after terraform-mode
;;   :config
;;   (set-company-backend! 'terraform-mode 'company-terraform))


;; ;;------------------------------------------------------------------------------
;; ;; Language Server
;; ;;------------------------------------------------------------------------------
;;
;; https://github.com/juliosueiras/terraform-lsp/blob/master/docs/editors/emacs.md
;; TODO-lsp: terraform lsp?
;; Doom does this, but just use what the 'emacs.md' doc says instead?
;; (when (modulep! +lsp)
;;   (add-hook 'terraform-mode-local-vars-hook #'lsp! 'append))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'config 'user 'dev-env 'languages 'terraform)

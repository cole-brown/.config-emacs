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


;;--------------------------------------------------------------------------------
;; Keybinds : Meow
;;--------------------------------------------------------------------------------

(imp:use-package terraform-mode
  :when  (imp:flag? :keybinds +meow)
  :after meow

  ;;------------------------------
  :config
  ;;------------------------------

  ;;------------------------------
  ;; `General'
  ;;------------------------------

  (defun mantle:meow/keybind/general:terraform ()
    "Create the `terraform-mode' keybinds in `general' for `meow'."
    (keybind:meow:leader/local:bind-keys
        'terraform-mode-map
      "a" (list (elisp:cmd (compile (format "%s apply" mantle:terraform:runner) t)) :which-key (format "%s apply" mantle:terraform:runner))
      "i" (list (elisp:cmd (compile (format "%s init" mantle:terraform:runner)))    :which-key (format "%s init" mantle:terraform:runner))
      "p" (list (elisp:cmd (compile (format "%s plan" mantle:terraform:runner)))    :which-key (format "%s plan" mantle:terraform:runner))))


  ;;------------------------------
  ;; `Transient'
  ;;------------------------------

  (defun mantle:meow/keybind/transient:terraform ()
    "Create the `terraform-mode' keybinds in `transient' for `meow'."
    (transient-define-suffix mantle:meow/transient:terraform/compile/apply ()
      "Terraform/Terragrunt 'apply' command."
      :key "a"
      :description (format "%s apply" mantle:terraform:runner)
      (interactive)
      (compile (format "%s apply" mantle:terraform:runner) t))


    (transient-define-suffix mantle:meow/transient:terraform/compile/fmt ()
      "Terraform/Terragrunt 'fmt' command."
      :key "f"
      :description (format "%s fmt" mantle:terraform:runner)
      (interactive)
      (compile (format "%s fmt" mantle:terraform:runner) t))


    (transient-define-suffix mantle:meow/transient:terraform/compile/init ()
      "Terraform/Terragrunt 'init' command."
      :key "i"
      :description (format "%s init --backend-config='_backend.tfvars'" mantle:terraform:runner)
      (interactive)
      (compile (format "%s init --backend-config='_backend.tfvars'" mantle:terraform:runner) t))


    (transient-define-suffix mantle:meow/transient:terraform/compile/plan ()
      "Terraform/Terragrunt 'plan' command."
      :key "p"
      :description (format "%s plan" mantle:terraform:runner)
      (interactive)
      (compile (format "%s plan" mantle:terraform:runner) t))


    (transient-define-prefix mantle:meow/transient:terraform/compile ()
      "Compile-ish commands for terraform."
      ["Compile..."
       (mantle:meow/transient:terraform/compile/apply)
       (mantle:meow/transient:terraform/compile/fmt)
       (mantle:meow/transient:terraform/compile/init)
       (mantle:meow/transient:terraform/compile/plan)])
    ;; (mantle:meow/transient:terraform/compile)

    (mantle:meow:leader/local:key terraform-mode-map
                                  mantle:meow/key:local/compile
                                  #'mantle:meow/transient:terraform/compile))


  ;;------------------------------
  ;; Actually Create Keybinds:
  ;;------------------------------

  (if (imp:provided? :keybinds 'user 'general 'meow)
      (mantle:meow/keybind/general:terraform)
    (mantle:meow/keybind/transient:terraform)))



;;--------------------------------------------------------------------------------
;; Keybinds : Evil
;;--------------------------------------------------------------------------------

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
;; NOTE: Do not currently use `company'.
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

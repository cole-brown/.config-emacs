;;; mantle/config/dev-env/terraform.el --- Hardware as Code? -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2023-01-03
;; Timestamp:  2023-07-10
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
;; No, no. Not Verilog or VHDL...
;;
;; Hardware like entire computers and switches and stuff.
;; Like... The Cloud.
;;
;;; Code:


(imp:require :path)


;;------------------------------------------------------------------------------
;; Sanity Check
;;------------------------------------------------------------------------------

;; Lodge a complaint if 'terraform' isn't installed on the system. But don't
;; skip the `terraform-mode' `use-package', since it only needs the exe for the
;; compile stuff.
(unless (executable-find "terraform")
  (nub:warning
      :innit
      (imp:path:join (imp:path:current:dir/relative :mantle)
                     (imp:path:current:file))
    '("Could not find 'terraform' executable. Is it installed? "
      "My helpers around `terraform-mode' want it (`mantle:terraform:runner'). "
      "NOTE: `terragrunt' could be needed too, depending on if you need it or not...")))


;;------------------------------------------------------------------------------
;; Syntax Highlighting
;;------------------------------------------------------------------------------

;; https://github.com/emacsorphanage/terraform-mode
(imp:use-package terraform-mode

  ;;------------------------------
  :init
  ;;------------------------------

  (defun mantle:terraform:runner (&optional path)
    "Figure out what should run the terraform commands based on PATH.

PATH should be:
  - string - an absolute path to the terraform config directory
  - nil    - `default-directory' for `current-buffer'

Return a string:
  - \"terraform\"
  - \"terragrunt\""
    (let ((func-name "mantle:terraform:runner")
          (path/input path)
          (path (or path
                    default-directory)))

      ;;------------------------------
      ;; Error Checks
      ;;------------------------------

      ;; NOTE:
      ;;   a) `path' should no longer be null.
      ;;   b) Use `path/input' for feedback to user.
      (cond ((null path)
             (error "%s: `path' is unexpectedly null..? Supplied PATH: %S => `path': %S"
                    func/name
                    path/input
                    path))
            ((not (stringp path))
             (error "%s: PATH must be a string or nil. Supplied PATH: %S => `path' of %S: %S"
                    func/name
                    path/input
                    (type-of path)
                    path))
            ((not (path:exists? path))
             (error "%s: `path' does not exist; cannot determine a Terraform command runner: '%s'"
                    func-name
                    path)))

      ;;------------------------------
      ;; Look for Canary
      ;;------------------------------
      ;; Look in this directory and above for canary file 'terragrunt.hcl',
      ;; which indicates we should use `terragrunt'. Otherwise use `terraform'.
      (if (file:find:in-ancestors path ; Start here...
                                  "terragrunt.hcl"
                                  (path:project:root path)) ; Quit at project root if there is one.
          "terragrunt"
        "terraform")))
  ;; (mantle:terraform:runner)


  (defun mantle:terraform:command (command &rest args)
    "Get the full string to use to invoke `terragrunt' or `terraform', depending."
    (interactive)
    (format "%s %s%s"
            (mantle:terraform:runner) ; Who runs this?
            command
            (if args
                (concat " "
                        (string-join args " "))
              "")))
  ;; (mantle:terraform:command "apply")
  ;; (mantle:terraform:command "apply" "--hello=there")


  (defun mantle:terraform:run (command &rest args)
    "Run a Terraform command using either `terragrunt' or `terraform', depending."
    (interactive)
    (compile (mantle:terraform:command command args) ; What's the full command to run?
             ;; Run in Comint mode?
             ;; TODO: Dunno why only "apply" runs in comint mode.
             (string= command "apply")))


  (innit:hook:defun
      (:name   'terraform:settings
       :docstr "Settings for Terraform mode. Non-LSP stuff.")
    (setq compile-command (mantle:terraform:runner)))


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
      "a" (list (elisp:cmd (mantle:terraform:run "apply")) :which-key "terraform apply") ; (mantle:terraform:command "apply"))
      "i" (list (elisp:cmd (mantle:terraform:run "init"))  :which-key "terraform init")  ; (mantle:terraform:command "init")))
      "I" (list (elisp:cmd (mantle:terraform:run "init" "--backend-config='_backend.tfvars'"))
                :which-key "terraform init --backend-config='_backend.tfvars'")         ; (mantle:terraform:command "init" "--backend-config='_backend.tfvars'"))
      "p" (list (elisp:cmd (mantle:terraform:run "plan"))  :which-key "terraform plan") ; (mantle:terraform:command "plan"))
      "f" (list (elisp:cmd (mantle:terraform:run "fmt"))   :which-key "terraform fmt")  ; (mantle:terraform:command "fmt"))
      ;; TODO: "console" command in... eshell or something?
      ))


  ;;------------------------------
  ;; `Transient'
  ;;------------------------------

  (defun mantle:meow/keybind/transient:terraform ()
    "Create the `terraform-mode' keybinds in `transient' for `meow'."
    (transient-define-suffix mantle:meow/transient:terraform/compile/apply ()
      "Terraform/Terragrunt 'apply' command."
      :key "a"
      :description (mantle:terraform:command "apply")
      (interactive)
      (mantle:terraform:run "apply"))


    (transient-define-suffix mantle:meow/transient:terraform/compile/fmt ()
      "Terraform/Terragrunt 'fmt' command."
      :key "f"
      :description (mantle:terraform:command "fmt")
      (interactive)
      (mantle:terraform:run "fmt"))


    (transient-define-suffix mantle:meow/transient:terraform/compile/init ()
      "Terraform/Terragrunt 'init' command."
      :key "i"
      :description (mantle:terraform:command "init" "--backend-config='_backend.tfvars'")
      (interactive)
      (mantle:terraform:run "init" "--backend-config='_backend.tfvars'"))


    (transient-define-suffix mantle:meow/transient:terraform/compile/plan ()
      "Terraform/Terragrunt 'plan' command."
      :key "p"
      :description (mantle:terraform:run "plan" mantle:terraform:runner)
      (interactive)
      (compile (mantle:terraform:run "plan" mantle:terraform:runner) t))


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

  (if (imp:provided? :keybinds 'general 'meow)
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
    "a" (list (elisp:cmd (mantle:terraform:run "apply")) :which-key "terraform apply")  ; (mantle:terraform:command "apply"))
    "i" (list (elisp:cmd (mantle:terraform:run "init"))  :which-key "terraform init")   ; (mantle:terraform:command "init"))
    "p" (list (elisp:cmd (mantle:terraform:run "plan"))  :which-key "terraform plan"))) ; (mantle:terraform:command "plan"))))


;;------------------------------------------------------------------------------
;; Documentation
;;------------------------------------------------------------------------------

;; Don't bother unless `terraform` is actually installed?
(when (executable-find "terraform")

  ;; https://github.com/TxGVNN/terraform-doc
  (imp:use-package terraform-doc)

  ;;------------------------------
  ;; Keybinds : Meow
  ;;------------------------------

  (imp:use-package terraform-doc
    :when  (imp:flag? :keybinds +meow)
    :after (:and terraform-mode meow)

    ;;------------------------------
    :config
    ;;------------------------------

    (keybind:meow:leader/local:bind-keys
        'terraform-mode-map
      "d" (list #'terraform-doc :which-key "Docs..."))))


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
;;
;; ;; Don't bother unless `terraform` is actually installed?
;; (when (executable-find "terraform")

;;   ;; https://github.com/TxGVNN/terraform-doc
;;   (imp:use-package terraform-doc)


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'config 'dev-env 'languages 'terraform)

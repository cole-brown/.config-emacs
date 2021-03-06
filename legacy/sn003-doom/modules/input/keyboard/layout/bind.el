;;; input/keyboard/registration.el -*- lexical-binding: t; -*-


;;                                 ──────────                                 ;;
;; ╔════════════════════════════════════════════════════════════════════════╗ ;;
;; ║                       Register to be The Layout.                       ║ ;;
;; ╚════════════════════════════════════════════════════════════════════════╝ ;;
;;                                   ──────                                   ;;
;;                           There can be only one.                           ;;
;;                                 ──────────                                 ;;


(imp:require :input 'keyboard 'output)
(imp:require :input 'keyboard 'debug)
(imp:require :input 'keyboard 'registrars)
(imp:require :input 'keyboard 'vars)


;;------------------------------------------------------------------------------
;; Unbinding
;;------------------------------------------------------------------------------

(defun int<keyboard>:layout:unbind (registrar layout type unbind-map)
  "Saves TYPE's UNBIND-MAP for final configuration in
`int<keyboard>:layout:activate'.

REGISTRAR should be a keyword from `int<keyboard>:registrars'.

TYPE should be one of:
  :common - Any keybinds that exist in both evil-mode and standard Emacs.
  :emacs  - Any Emacs-only keybinds (non-evil-mode).
  :evil   - Any Evil-only keybinds.

If called twice with the same TYPE, the later UNBIND-MAP will overwrite the
earlier.

Unbindings are applied before bindings."
  (let ((func/name "int<keyboard>:layout:unbind")
        (debug/tags '(:registering :init)))
    (int<keyboard>:debug:func
     func/name
     debug/tags
     :start
     (list (cons 'registrar registrar)
           (cons 'layout layout)
           (cons 'type type)
           (cons 'unbind-map unbind-map)))

    (when (not (int<keyboard>:layout:valid? layout))
      (int<keyboard>:output :error
                            func/name
                            '("`layout' must be a keyword. "
                              "Got: %S")
                            layout))

    (when (not (int<keyboard>:layout:type/valid? type))
      (int<keyboard>:output :error
                            func/name
                            '("Type '%S' is not a valid type. "
                              "Must be one of: %S")
                            type int<keyboard>:layout:types))

    ;; Set to `:init' state unless we're in some finalized state. If we're in a finalized state,
    ;; just let the keymap be updated for possibly an `keyboard:layout:apply' or something.
    ;;
    ;; This will error out for us if we're currently in the wrong state to transition to `:init'.
    (int<keyboard>:registration:state/transition:set registrar :init)

    ;; Ok - errors checked; set it.
    (setq int<keyboard>:layout:active layout)
    (prog1
        (int<keyboard>:alist:update type
                                    unbind-map
                                    (int<keyboard>:registrar:symbol registrar :unbinds))
      (int<keyboard>:debug:func
       func/name
       debug/tags
       :end))))
;; (int<keyboard>:registrar:symbol :debug :unbinds)
;; int<keyboard>:registrar<debug>:unbinds


;;------------------------------
;; API
;;------------------------------

(defun keyboard:layout:unbind (layout type unbind-map)
  "Saves TYPE's UNBIND-MAP for final configuration in
`int<keyboard>:layout:activate'.

TYPE should be one of:
  :common - Any keybinds that exist in both evil-mode and standard Emacs.
  :emacs  - Any Emacs-only keybinds (non-evil-mode).
  :evil   - Any Evil-only keybinds.

If called twice with the same TYPE, the later UNBIND-MAP will overwrite the
earlier.

Unbindings are applied before bindings."
  (declare (indent 2))
  (int<keyboard>:layout:unbind :actual layout type unbind-map))
;; (pp-macroexpand-expression (int<keyboard>:registrar:get registrar :unbinds))
;; (pp-macroexpand-expression (int<keyboard>:registrar:get registrar :keybinds))


;;------------------------------------------------------------------------------
;; Functions: Binding
;;------------------------------------------------------------------------------

(defun int<keyboard>:layout:bind (registrar layout type keybind-map)
  "Saves TYPE's KEYBIND-MAP for final configuration in
`int<keyboard>:layout:activate'.

REGISTRAR should be a keyword from `int<keyboard>:registrars'.

TYPE should be one of:
  :common - Any keybinds that exist in both evil-mode and standard Emacs.
  :emacs  - Any Emacs-only keybinds (non-evil-mode).
  :evil   - Any Evil-only keybinds.

If called twice with the same TYPE, the later KEYBIND-MAP will overwrite the
earlier."
  (let ((func/name "int<keyboard>:layout:bind")
        (debug/tags '(:registering :init)))
    (int<keyboard>:debug:func
     func/name
     debug/tags
     :start
     (list (cons 'registrar registrar)
           (cons 'layout layout)
           (cons 'type type)
           (cons 'keybind-map keybind-map)))

    (when (not (int<keyboard>:layout:valid? layout))
      (int<keyboard>:output :error
                            func/name
                            '("`layout' must be a keyword. "
                              "Got: %S")
                            layout))

    (when (not (int<keyboard>:layout:type/valid? type))
      (int<keyboard>:output :error
                            func/name
                            '("Type '%S' is not a valid type. "
                              "Must be one of: %S")
                            type int<keyboard>:layout:types))

    ;; Set to `:init' state unless we're in some finalized state. If we're in a finalized state,
    ;; just let the keymap be updated for possibly an `keyboard:layout:apply' or something.
    ;;
    ;; This will error out for us if we're currently in the wrong state to transition to `:init'.
    (int<keyboard>:registration:state/transition:set registrar :init)

    ;; Ok - errors checked; set it.
    (setq int<keyboard>:layout:active layout)
    ;; Get the symbol name of the variable that stores these keybinds so we can use the alist helper macros to update it.
    (prog1
        (int<keyboard>:alist:update type
                                    keybind-map
                                    (int<keyboard>:registrar:symbol registrar :keybinds))
      (int<keyboard>:debug:func
       func/name
       debug/tags
       :end))))


;;------------------------------
;; API
;;------------------------------

(defun keyboard:layout:bind (layout type keybind-map)
  "Saves TYPE's KEYBIND-MAP for final configuration in
`int<keyboard>:layout:activate'.

TYPE should be one of:
  :common - Any keybinds that exist in both evil-mode and standard Emacs.
  :emacs  - Any Emacs-only keybinds (non-evil-mode).
  :evil   - Any Evil-only keybinds.

If called twice with the same TYPE, the later KEYBIND-MAP will overwrite the
earlier."
  (declare (indent 2))
  (int<keyboard>:layout:bind :actual layout type keybind-map))
;; (pp-macroexpand-expression (int<keyboard>:registrar:get registrar :keybinds))


;;------------------------------------------------------------------------------
;; Configuration
;;------------------------------------------------------------------------------

(defun int<keyboard>:layout:config (registrar bind/unbind layout)
  "Verifies that LAYOUT is still valid and is ready for finalization.

REGISTRAR should be a keyword from `int<keyboard>:registrars'.

BIND/UNBIND should be a valid keyword in `int<keyboard>:registration:action'.

LAYOUT should be a valid keyboard layout keyword."
  (let ((func/name "int<keyboard>:layout:config")
        (debug/tags '(:registering :config)))
    (int<keyboard>:debug:func
     func/name
     debug/tags
     :start
     (list (cons 'registrar registrar)
           (cons 'bind/unbind bind/unbind)
           (cons 'layout layout)))

    ;;---
    ;; Can use finalization's check here w/ known-good type.
    ;;---
    (int<keyboard>:activate/validate func/name
                                     registrar
                                     bind/unbind
                                     :common
                                     :config)

    ;;---
    ;; Also verify the `layout'.
    ;;---
    (unless (int<keyboard>:layout:valid? layout :active)
      (int<keyboard>:output :error
                            func/name
                            '("`layout' must be a keyword. "
                              "Got: %S")
                            layout))

    ;; This will error out for us.
    (int<keyboard>:registration:state/transition:set registrar :config)

    ;;------------------------------
    ;; Configuration
    ;;------------------------------
    ;; Nothing to do, currently.
    ;;
    ;; Only steps that need to happen after `keyboard:layout:bind' is the
    ;; `int<keyboard>:layout:activate', which happens in Finalization.
    ;;
    ;; NOTE: If we get something to do: obey `bind/unbind'!

    (int<keyboard>:debug:func
     func/name
     debug/tags
     :end
     nil)))


;;------------------------------
;; API
;;------------------------------

(defun keyboard:layout:config (bind/unbind layout)
  "Verifies that LAYOUT is still valid and is ready for finalization.

BIND/UNBIND should be a valid keyword in `int<keyboard>:registration:action'.

LAYOUT should be a valid keyboard layout keyword."
  (let ((func/name "keyboard:layout:config")
        (debug/tags '(:registering :config)))
    (int<keyboard>:debug:func
     func/name
     debug/tags
     :start
     (list (cons 'bind/unbind bind/unbind)
           (cons 'layout layout)))

    (int<keyboard>:layout:config :actual bind/unbind layout)

    (int<keyboard>:debug:func
     func/name
     debug/tags
     :end
     nil)))



;;------------------------------------------------------------------------------
;; Activation
;;------------------------------------------------------------------------------

;; Also used in config step, currently, but could make that have its own checks
;; if this needs to change to be specific to finalization.
(defun int<keyboard>:activate/validate (caller registrar bind/unbind type registering)
  "Checks that things are valid for `int<keyboard>:layout:activate'.

1. There must be an active layout.
2. Some keybinds must be set/saved.
3. BIND/UNBIND must be valid
4. TYPE must be valid.
5. current registration state must be able to transition to REGISTERING.

CALLER should be the function name of the caller.

BIND/UNBIND should be a valid keyword in `int<keyboard>:registration:action'.

TYPE should be one of the keywords from `int<keyboard>:layout:types'.

REGISTERING should be a registering state (see `int<keyboard>:registration:states')."
  (cond ((not (int<keyboard>:registration:valid/action? bind/unbind))
         ;; Should have already signaled an error if invalid, but to be extra cautious:
         (int<keyboard>:output :error
                               caller
                               '("Invalid bind/unbind keyword `%S'.")
                               bind/unbind))

        ((not (int<keyboard>:registration:state/transition:valid? registrar registering))
         ;; valid? will signal error, so no need to do it again.
         (int<keyboard>:output :error
                               caller
                               '("Cannot transition registering state %S -> %S.")
                               (int<keyboard>:registrar:get registrar :state)
                               registering))

        ((null int<keyboard>:layout:active)
         (int<keyboard>:output :error
                               caller
                               '("No active layout set; cannot configure keyboard layout! "
                                 "desired: '%S', "
                                 "active:  '%S'")
                               int<keyboard>:layout:desired
                               int<keyboard>:layout:active))

        ;; If we're binding, we need keys to bind.
        ;;   - If we're unbinding, though, we are ok with not having anything to unbind.
        ((and (memq bind/unbind '(:bind :full))
              (null (int<keyboard>:registrar:get registrar :keybinds)))
         (int<keyboard>:output :error
                               caller
                               '("Active layout has not set its keybinds; "
                                 "cannot configure keyboard layout! "
                                 "Expected %S to have called `keyboard:layout:bind'."
                                 "Keybinds are: %S")
                               int<keyboard>:layout:active
                               (int<keyboard>:registrar:get registrar :keybinds)))

        ((not (int<keyboard>:layout:type/valid? type))
         (int<keyboard>:output :error
                               caller
                               '("Type '%S' is not a valid type. "
                                 "Must be one of: %S")
                               type int<keyboard>:layout:types))

        (t
         ;; No errors - return something non-nil.
         type)))


(defun int<keyboard>:layout:activate (registrar bind/unbind types &optional no-eval)
  "Map the active keyboard layout's keybinds for each in supplied TYPE list.

BIND/UNBIND should be a valid keyword in `int<keyboard>:registration:action'.

TYPES should be a list of keywords from `int<keyboard>:layout:types'.

Return value will be either nil/non-nil (normally),
or `int<keyboard>:layout:map/process' output if NO-EVAL is non-nil."
  (let ((func/name "int<keyboard>:layout:activate")
        (debug/tags '(:registering :finalize))
        results)
    (int<keyboard>:debug:func
     func/name
     debug/tags
     :start
     (list (cons 'registrar registrar)
           (cons 'bind/unbind bind/unbind)
           (cons 'types types)
           (cons 'no-eval no-eval)))

    ;; Validate inputs.
    (int<keyboard>:registration:valid/action? bind/unbind)

    ;;------------------------------
    ;; First: Apply all TYPES unbindings.
    ;;------------------------------
    (when (memq bind/unbind '(:unbind :full))
      (dolist (type types)
        (let ((result/unbind (int<keyboard>:activate/type registrar
                                                          :unbind
                                                          type
                                                          no-eval
                                                          debug/tags)))
          (int<keyboard>:debug
              func/name
              debug/tags
            "unbind result:\n%S"
            result/unbind)
          (push result/unbind results)))

      ;; Did we succeed? How do we deal with `results'?
      (cond ((not results)
             (int<keyboard>:output :error
                                   func/name
                                   '("Failed to activate unbindings.")))
            ((not no-eval)
             ;; Don't need to save these. Clear out for binding.
             (setq results nil)))

      ;; Do not set registering state for unbindings.
      )

    ;;------------------------------
    ;; Second: Apply all TYPES keybindings.
    ;;------------------------------
    (when (memq bind/unbind '(:bind :full))
      ;; Activate each type.
      (dolist (type types)
        (let ((result/bind (int<keyboard>:activate/type registrar
                                                        :bind
                                                        type
                                                        no-eval
                                                        debug/tags)))
          (int<keyboard>:debug
              func/name
              debug/tags
            "bind result:\n%S"
            result/bind)
          (push result/bind results)))

      ;; Prep return value?
      (unless no-eval
        (setq results (seq-reduce (lambda (x y)
                                    "Returns t if /some/ keybinds were applied."
                                    (not (null (or x y))))
                                  results
                                  nil)))

      ;; If we have a non-nil return we're `:active'.
      ;;
      ;; This will error out if invalid transition.
      (int<keyboard>:registration:state/transition:set registrar
                                                       (if results
                                                           :active
                                                         :inactive)))

    ;;------------------------------
    ;; Done.
    ;;------------------------------
    ;; If only unbinding (no binds), and we got here, and we have a nil
    ;; results and nil NO-EVAL... it's ok; that's valid: change to `t' return.
    (if (and (not results)
             (null no-eval)
             (eq bind/unbind :unbind))
        (progn
          (int<keyboard>:debug:func
           func/name
           debug/tags
           :end/list
           (list
            (cons :title "'Only unbinding but still true' return:")
            (cons 'no-eval no-eval)
            (cons 'bind/unbind bind/unbind)
            (cons 'results results)
            results))

          t)

      (int<keyboard>:debug:func
       func/name
       debug/tags
       :end
       results)
      results)))


(defun int<keyboard>:activate/type (registrar bind/unbind type &optional no-eval debug/tags)
  "Map the BINDINGS for TYPE.

BIND/UNBIND should be either `:unbind', `:bind', or `:full'. It will be used to determine
whether to get:
  - :unbind -> `(int<keyboard>:registrar:get registrar :unbinds)'
  - :bind   -> `(int<keyboard>:registrar:get registrar :keybinds)'
  - :full   -> both of the above

TYPE should be one of the keywords from `int<keyboard>:layout:types'.

If NO-EVAL is non-nil, instead of mapping will return the code it would have used to map."
  (let ((valids:bind/unbind '(:bind :unbind))
        results)
    ;; We just apply the BINDINGS; what does `:full' mean in that context?
    (unless (memq bind/unbind valids:bind/unbind)
      (int<keyboard>:output :error
                            func
                            '("`bind/unbind' must be one of: %S")
                            valids:bind/unbind))

    (if-let ((valid (int<keyboard>:activate/validate "int<keyboard>:activate/type"
                                                     registrar bind/unbind type :active))
             ;; Could be this keybind has nothing for type, and that's fine...
             ;; It will error if there is nothing at all (e.g. layout never called `keyboard:layout:bind'.
             (keybinds (int<keyboard>:alist:get/value
                        type
                        (cond ((eq bind/unbind :bind)
                               (int<keyboard>:registrar:get registrar :keybinds))
                              ((eq bind/unbind :unbind)
                               (int<keyboard>:registrar:get registrar :unbinds))
                              (t
                               (int<keyboard>:output :error
                                                     func
                                                     "`bind/unbind' must be one of: %S"
                                                     '(:bind :unbind)))))))

        ;;------------------------------
        ;; Valid and have keybinds:
        ;;------------------------------
        (setq results
              ;; Return something non-nil.
              (or
               ;; Should we do any sanity checks before `int<keyboard>:layout:map/process' output is eval'd?
               (if no-eval
                   ;; Not applying the keybinds - returning the sexprs instead.
                   (progn
                     (int<keyboard>:debug
                         "int<keyboard>:activate/type"
                         debug/tags
                       "no-eval input for map-process: %S" keybinds)
                     (int<keyboard>:debug
                         "int<keyboard>:activate/type"
                         debug/tags
                       "no-eval: %S"
                       (int<keyboard>:layout:map/process registrar keybinds))
                     (setq results (int<keyboard>:layout:map/process registrar keybinds)))

                 ;; We are applying the keybinds.
                 (int<keyboard>:debug
                     "int<keyboard>:activate/type"
                     debug/tags
                   "eval: %S"
                   (int<keyboard>:layout:map/process registrar keybinds))
                 (eval
                  ;; This is the function that actually creates the keybinds for `keyboard:layout:map!'.
                  ;; It'll return a `progn' of 'general' function calls, and we'll evaluate it.
                  (int<keyboard>:layout:map/process registrar keybinds)))
               t))

      ;;------------------------------
      ;; Invalid or didn't find keybinds...
      ;;------------------------------
      (int<keyboard>:debug
          "int<keyboard>:activate/type"
          debug/tags
        (concat "No keybinds to activate for %S.\n"
                "  valid?    %S\n"
                "  keybinds: %S")
        type (not (null valid)) keybinds)
      ;; NOTE: Currently not an error as common/emacs may not have any keybinds.
      ;; Should probably return to being an error in the future?
      ;; (int<keyboard>:output :error
      ;;                       "int<keyboard>:activate/type"
      ;;                       '("Cannot activate keybinds.\n"
      ;;                         "  valid? %S\n"
      ;;                         "  keybinds: %S")
      ;;                       valid keybinds)

      ;; Return nil for invalid case.
      (setq results nil))

    (int<keyboard>:debug
        "int<keyboard>:activate/type"
        debug/tags
      "Returning for `no-eval': %S" no-eval)
    ;; Return t/nil normally, or the map-process output if `no-eval'.
    (if no-eval
        results
      (not (null results)))))


;;------------------------------
;; API
;;------------------------------

(defun keyboard:layout:finalize ()
  "Hook to run ONCE under `doom-init-modules-hook' for finalizing all
evil-mode keybinds. NOTE: ONLY CALL IF USING EVIL!"

  ;; Common first, then +Good+ Emacs vs Evil.
  (let ((types '(:common)))
    ;; Evil or Emacs? Only load one...
    (if (featurep! :editor evil)
        (push :evil types)
      (push :emacs types))
    ;; Activate the keybinds for types.
    (int<keyboard>:layout:activate :actual :full types)))


;;------------------------------------------------------------------------------
;; The End
;;------------------------------------------------------------------------------
(imp:provide :input 'keyboard 'layout 'bind)

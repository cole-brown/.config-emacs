;;; system/multiplexer/multiplex.el -*- lexical-binding: t; -*-


;;------------------Init & Config Help for Multiple Systems.--------------------
;;--                     What computer is this anyways?                       --
;;--------------------------(probably the wrong one)----------------------------

(require 'cl-lib) ;; cl-defun for '&keys'

(imp:require :nub)
(imp:require :jerky)
(imp:require :str)
(imp:require :path)
(imp:require :buffer)


;;------------------------------------------------------------------------------
;; Getter/Setter for Multiplexer keys in Jerky.
;;------------------------------------------------------------------------------

(defun int<system/multiplexer>:add (hash)
  "Add HASH to the list of system hashes if not already present."
  (let ((hashes (jerky:get 'system 'hashes)))
    (when (not (member hash hashes))
      (jerky:set 'system 'hashes
                 :value (cons hash hashes)
                 :docstr "List of all system hashes."))))


(defun int<system/multiplexer>:system/hashes ()
  "Get a list of all system hashes."
  (jerky:get 'system 'hashes))
;; (int<system/multiplexer>:system/hashes)


(defun system:multiplexer:set (&rest plist)
  "For setting a system's multiplex settings.

PLIST is a plist with keys:
  - REQUIRED:
    + `:hash'  - This system's hash string.
    + `:keys'  - A list of keys to use for storing setting
               - Will be prepended with `:system hash'.
    + `:value' - The value to store.
  - OPTIONAL:
    + `:docstr' - Documentation string."

  (let ((func/name "system:multiplexer:set")
        (hash   (plist-get plist :hash))
        (keys   (plist-get plist :keys))
        (value  (plist-get plist :value))
        (docstr (plist-get plist :docstr)))

    ;; Check for required plist keys:
    (cond ((not hash)
           (nub:error
               :system/multiplexer
               func/name
             "Must have a `:hash' in plist params: %S"
             plist))
          ((not value)
           (nub:error
               :system/multiplexer
               func/name
             "Must have a `:value' in plist params: %S"
             plist))

          ;; Special case: save value as this system's hash if `hash' and
          ;; `value' are equal and there are no `keys' or `docstr'.
          ((and hash
                value
                (not keys)
                (not docstr)
                (stringp hash)
                (stringp value)
                (string= hash value))
           (int<system/multiplexer>:add hash)
           (jerky:set 'system 'hash
                      :value value))

          ;; Check for required plist keys:
          ((not keys)
           (nub:error
               :system/multiplexer
               func/name
             "Must have a `:keys' in plist params: %S"
             plist))

          ;; Check validitiy:
          ((not (listp keys))
           (nub:error
               :system/multiplexer
               func/name
             "`:keys' must be a list; got: %S"
             keys))
          ((and docstr
                (not (stringp docstr)))
           (nub:error
               :system/multiplexer
               func/name
             "`:docstr' must be a string; got: %S"
             docstr))

          ;; Valid - save it.
          (t
           (int<system/multiplexer>:add hash)
           (if docstr
               (jerky:set 'system hash keys
                          :value value
                          :docstr docstr)
             (jerky:set 'system hash keys
                        :value value))))))
;; (system:multiplexer:set :hash "foo" :keys '(bar) :value 42 :docstr "hello there")
;; (system:multiplexer:set :hash "jeff" :value "jeff")
;; (jerky:get 'system 'hash)
;; (system:multiplexer:set :hash "5730ce-91e149" :value "5730ce-91e149")
;; (jerky:get 'system 'hash)


(defun system:multiplexer:get (&optional hash &rest keys)
  "For getting a system's multiplex settings.

HASH should be the system's hash used when saving the setting.
  - If both HASH and KEYS are nil, this will return the saved hash for
    /this/ system.
  - If HASH is nil, it will use /this/ system's hash.

KEYS will be prepended with `:system' and the HASH."
  (let ((func/name "system:multiplexer:get"))
    ;; Special case: get saved hash.
    (cond ((and (null hash)
                (null keys))
           (jerky:get 'system 'hash))

          ;; Check validitiy:
          ((not keys)
           (nub:error
               :system/multiplexer
               func/name
             "Must have `keys' in params; got: %S"
             keys))

          ;; Try to get it.
          (t
           (let ((hash/get (or hash
                               (jerky:get 'system 'hash))))
             ;; Check `hash/get' validity, finally.
             (if (not hash/get)
                 (nub:error
                     :system/multiplexer
                     func/name
                   '("No hash provided and could "
                     "not find a saved hash... "
                     "provided: %S, saved: %S")
                   hash
                   (jerky:get 'system 'hash))
               (jerky:get 'system hash/get keys)))))))
;; (system:multiplexer:get nil)
;; (system:multiplexer:get nil 'path 'secret 'emacs)
;; (system:multiplexer:get (jerky:get 'system 'hash) 'path 'secret 'emacs)
;; (system:multiplexer:get "foo" 'bar)
;; (jerky:/parse '(system "foo" (bar)) t)
;; (jerky:get 'system "foo" '(bar))


;;------------------------------------------------------------------------------
;; System UID
;;------------------------------------------------------------------------------

(defun system:multiplexer:hash/this ()
  "Return /this/ system's hash.

If a system hash has been saved, return the saved hash.

Else, generate a system hash from function `system-name' and
variable `system-type'."
  ;; TODO: Why does a system have a different hash at start of init compared
  ;; to after doom/emacs is running? Is it intentional? Cuz it's confusing to
  ;; me when adding a new system.
  ;; (message "system hash for system-name %S system-type %S: %S"
  ;;          (system-name) system-type
  ;;          (str:hash:pretty (list (system-name) system-type)))
  ;;
  ;; Band-aid solution: check if we've got a saved hash.
  (if-let ((saved-hash (system:multiplexer:get nil)))
      saved-hash
    (let ((hash (str:hash:pretty (list (system-name) system-type))))
      (system:multiplexer:set :hash hash :value hash)
      hash)))
;; (system:multiplexer:hash/this)
;; (str:hash:pretty (list (system-name) system-type))


(defun system:multiplexer:hash:uid (domain date name)
  "Generate a system UID from parameters.

DOMAIN, DATE, and NAME should be strings. They can be anything, but are named
after how my system names are (currently) created:
  - DOMAIN:
    - \"work\", \"home\", ...
  - DATE:
    - Just the year, usually.
      -  \"2020\", \"2022\", etc.
  - NAME:
    - Some unimaginative name like \"desk\", \"lap\", etc...

Return a system UID string from the specified DOMAIN, DATE and NAME, with
function `system-name' and variable `system-type' as additional information."
  (str:hash (list domain date name)
            (list (system-name) system-type)))
;; (system:multiplexer:hash:uid "jeff" "2020" "compy")


(defun system:multiplexer:path/rel (&optional unique-id)
  "Convert system's UNIQUE-ID to a safe (relative) directory path.

If UNIQUE-ID is nil, use this system's ID."
  (let ((unique-id (or unique-id
                       (system:multiplexer:get nil 'id))))
    (replace-regexp-in-string "::" "_"
                              (replace-regexp-in-string "/" "-"
                                                        unique-id))))
;; (system:multiplexer:path/rel)
;; (system:multiplexer:path/rel (system:multiplexer:hash:uid "jeff" "2020" "compy"))


(defun system:multiplexer:path/abs (root unique-id)
  "Convert system's UNIQUE-ID to a safe (absolute) directory path.

ROOT must be an absolute path string.

If UNIQUE-ID is nil, use this system's ID."
  (path:abs:dir root
                (system:multiplexer:path/rel unique-id)))
;; (system:multiplexer:path/abs "c:/foo" ":bar")
;; (system:multiplexer:path/abs "/foo" ":bar")


;;------------------------------------------------------------------------------
;; Define a System
;;------------------------------------------------------------------------------

(cl-defun system:multiplexer:define (&key hash
                                          domain
                                          date
                                          type
                                          description
                                          (path/secret/root "~/.config/secret")
                                          (path/secret/init "emacs/sn004")
                                          (debug nil))
  "Define a system based on the arguments plist.

HASH - /Must/ be a static string of a system's hash from
       `system:multiplexer:hash/this', not a return value from dynamically
       calling `system:multiplexer:hash/this'.
TODO: Why can't we call `system:multiplexer:hash/this' in the function call?

DOMAIN - A string/symbol of \"home\", \"work\", or other domain name.

DATE - A string/symbol of the year, or year-month-day.

TYPE - Another string/symbol for identifying the system.
     - \"desk\", \"desktop\", \"lap\", etc.

DOMAIN, DATE, and TYPE are combined with forward slashes to create the
human-readable part of the system's ID. The HASH is applied after these: id =
\"<domain>/<date>/<type>::<hash>\" e.g. \"home/2021/desk::12345-abcdef\" - See
`str:hash:recreate'

DESCRIPTION - A short string for the 'docstr' of the ID and the path.

PATH/SECRET/ROOT - Absolute path to the specified system's secret folder.

PATH/SECRET/INIT - Relative path from PATH/SECRET/ROOT to the Emacs init files
                   for this system. - NOTE: Can be the init for the specified
                   system, or for all systems, depending on how your secret's
                   init is itself set up.

DEBUG - If non-nil, just print out stuff instead of setting it into Jerky."
  (let ((func/name "system:multiplexer:define"))
    (nub:debug:func/start
        :system/multiplexer
        func/name
        '(:system :multiplex)
      (cons 'hash hash)
      (cons 'domain domain)
      (cons 'date date)
      (cons 'type type)
      (cons 'description description)
      (cons 'path/secret/root path/secret/root)
      (cons 'path/secret/init path/secret/init)
      (cons 'debug debug))

  ;;------------------------------
  ;; Error Checking
  ;;------------------------------
  (let (errors)
    (unless (stringp hash)
      (push (format "HASH must be a string. Got type %S: %S"
                    (type-of hash)
                    hash)
            errors))
    (unless (or (stringp domain)
                (symbolp domain))
      (push (format "DOMAIN must be a string or symbol. Got type: %S"
                    (type-of domain)
                    domain)
            errors))
    (unless (or (stringp date)
                (symbolp date))
      (push (format "DATE must be a string or symbol. Got type: %S"
                    (type-of date)
                    date)
            errors))
    (unless (or (stringp type)
                (symbolp type))
      (push (format "TYPE must be a string or symbol. Got type: %S"
                    (type-of type)
                    type)
            errors))
    (unless (stringp description)
      (push (format "DESCRIPTION must be a string. Got type %S: %S"
                    (type-of description)
                    description)
            errors))
    (unless (stringp path/secret/root)
      (push (format "PATH/SECRET/ROOT must be a string. Got type %S: %S"
                    (type-of path/secret/root)
                    path/secret/root)
            errors))
    (unless (stringp path/secret/init)
      (push (format "PATH/SECRET/INIT must be a string. Got type %S: %S"
                    (type-of path/secret/init)
                    path/secret/init)
            errors))

    (when errors
      (nub:error
          :system/multiplexer
          func/name
        (concat "Invalid parameter"
                (when (/= 1 (length errors))
                  "s")
                ":\n"
                (mapconcat #'identity
                           errors
                           "\n")))))

  (let* ((id (str:hash:recreate (list domain date type) hash))
         (path/secret/init.abs (path:abs:dir path/secret/root path/secret/init)))
    ;;------------------------------
    ;; Debug?
    ;;------------------------------
    (if debug
        (message (mapconcat #'identity
                            '("System:"
                              "  -> hash:          %S"
                              "  -> domain:        %S"
                              "  -> date:          %S"
                              "  -> type:          %S"
                              "  -> description:   %S"
                              "  -> path/root:     %S"
                              "  -> path/init:     %S"
                              "  <- id:            %S"
                              "  <- path/init.abs: %S")
                            "\n")
                 hash
                 domain
                 date
                 type
                 description
                 path/secret/root
                 path/secret/init
                 id
                 path/secret/init.abs)

      ;;------------------------------
      ;; Define the System.
      ;;------------------------------

      (system:multiplexer:set :hash hash
                      :keys (list 'path 'secret 'root)
                      :value path/secret/root
                      :docstr "Root for .secret.d")
      (system:multiplexer:set :hash hash
                      :keys (list 'path 'secret 'emacs)
                      :value path/secret/init.abs
                      :docstr "Root for Per-Computer Set-Up of Emacs")
      (system:multiplexer:set :hash hash
                      :keys (list 'id)
                      :value id
                      :docstr description)

      ;; Have to set path per-system since work comps have restrictions on where
      ;; things can be, and home comps tend to have a random number of hard drives
      ;; just wherever.
      (system:multiplexer:set :hash hash
                      :keys (list 'path 'secret 'system)
                      :value  (system:multiplexer:path/abs path/secret/init.abs id)
                      :docstr description))

    (nub:debug:func/end
        :system/multiplexer
        func/name
        '(:system :multiplex)
      (cons 'id id)
      (cons 'path path/secret/init.abs)))))
;; (system:multiplexer:define :debug t
;;                            :hash "123456-abcdef"
;;                            :domain "home"
;;                            :date "2021"
;;                            :type "desk"
;;                            :description "test description"
;;                            :path/secret/root "~/.config/secret"
;;                            :path/secret/init "emacs/sn004")


(defun int<system/multiplexer>:show (hash buffer)
  "Display system multiplexer info for system identified by HASH in BUFFER.

HASH should be a string from e.g. `system:multiplexer:hash/this'.

BUFFER should be a buffer or buffer name string."
  (with-current-buffer (get-buffer-create buffer)
    (goto-char (point-max))
    (let* ((current (string= hash (system:multiplexer:hash/this)))
           (id (system:multiplexer:get hash 'id))
           ;; Split ID up into domain, date, and type.
           (prefixes (nth 0 (if id
                                (str:hash:split id)
                              nil)))
           (domain   (nth 0 prefixes))
           (date     (nth 1 prefixes))
           (type     (nth 2 prefixes))
           (description (jerky:get 'system hash 'id
                                   :field :docstr))
           (path/root (system:multiplexer:get hash 'path 'secret 'root))
           (path/init (system:multiplexer:get hash 'path 'secret 'emacs)))
      (insert
       (format (mapconcat #'identity
                          '("\n"
                            "System %s:%s"
                            "    -> hash:          %s"
                            "    -> domain:        %s"
                            "    -> date:          %s"
                            "    -> type:          %s"
                            "    -> description:   %s"
                            "    -> path/root:     %s"
                            "    -> path/init:     %s")
                          "\n")
               id
               (if current
                   "\n  --> THIS SYSTEM! <--"
                 "")
               hash
               domain
               date
               type
               description
               path/root
               path/init)))))


(defun system:cmd:multiplexer:show (&optional hash)
  "Display system info for system identified by HASH.

If HASH is nil, displays all systems' infos."
  (interactive)
  ;;------------------------------
  ;; Error Checking
  ;;------------------------------
  (unless (or (null hash)
              (stringp hash))
    (nub:error
        :system/multiplexer
        "system:cmd:multiplexer:show"
        "HASH must be a string or nil. Got type %S: %S"
           (type-of hash)
           hash))

  ;; Show each system, or just the matching system.
  (let ((buffer (buffer:name:special "Systems" nil :info)))
    (if hash
        (int<system/multiplexer>:show hash buffer)

      (dolist (hash/system (int<system/multiplexer>:system/hashes))
        (int<system/multiplexer>:show hash/system buffer)))))
;; (system:cmd:multiplexer:show)


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :system 'multiplexer 'multiplex)

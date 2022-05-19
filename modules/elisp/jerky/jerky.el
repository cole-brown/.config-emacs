;;; jerky/jerky.el -*- mode: emacs-lisp; lexical-binding: t -*-


;; ┌───────────────────────────────┬───────┬───────────────────────────────────┐
;; ├───────────────────────────────┼ jerky ┼───────────────────────────────────┤
;; │ Named after ~dirky~, as in "dir key", which needed to be more general...  │
;; │ So... Generlky? Genky?... Jenky?                                          │
;; │ Jerky?                                                                    │
;; │                                 Jerky.                                    │
;; └───────────────────────────────────────────────────────────────────────────┘

(require 'dash) ; `-let*', `-non-nil'

(imp:require :str)
(imp:require :elisp 'utils)

(imp:require :nub)
(imp:require :jerky 'debug)


;; TODO [2020-11-06]: Change get/set to take only one keys arg. list or str.
;;   e.g.
;;     current: (jerky:get :path 'to "key" :namespace :work)
;;     desired: (jerky:get '(:path to "key") :namespace :work)
;;     desired: (jerky:get "path/to/key" :namespace :work)

;; TODO: Make '(namespace system) key a defconst or defcustom.
;;   - also known as: 'namespace 'system


;;------------------------------------------------------------------------------
;; Jerky's Repository of... dried meats?
;;------------------------------------------------------------------------------

(defun int<jerky>:repo/test/string=:cmp (a b)
  "Compare strings A and B (ignoring case).

For `int<jerky>:repo/test/string=' (`define-hash-table-test')."
  ;; `compare-strings' returns t or integer.
  ;; Convert that to a bool t/nil.
  (eq t
      ;; Compare the strings, ignoring case.
      (compare-strings a nil nil b nil nil t)))


(defun int<jerky>:repo/test/string=:hash (key)
  "Get a hashed value for the (lowercased) KEY.

For `int<jerky>:repo/test/string=' (`define-hash-table-test')."
  (sxhash-equal (downcase key)))


;; String comparison, ignores case.
(define-hash-table-test 'int<jerky>:repo/test/string=
  'int<jerky>:repo/test/string=:cmp
  'int<jerky>:repo/test/string=:hash)


(defvar int<jerky>:repo (make-hash-table :test 'int<jerky>:repo/test/string=
                                     :weakness nil)
  "A key-path/value store, basically.

Each hash table key/value is a plist 2-tuple of `:key' and `:record'
\(referred to as a kvp or pair).

The plist's `:key' association holds the hash table KEY it's stored under
\(for inspection).

The `:record' association holds an alist of 3-tuples (aka 'record'):
  (namespace value docstr)

So a hash table tuple might look like this:
  '(:key \"path/to/jeff\"
    :record ((:default \"default value\" \"default docstr\")
             (:jeff    \"jeff value\"    \"jeff docstr\")
             ...))

VALUES can be whatever.

DOCSTR should be short.
  - Or not.
    - I'm not your mother.")


;;------------------------------------------------------------------------------
;; Customizables and Constants
;;------------------------------------------------------------------------------

(defgroup jerky:group nil
  "A tree/value store, basically."
  :group 'extensions)


(defcustom jerky:key/separator "/"
  "Jerky keys will be split/joined using this character.

E.g.: (jerky:get 'my/key :path \"to/dir\")
  The key path used would be: '(my key path to dir)"
  :group 'jerky:group
  :type  '(string))


(defcustom jerky:namespace/default :default
  "(Keyword) name of the default namespace."
  :group 'jerky:group
  :type  '(string))


(defconst jerky:namespace/no-fallback :no-fallback
  "(Keyword) name of the default namespace.")


(defconst int<jerky>:action/delete :int<jerky>:action/delete
  "Value used when a delete is desired.

Use some keyword that will never be used by a jerky user.")



;;------------------------------------------------------------------------------
;; Helpers
;;------------------------------------------------------------------------------

(defun int<jerky>:parse (args-list keywords &rest extra-keywords)
  "Splits ARGS-LIST into keys, and keyword arg/value pairs.

KEY: A list of key strings/symbols/keywords, or a string key, or
a mix. These must come before any of the keyword args.

Keyword key/value pairs only exist after the KEYS. The keywords can be defined
by passing in a list of KEYWORDS '(:value :fallback), or passing t for the
defaults, which are:
  `:value'
  `:docstr'
  `:namespace'

`:namespace'
  The argument after :namespace will be used as the namespace to set the
  data under. If no namespace is provided, this will only look under
  the default.

`:docstr'
  The argument after :docstr will be evaluated and stored as the
  documentation string.

`:value'
  The argument after :value will be stored as the value.

If EXTRA-KEYWORDS are supplied, will apppend them to the end of the
usual returned plist.

Returns a 2-tuple list of:
  - jerky key parsed
  - plist of keys/values parsed.

If a keyword is requested but doesn't exist in the keys, their value in the
output will be nil."
  (-let* ((keywords (cond ((null keywords)   ; Must be supplied something.
                           (error "int<jerky>:parse: keywords cannot be null: %S" keywords))
                          ((listp keywords)  ; Given list? Use it.
                           keywords)
                          (t                 ; Use the defaults.
                           ;; Backwards so they'll end up forwards.
                           '(:docstr :value :namespace))))
          ((args kwargs) (elisp:parse:args+kwargs args-list keywords))
          parsed)

    ;; If extra keywords, add first so they're at the end of the plist.
    (when (not (null extra-keywords))
      ;; Look for extra keywords.
      (dolist (key extra-keywords)
        ;; Add key and value (nil default) to output.
        (push (plist-get kwargs key) parsed)
        (push key parsed)))

    ;; Now look for the usual keywords...
    (dolist (key keywords)
        ;; Add key and value (nil default) to output.
        (push (plist-get kwargs key) parsed)
        (push key parsed))

    ;; And build our tuple output.
    (list (int<jerky>:key:normalize args) parsed)))
;; (int<jerky>:parse '(foo bar baz :namespace qux :value 1) t)
;; (int<jerky>:parse '(foo bar baz :namespace qux :value 1 :baz "hello") t :baz :DNE)
;; (int<jerky>:parse '(foo bar baz :namespace nil :value 1) t)
;; (int<jerky>:parse '(foo (bar baz) :namespace nil :value 1) t)


;;------------------------------------------------------------------------------
;; Namespaces
;;------------------------------------------------------------------------------

(defvar int<jerky>:namespaces
  (list (list jerky:namespace/default
              "Default/Fallback Namespace"
              (concat "Default namespace for jerky. Other namespaces "
                      "default to this for fallbacks.")
              (list jerky:namespace/no-fallback)))
  "Alist of namespace keywords with titles, docstrs, and fallback namespaces.
So an entry would be, for example:
  '(:namespace-name \"Title\" \"docstring\" (:fallback-name-0 :fallback-name-1))

If no/nil fallbacks or nothing found after using fallbacks, jerky
will try the `jerky:namespace/default'.

If the fallback is explicitly `jerky:namespace/no-fallback', no fallbacks
will be checked (including the default).

This starts off with just the default namespace. More have to be
added. Namespaces must exist before keys can be added to them.")
;; (setq int<jerky>:namespaces (list (list jerky:namespace/default jerky:namespace/no-fallback)))


;;------------------------------------------------------------------------------
;; Namespace Functions
;;------------------------------------------------------------------------------

(defun int<jerky>:namespace:valid? (namespace &optional quiet)
  "Validates NAMESPACE.

Doesn't care if it exists or not; just if it's a good
possible-or-current-or-whatever namespace.

Returns t/nil if QUIET is non-nil, else it signals error on failure."
  (if (keywordp namespace)
      t
    (if quiet
        nil
      (error "%s: namespace must be a keyword. %S"
             "int<jerky>:namespace:valid?"
             namespace))))
;; (int<jerky>:namespace:valid? :jeff)
;; (int<jerky>:namespace:valid? 'jeff t)
;; (int<jerky>:namespace:valid? 'jeff)
;; (int<jerky>:namespace:valid? 'not-keyword)


(defun int<jerky>:namespace:entry/namespace:get (entry)
  "Return namespace keyword symbol from ENTRY."
  (nth 0 entry))


(defun int<jerky>:namespace:entry/title:get (entry)
  "Return namespace's title string from ENTRY."
  (nth 1 entry))


(defun int<jerky>:namespace:entry/docstr:get (entry)
  "Return namespace doc string from ENTRY."
  (nth 2 entry))


(defun int<jerky>:namespace:entry/fallback:get (entry)
  "Return namespace fallback list from ENTRY."
  (nth 3 entry))


(defun int<jerky>:namespace:entry:get (namespace)
  "Get a NAMESPACE's entry from `int<jerky>:namespaces'."
  (assoc namespace int<jerky>:namespaces))
;; (int<jerky>:namespace:entry:get :default)
;; (int<jerky>:namespace:entry:get :jeff)


(defun int<jerky>:namespace:entry:set (namespace title docstr fallbacks)
  "Create or overwrite a NAMESPACE entry in `int<jerky>:namespaces'.

TITLE, DOCSTR, and FALLBACKS are used to create the NAMESPACE entry.

TITLE should be a string.

DOCSTR should be a string.

FALLBACKS should be a list of fallback namespace keywords.

Return the created namespace entry."
  ;;------------------------------
  ;; Error checks.
  ;;------------------------------
  (when (eq namespace jerky:namespace/default)
    (error (concat "int<jerky>:namespace:entry:set: "
                   "Cannot create the default namespace's entry: %s")
           namespace))

  (when (not (keywordp namespace))
    (error (concat "int<jerky>:namespace:entry:set: "
                   "NAMESPACE must be a keyword: %s")
           namespace))

  ;;------------------------------
  ;; Validation.
  ;;------------------------------
  (let ((verified-fallbacks nil))
    (dolist (fb-ns (-non-nil fallbacks))
      (cond ((eq fb-ns jerky:namespace/no-fallback)
             (push fb-ns verified-fallbacks))

            ((not (keywordp fb-ns))
             (error (concat "int<jerky>:namespace:entry:set: "
                            "FALLBACK must be a keyword: %s")
                    fb-ns))

            ((not (int<jerky>:namespace/ordered fb-ns 'quiet))
             (error (concat "int<jerky>:namespace:entry:set: "
                            "FALLBACK must be an existing namespace: %s")
                    fb-ns))

            (t
             (push fb-ns verified-fallbacks))))

    ;;------------------------------
    ;; Create Namespace.
    ;;------------------------------
    ;; Ok, good to go. Just make a list in this particular order.
    (list namespace title docstr verified-fallbacks)))


(defun int<jerky>:namespace:set (entry &optional action)
  "Create or overwrite namespace ENTRY in `int<jerky>:namespaces'.

If ACTION is `int<jerky>:action/delete', delete the namespace instead."
  (let* ((namespace (int<jerky>:namespace:entry/namespace:get entry))
         (existing (int<jerky>:namespace:entry:get namespace)))
    (if (null existing)
        ;; None exists. Add it.
        (push entry int<jerky>:namespaces)

      ;; Have the entry! Delete or update it.
      (if (eq action int<jerky>:action/delete)
          ;; Delete entry.
          (setf (alist-get namespace int<jerky>:namespaces
                           ;; DEFAULT set to same as new value for
                           ;; removing from alist. REMOVE set to non-nil.
                           int<jerky>:action/delete int<jerky>:action/delete)
                ;; New value must be eql to DEFAULT provided to alist-get.
                int<jerky>:action/delete)

        ;; Overwrite entry.
        (setf (alist-get namespace int<jerky>:namespaces) (cdr entry))

        ;; Return the updated entry.
        entry))))
;; (int<jerky>:namespace:set :jeffory)
;; int<jerky>:namespaces
;; (int<jerky>:namespace:set :jeff :jeffory)
;; int<jerky>:namespaces


(defun jerky:namespace:create (namespace &rest args)
  "Make the NAMESPACE, which must be a keyword, with optional ARGS.

ARGS is a keyword plist. These are the keywords supported:
  `:title'     - expects a string value
  `:docstr'    - expects a string value
  `:fallbacks' - expects a list of keywords of existing namespaces
                 or `jerky:namespace/no-fallback'.

If fallbacks are supplied, they will be followed in order when a key is not
found in the namespace.

If fallbacks is nil, the namespace will fall back to the default namespace when
a key is not found.

If fallbacks is `jerky:namespace/no-fallback', no fallbacks will be
used/allowed."
  ;; dash-let's plist match pattern to non-keys in ARGS.
  (-let* (((&plist :docstr docstr :title title :fallbacks fallbacks) args)
          ;; Make fallbacks a flat list of inputs or the default.
          (fallbacks (elisp:list:flatten (or fallbacks
                                             jerky:namespace/default))))

    ;; Set new entry.
    (int<jerky>:namespace:set
     ;; Make new entry from provided args.
     (int<jerky>:namespace:entry:set namespace title docstr fallbacks))))
;; (jerky:namespace:create :the-namespace :title "hello there" :docstr "jeff" :fallbacks '(a b c))
;; (jerky:namespace:create :the-namespace :title "hello there" :docstr "jeff")


(defun jerky:namespace:has (namespace)
  "Return t if NAMESPACE is a current Jerky namespace.

That is, is NAMESPACE present in `jerky:namespace/default'?"
  (not (null (assoc namespace int<jerky>:namespaces))))
;; (jerky:namespace:has :jeff)


(defun int<jerky>:namespace/ordered (namespace &optional quiet)
  "Get a NAMESPACE & its fallbacks.

If QUIET is non-nil, don't output messages/warnings.

Return an ordered list of namespaces. If NAMESPACE isn't found, return
`jerky:namespace/default' entry."
  ;;------------------------------
  ;; Sanity Checks
  ;;------------------------------
  (let ((entry (assoc namespace int<jerky>:namespaces))
        (namespaces nil)
        (stop? nil))

    ;; No entry at all? Default to default.
    (when (null entry)
      ;; Warn user that their namespace wasn't found...
      (unless quiet
        (warn "%s: No namespace found for: %s. Using default: %s"
              "int<jerky>:namespace/ordered"
              namespace
              jerky:namespace/default))

      ;; Get default?
      (setq entry (assoc jerky:namespace/default int<jerky>:namespaces))
      (when (null entry)
        (unless quiet
          ;; Warn user there's no default...
          (warn (concat "int<jerky>:namespace/ordered: No default namespace "
                        "found in namespaces!: %s. Where'd it go? %s")
                jerky:namespace/default
                int<jerky>:namespaces))
        ;; Create a default to give user.
        (setq entry
              (int<jerky>:namespace:entry:set jerky:namespace/default
                                          "No default namespace exists."
                                          "No default namespace exists."
                                          jerky:namespace/no-fallback))))

    ;;------------------------------
    ;; Get Namespace and Fallbacks
    ;;------------------------------
    ;; Convert namespaces entry into a list of namespace symbols.
    ;; Follow namespaces and keep appending on to the list until done.

    ;; First things first: the actual namespace.
    (push (int<jerky>:namespace:entry/namespace:get entry) namespaces)

    ;; Second things second: actual namespace's fallbacks.
    (dolist (ns (int<jerky>:namespace:entry/fallback:get entry))
      ;; If we already got a no-fallback, or this one is, set `stop?' flag
      ;; and ignore the rest.
      (if (or stop?
              (eq ns jerky:namespace/no-fallback))
          (setq stop? t)
        (push ns namespaces)))

    ;; Do we have a stop condition already?
    (unless stop?
      ;; Start cascading fallbacks.

      ;; Last things last: get the fallbacks' fallbacks.
      (dolist (ns (cdr namespaces)) ; Skip primary namespace.
        ;; When we have a fallback entry with fallbacks...
        (when-let* ((fb-entry (assoc ns int<jerky>:namespaces))
                    (fb-fb (int<jerky>:namespace:entry/fallback:get fb-entry)))
          ;; ...push each of those onto the list if not there already.
          (dolist (fb-ns fb-fb)
            (unless (or (memq fb-ns namespaces)
                        (eq fb-ns jerky:namespace/no-fallback))
                (push fb-ns namespaces))))))

    ;; Return list of namespaces to check.
    (nreverse namespaces)))
;; (int<jerky>:namespace/ordered :default)
;; (int<jerky>:namespace/ordered :work)
;; (int<jerky>:namespace/ordered :jeff)


(defun jerky:namespace:get ()
  "Look for a namespace to use.

Check/return first to be non-nil of:
  - `int<jerky>:dlv:namespace/local' for a Directory Local Variable value
    (if using jerky/+dlv).
  - Jerky key: 'namespace 'system
  - `jerky:namespace/default'"
  (let ((namespace
         (if (and (boundp 'int<jerky>:dlv:namespace/local)
                  (not (null int<jerky>:dlv:namespace/local)))
             int<jerky>:dlv:namespace/local

           (if-let ((system (jerky:get 'namespace 'system)))
               system
             jerky:namespace/default))))
    (nub:debug :jerky
               "jerky:namespace:get"
               '(:namespace)
               '("vars/settings:\n"
                 "  dlv enabled?     %S\n"
                 "  namespace/local: %S\n"
                 "  system:          %S\n"
                 "  default:         %S\n"
                 "  result:          %S")
               (imp:provided? :jerky 'dlv)
               (if (boundp 'int<jerky>:dlv:namespace/local)
                   int<jerky>:dlv:namespace/local
                 "<jerky/+dlv not in use>")
               (jerky:get 'namespace 'system)
               jerky:namespace/default
               namespace)
    namespace))
;; (jerky:namespace:get)


;;------------------------------------------------------------------------------
;; Key Functions
;;------------------------------------------------------------------------------

(defalias 'int<jerky>:key:symbol->str 'str:normalize:symbol->string
  "Convert a symbol to a string. Removes ':' from keywords.")


(defun int<jerky>:key:normalize (args &optional quiet)
  "Turn ARGS into a key string.

If QUIET is non-nil, return nil instead of signaling error.

(int<jerky>:key:normalize '(\"a/b\" \"c\"))
  -> a/b/c
(int<jerky>:key:normalize '(\"a/b\" c))
  -> a/b/c
(int<jerky>:key:normalize '(\"a\" b c))
  -> a/b/c"
  (if (null args)
      nil
    (let ((strings '()) ; List for processing args.
          (key  nil))   ; Final keypath string built from `strings'.
      ;; If just a string, turn into our `strings' list.
      (cond ((stringp args)
             (setq strings (list args)))

            ;; If a list, turn each item into a string and push to the `strings' list.
            ((listp args)
             (dolist (arg (elisp:list:flatten args)) ; Just want one level of list.
               ;; Push string args to strings, turn non-strings into strings.
               (cond ((stringp arg)
                      (push arg strings))

                     ;; symbol->string: drop keyword prefix if exists.
                     ((symbolp arg)
                      (push (int<jerky>:key:symbol->str arg) strings))

                     ;; function->string:
                     ((functionp arg)
                      (push (funcall arg) strings))

                     ;; fail
                     (t
                      (if quiet
                          nil
                        (error (concat "%s: Can't convert '%S' to string for conversion "
                                       "of keys into key list.")
                               "int<jerky>:key:normalize"
                               arg)))))))

      ;; Now we have strings. They are in backwards order. They need to be turned
      ;; into a final separated string.
      (setq key (string-join (nreverse strings)
                             jerky:key/separator))

      ;; Return the full key string.
      key)))
;; (int<jerky>:key:normalize "a/b")
;; (int<jerky>:key:normalize "a/b" "c")
;; (int<jerky>:key:normalize :base "a/b" "c")
;; (int<jerky>:key:normalize nil)


(defun jerky:key:string (&rest keys)
  "Return the jerky key string obtained by combining & normalizing KEYS."
  (int<jerky>:key:normalize keys))
;; (jerky:key:string "a/b")
;; (jerky:key:string 'a :b)
;; (jerky:key:string "a/b" "c")
;; (jerky:key:string :base "a/b" "c")


;;------------------------------------------------------------------------------
;; Reading from Key-Value Store
;;------------------------------------------------------------------------------

(defun int<jerky>:repo:get (key)
  "Get the key-record plist in jerky's repo at KEY's location."
  (gethash key int<jerky>:repo))


(defun int<jerky>:repo/key:get (plist)
  "Get the `:key' from this key-record PLIST in jerky's repo."
  (plist-get plist :key))


(defun int<jerky>:repo/record:get (plist)
  "Get the `:record' from this key-record PLIST in jerky's repo."
  (plist-get plist :record))


(defun int<jerky>:repo/record/namespace:get (namespaces record)
  "Get the specific NAMESPACES's alist assoc from the key's RECORD in the repo.

If NAMESPACES is a keyword, get exactly that namespace's.

If NAMESPACES is a list (from e.g. `int<jerky>:namespace/ordered'), walk the
list in order and return record from first namespace that has one."
  ;; If one keyword, get it.
  (cond ((keywordp namespaces)
         (assoc namespaces record))

        ((listp namespaces)
         (let ((item nil))
           ;; Loop looking for an item in the record. Save it off for
           ;; returning if found.
           (dolist (ns namespaces item)
             (when (null item)
               (setq item (assoc ns record))))))

        (t
         (error (concat "int<jerky>:repo/record/namespace:get: "
                        "`namespaces' must be a keyword or a list of them. "
                        "Got: %S")
                namespaces))))


(defun int<jerky>:record/namespace:get (record)
  "Get the namespace of this RECORD in jerky's repo."
  (nth 0 record))


(defun int<jerky>:record/value:get (record)
  "Get the value of this RECORD in jerky's repo."
    ;; value is at index 1
    (nth 1 record))


(defun int<jerky>:record/docstr:get (record)
  "Get the docstr of this RECORD in jerky's repo."
  ;; docstr is at index 2
  (nth 2 record))


(defun jerky:get (&rest keys-and-options)
  "Get a record's value from `int<jerky>:repo'.

Split KEYS-AND-OPTIONS into keys, and optional keyword arg/value pairs.

key: A list of key strings/symbols/keywords, or a string key, or
a mix. These must come before any of the optional keyword args.

Keyword key/value pairs only exist after the KEYS. The keywords are:
  `:namespace'
     - The namespace to look in, and what fallbacks to use.
  `:field'
     - Can be: `:namespace', `:value', `:docstr',
     - Defaults to `:value' if not supplied.

If nothing found at key, return will be nil."
  ;; Some shenanigans to do to turn input into key/kwargs,
  ;; then kwargs into options.
  (-let* (((key kwargs) (int<jerky>:parse keys-and-options
                                       '(:namespace :field)))
          (getter nil)
          ((&plist :namespace namespace :field field) kwargs)
          (func.name "jerky:get")
          (func.tags '(:get)))
    (nub:debug:func/start :jerky
                          func.name
                          func.tags
                          (list
                           (cons 'key       key)
                           (cons 'kwargs    kwargs)
                           (cons 'namespace namespace)))

    ;; Check field... is it a known value?
    (cond ((memq field '(:namespace :value :docstr))
           ;; Known values are good. Leave them be.
           (ignore))

          ;; Be the default.
          ((null field)
           (setq field :value))

          ;; Be the... error.
          (t
           (error "Unsupported value for `:field': %s. Only support: %S"
                  field '(:namespace :value :docstr))))

    ;; Set up getter.
    (cond ((eq field :namespace)
           (setq getter #'int<jerky>:record/namespace:get))

          ((eq field :value)
           (setq getter #'int<jerky>:record/value:get))

          ((eq field :docstr)
           (setq getter #'int<jerky>:record/docstr:get)))

    (nub:debug:func :jerky
                    func.name
                    func.tags
                    "ordered namespaces: %s"
                    (int<jerky>:namespace/ordered namespace 'quiet))
    ;; Return whatever the field-getter gets from the namespaced record.
    (let ((got (funcall getter
                        ;; Filter all down to the namespace we want.
                        (int<jerky>:repo/record/namespace:get
                         (int<jerky>:namespace/ordered namespace 'quiet)
                         ;; Get the record.
                         (int<jerky>:repo/record:get (int<jerky>:repo:get key))))))

      (nub:debug:func/end :jerky
                          func.name
                          func.tags
                          (cons 'got got))

      got)))
;; (jerky:get 'path 'to 'thing)
;; (jerky:get :test :jeff)
;; (jerky:get :test :jill)
;; (jerky:get '(signature id sigil))
;; (jerky:get '(signature id sigil) :namespace :work)
;; (jerky:get '(signature id email) :namespace :work)


;;------------------------------------------------------------------------------
;; Writing to Key-Value Store
;;------------------------------------------------------------------------------

(defun int<jerky>:repo/record/namespace:set (namespace value docstr record)
  "Create/overwrite NAMESPACE's alist assoc with new VALUE & DOCSTR.

If VALUE is `int<jerky>:action/delete', remove NAMESPACE's record instead.

Return new, updated copy of record list that the old RECORD should be
replaced with."
  (if (null record)
      ;; No existing record. Create a new one.
      (list (list namespace value docstr))

    ;; Have the record! Delete or update it.
    (if (eq value int<jerky>:action/delete)
        ;; Delete record.
        (setf (alist-get namespace record
                         ;; DEFAULT set to same as new value for
                         ;; removing from alist. REMOVE set to non-nil.
                         int<jerky>:action/delete int<jerky>:action/delete)
              ;; New value must be eql to DEFAULT provided to alist-get.
              int<jerky>:action/delete)

      ;; Overwrite record.
      (setf (alist-get namespace record)
            (list value docstr))

      ;; Return the updated record.
      record)))
;; (setq int<jerky>:alist/test '((:default "default value" "default ds")))
;; (int<jerky>:repo/record.namespace:set :home "test" "test ds" 'int<jerky>:alist/test)
;; int<jerky>:alist/test


(defun int<jerky>:repo/key:set (key plist)
  "Set the KEY in the hash table value PLIST.

Return the updated plist that the old plist should be replaced with; you may or
may not get a copy and the original may or may not have been destructievly
updated."
  (plist-put plist :key key))
;; (int<jerky>:repo/key:set 'jeff nil)


(defun int<jerky>:repo/record:set (record plist)
  "Set the RECORD in the hash table value PLIST.

Return the updated plist that the old plist should be replaced with; you may or
may not get a copy and the original may or may not have been destructievly
updated."
  (plist-put plist :record record))


(defun int<jerky>:repo:set (key plist)
  "Create/overwrite the KEY in `int<jerky>:repo', setting it to PLIST.

If PLIST is `int<jerky>:action/delete', remove KEY from `int<jerky>:repo'
instead."
  ;; Delete?
  (cond ((eq plist int<jerky>:action/delete)
         (remhash key int<jerky>:repo))

        ;; Has the correct members for adding?
        ((and (plist-member plist :key)
              (plist-member plist :record))
         (puthash key plist int<jerky>:repo))

        ;; Error out, I guess.
        (t
         (error (concat "int<jerky>:repo:set: plist must be "
                        "`int<jerky>:action/delete' or an actual plist "
                        "with `:key' and `:record' members. Got: %s")
                plist))))


(defun int<jerky>:repo/update (key namespace value docstr)
  "Create, delete, or update/overwrite a NAMESPACE'd VALUE with DOCSTR.
File it under KEY in `int<jerky>:repo'.

To delete the NAMESPACE's value, pass `int<jerky>:action/delete' as the value."
  ;; Get existing plist value in our repo under key. Could be nil if it
  ;; doesn't exist; that's fine.
  (let* ((plist (int<jerky>:repo:get key))
         (record (int<jerky>:repo/record:get plist)))

    ;; plist/record will be nil if they don't exist.
    ;; The code path is the same for a brand new thing and for updating
    ;; an existing thing.

    (int<jerky>:repo:set
     key
     ;; Make the record w/ plist from making the key.
     (int<jerky>:repo/record:set
      (int<jerky>:repo/record/namespace:set namespace value docstr record)
      ;; Make plist for the key.
      (int<jerky>:repo/key:set key plist)))))


(defun jerky:set (&rest keys-and-options)
  "Overwrite an existing record or add new record to `int<jerky>:repo'.

Splits KEYS-AND-OPTIONS into keys, and keyword arg/value pairs.

key: A list of key strings/symbols/keywords, or a string key, or
a mix. These must come before any of the keyword args.

Keyword key/value pairs only exist after the keys. The keywords are:
  `:value'
  `:docstr'
  `:namespace'

`:namespace'
  The argument after :namespace will be used as the namespace to set the
  data under. If no namespace is provided, this will only look under
  the default.

`:docstr'
  The argument after :docstr will be evaluated and stored as the
  documentation string.

`:value'
  The argument after :value will be stored as the value.

If not provided, they will be nil."
  ;; Some shenanigans to do to turn input into args/kwargs into a key
  ;; and values.
  (-let* (((key kwargs) (int<jerky>:parse keys-and-options t))
          ;; dash-let's plist match pattern to non-keys in ARGS.
          ((&plist :docstr :value :namespace) kwargs))

    ;; Get/update/create entries, set hash to key in repo.
    (int<jerky>:repo/update key
                        (or namespace jerky:namespace/default)
                        value
                        docstr)))
;; (jerky:set '(path to thing) :value "hello there")
;; (jerky:set '(:test :jeff) :value "jeffe" :docstr "I am a comment.")
;; (jerky:set :test "jeff" :value "jeffe overwrite" :docstr "I am not a comment.")
;; (jerky:set "test/jeff" :value "jeffe")
;; (jerky:set "test/jill" :value "jill")


;;------------------------------------------------------------------------------
;; Searching the Repo.
;;------------------------------------------------------------------------------

(defun int<jerky>:search/filter (search-key &optional namespace)
  "Return any entries in jerky that match SEARCH-KEY and optional NAMESPACE.

Walk `int<jerky>:repo' and return anything that matches (potentially partial)
SEARCH-KEY (and possibly NAMESPACE).

KEYS /must/ be normalized already!

If NAMESPACE is nil, all namespaces will be matched.

Return list of results or nil.

Result format is:
  '(full-key-str namespace-keyword value)
Note: This is /not/ an alist, as the same key (differing namespaces)
can exist multiple times."
  (let ((results nil))
    ;; Maphash's function must take only: key and value.
    (maphash (lambda (key value)
               (when (and (stringp key)
                          (string-prefix-p search-key key))
                 ;; Matched key; get record from 'value'.
                 (if-let ((records (int<jerky>:repo/record:get value)))
                     (dolist (rec records)
                       ;; Check namespace if needed; add rec if matches.
                       (when (or (null namespace)
                                 (eq namespace (int<jerky>:record/namespace:get rec)))
                         (push (list key
                                     (int<jerky>:record/namespace:get rec)
                                     (int<jerky>:record/value:get rec))
                               results))))))
             int<jerky>:repo)

    ;; Return whatever we found.
    results))
;; (int<jerky>:search/filter "signature/id")
;; (int<jerky>:search/filter "signature/id" :work)


(defun jerky:has (&rest keys-and-options)
  "Return a list of matches that jerky has to KEYS-AND-OPTIONS.

Split KEYS-AND-OPTIONS into keys, and optional keyword arg/value pairs.

key: A list of key strings/symbols/keywords, or a string key, or
a mix. These must come before any of the optional keyword args.

Keyword key/value pairs only exist after the KEYS. The keywords are:
  `:namespace'
     - The namespace to look in, and what fallbacks to use.

Return list of results or nil.

Result format is:
  '(full-key-str namespace-keyword value)
Note: This is /not/ an alist, as the same key (differing namespaces)
can exist multiple times.

Example:
  - KEYS-AND-OPTIONS: \"path/to\"
  '((\"path/to/jeff\" :work 9001)
    (\"path/to/jeff\" :home :value)
    (\"path/to/jill\" :default \"42\"))"
  ;; Some shenanigans to do to turn input into args/kwargs into a key
  ;; and a namespace.
  (-let* (((partial-key kwargs) (int<jerky>:parse keys-and-options '(:namespace)))
          ;; dash-let's plist match pattern to non-keys in ARGS.
          ((&plist :namespace namespace) kwargs)
          matches)
    (nub:debug:func/start :jerky
                          "jerky:has"
                          '(:get)
                          (list
                           (cons "keys-and-options" keys-and-options)
                           (cons "->    key"           partial-key)
                           (cons "-> kwargs"        kwargs)
                           (cons "partial-key"      partial-key)
                           (cons "namespace"        namespace)))
    ;; Now we can search & filter.
    (setq matches (int<jerky>:search/filter partial-key namespace)
    (nub:debug:func/start :jerky
                          "jerky:has"
                          '(:get)
                          (list
                           (cons 'matches matches)))
    matches))
;; (jerky:has 'signature 'id)
;; (jerky:has "signature/id")
;; (jerky:has 'signature 'id :namespace :work)


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :jerky 'jerky)
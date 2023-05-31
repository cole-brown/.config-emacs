;;; mantle/config/dev-env/web.el --- Spider Web Development? -*- lexical-binding: t; -*-
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
;;  Spider Web Development?
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Keybinds: General Infixes
;;------------------------------------------------------------------------------
;; Define the infix with the title here so we don't have to worry about what's
;; defined in what order since you can only define the title once or else things
;; overwrite each other?

(when (imp:flag? :keybinds +evil)
  (imp:eval:after (:and evil evil-collection
                        (:keybinds user general evil))
    (keybind:leader/local:def
      :keymaps 'web-mode-map
      :infix "a"
      "" '(nil :which-key "attribute..."))


    (keybind:leader/local:def
      :keymaps 'web-mode-map
      :infix "b"
      "" '(nil :which-key "block..."))


    (keybind:leader/local:def
      :keymaps 'web-mode-map
      :infix "d"
      "" '(nil :which-key "dom..."))


    (keybind:leader/local:def
      :keymaps 'web-mode-map
      :infix "e"
      "" '(nil :which-key "element..."))


    (keybind:leader/local:def
      :keymaps 'web-mode-map
      :infix "t"
      "" '(nil :which-key "tag..."))))


;;------------------------------------------------------------------------------
;; Web
;;------------------------------------------------------------------------------

(imp:use-package web-mode

  :mode "\\.[px]?html?\\'" ; Will make `web-mode' responsible for normal "*.html"/"*.htm" files
  :mode "\\.\\(?:tpl\\|blade\\)\\(?:\\.php\\)?\\'"
  :mode "\\.erb\\'"
  :mode "\\.[lh]?eex\\'"
  :mode "\\.sface\\'"
  :mode "\\.jsp\\'"
  :mode "\\.as[cp]x\\'"
  :mode "\\.ejs\\'"
  :mode "\\.hbs\\'"
  :mode "\\.mustache\\'"
  :mode "\\.svelte\\'"
  :mode "\\.twig\\'"
  :mode "\\.jinja2?\\'"
  :mode "\\.eco\\'"
  :mode "wp-content/themes/.+/.+\\.php\\'"
  :mode "templates/.+\\.php\\'"

  ;;------------------------------
  :init
  ;;------------------------------

  (innit:hook:defun
     (:name   'web:settings
      :docstr "Settings for Web mode. Non-LSP stuff.")

   ;; Nothing currently, I guess?

   ;; Separate camel-case into separate words?
   ;; (subword-mode t)
   )


  (innit:hook:defun
     (:name   'web:fix-js-comments
      :docstr "Fix comment handling in `web-mode' for JavaScript.")
   (when (member web-mode-content-type '("javascript" "jsx"))
     ;; For some reason the default is to insert HTML comments even
     ;; in JavaScript.
     (setq-local comment-start "//")
     (setq-local comment-end "")
     ;; Needed since otherwise the default value generated by
     ;; `comment-normalize-vars' will key off the syntax and think
     ;; that a single "/" starts a comment, which completely borks
     ;; auto-fill.
     (setq-local comment-start-skip "// *")))


  ;;------------------------------
  ;; NOTE: Vue Fallback Mode: `:init'
  ;;------------------------------
  ;; If the user has installed `vue-mode' then, by appending this to
  ;; `auto-mode-alist' rather than prepending it, its autoload will have
  ;; priority over this one.
  (add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode) 'append)


  ;;------------------------------
  ;; NOTE: Vue Fallback Mode: `:mode'
  ;; I think this has to happen after `add-to-list' so it doesn't get prepended by `use-package'?
  :mode "\\.vue\\'"
  ;;------------------------------


  ;;------------------------------
  :hook
  ;;------------------------------
  ((web-mode-hook . mantle:hook:web:settings)
   (web-mode-hook . mantle:hook:web:fix-js-comments))


  ;;------------------------------
  :custom
  ;;------------------------------

  ;; These default to 2 if `standard-indent' is not `boundp' and truthy.
  (web-mode-markup-indent-offset (jerky:get 'code 'tab 'standard))

  (web-mode-enable-html-entities-fontification t)

  ;; (web-mode-auto-close-style 1)


  ;;------------------------------
  :config
  ;;------------------------------

  ;;---
  ;; SmartParens & Web-Mode
  ;;---
  (imp:eval:after smartparens
    (defun mantle:user:web:is-auto-close-style-3 (_id action _context)
      (and (eq action 'insert)
           (eq web-mode-auto-close-style 3)))
    (sp-local-pair 'web-mode
                   "<" ">"
                   :unless '(:add mantle:user:web:is-auto-close-style-3))

    ;; let smartparens handle these
    (innit:customize-set-variable web-mode-enable-auto-quoting nil)
    (innit:customize-set-variable web-mode-enable-auto-pairing t)

    ;; 1. Remove web-mode auto pairs whose end pair starts with a latter
    ;;    (truncated autopairs like <?p and hp ?>). Smartparens handles these
    ;;    better.
    ;; 2. Strips out extra closing pairs to prevent redundant characters
    ;;    inserted by smartparens.
    (dolist (alist web-mode-engines-auto-pairs)
      (setcdr alist
              (cl-loop for pair in (cdr alist)
                       unless (string-match-p "^[a-z-]" (cdr pair))
                       collect (cons (car pair)
                                     (string-trim-right (cdr pair)
                                                        "\\(?:>\\|]\\|}\\)+\\'")))))
    (setq web-mode-engines-auto-pairs (delq nil web-mode-engines-auto-pairs)))

  ;;---
  ;; Web-Mode Config
  ;;---
  (add-to-list 'web-mode-engines-alist '("elixir" . "\\.eex\\'"))
  (add-to-list 'web-mode-engines-alist '("phoenix" . "\\.[lh]eex\\'"))

  ;; Use // instead of /* as the default comment delimited in JS
  (setf (alist-get "javascript" web-mode-comment-formats nil nil #'equal)
        "//"))


;;------------------------------
;; Keybinds : Meow
;;------------------------------

(imp:use-package web-mode
  :when  (imp:flag? :keybinds +meow)
  :after meow

  ;;------------------------------
  :bind ; meow
  ;;------------------------------

  (:map web-mode-map
   ;; TODO-meow: Where is my comment/uncomment actually? Evil/Doom had it on "gc".
   ("M-/" . web-mode-comment-or-uncomment)) ; Comment/Uncomment


  ;;------------------------------
  :config
  ;;------------------------------

  ;;------------------------------
  ;; Common
  ;;------------------------------
  ;; Some big menus - use transients for better control of layout?

  ;;---
  ;; Local Leader: Insert...
  ;;---
  (transient-define-prefix mantle:meow/transient:web/insert ()
    "`web-mode' insert menu"
    ["Insert..."
     ["Attribute"
      ("b" "Beginning" web-mode-attribute-beginning)
      ("e" "End" web-mode-attribute-end)
      ("i" "Insert" web-mode-attribute-insert)
      ("n" "Next" web-mode-attribute-next)
      ("s" "Select" web-mode-attribute-select)
      ("k" "Kill" web-mode-attribute-kill)
      ("p" "Previous" web-mode-attribute-previous)
      ("t" "Transpose" web-mode-attribute-transpose)]

     ["Block"
      ("b" "Beginning" web-mode-block-beginning)
      ("c" "Close" web-mode-block-close)
      ("e" "End" web-mode-block-end)
      ("k" "Kill" web-mode-block-kill)
      ("n" "Next" web-mode-block-next)
      ("p" "Previous" web-mode-block-previous)
      ("s" "Select" web-mode-block-select)]

     ["DOM"
      ("a" "Replace Apostrophies" web-mode-dom-apostrophes-replace)
      ("d" "Show Errors" web-mode-dom-errors-show)
      ("e" "Encode Entities" web-mode-dom-entities-encode)
      ("n" "Normalize" web-mode-dom-normalize)
      ("q" "Replace Quotes" web-mode-dom-quotes-replace)
      ("t" "Traverse" web-mode-dom-traverse)
      ("x" "XPath" web-mode-dom-xpath)]

     ["Element"
      ("/" "Close" web-mode-element-close)
      ("a" "Select Content" web-mode-element-content-select)
      ("b" "Beginning" web-mode-element-beginning)
      ("c" "Clone" web-mode-element-clone)
      ("d" "Child" web-mode-element-child)
      ("e" "End" web-mode-element-end)
      ("f" "Fold/Unfold Children" web-mode-element-children-fold-or-unfold)
      ("i" "Insert" web-mode-element-insert)
      ("k" "Kill" web-mode-element-kill)
      ("m" "Mute Blanks" web-mode-element-mute-blanks)
      ("n" "Next" web-mode-element-next)
      ("p" "Previous" web-mode-element-previous)
      ("r" "Rename" web-mode-element-rename)
      ("s" "Select" web-mode-element-select)
      ("t" "Transpose" web-mode-element-transpose)
      ("u" "Parent" web-mode-element-parent)
      ("v" "Vanish" web-mode-element-vanish)
      ("w" "Wrap" web-mode-element-wrap)]

     ["Tag"
      ("a" "Sort Attributes" web-mode-tag-attributes-sort)
      ("b" "Beginning" web-mode-tag-beginning)
      ("e" "End" web-mode-tag-end)
      ("m" "Match" web-mode-tag-match)
      ("n" "Next" web-mode-tag-next)
      ("p" "Previous" web-mode-tag-previous)
      ("s" "Select" web-mode-tag-select)]])


  ;;------------------------------
  ;; `General'
  ;;------------------------------

  (defun mantle:meow/keybind/general:web ()
    "Create the `web' keybinds in `general' for `meow'."
    (keybind:meow:leader/local:bind-keys
        'python-mode-map
      "i"   (list #'mantle:meow/transient:web/insert :which-key "Insert...")
      "b h" (list #'web-mode-reload                  :which-key "Rehighlight Buffer")
      "b i" (list #'web-mode-buffer-indent           :which-key "Indent Buffer")
      ;; TODO-meow: Somewhere else? This is an evil key for fold/unfold...
      ;; TODO-meow: like... ([remap func-or-unfunc] . web-mode-fold-or-unfold) in a `:bind' section?
      "b f"  (list #'web-mode-fold-or-unfold         :which-key "Fold/Unfold")
      ;; TODO-meow: move to actual movement keys or... somewhere you can spam repeat?
      "] a" (list #'web-mode-attribute-next          :which-key "Attribute: Next")
      "[ a" (list #'web-mode-attribute-previous      :which-key "Attribute: Previous")
      "] t" (list #'web-mode-tag-next                :which-key "Tag: Next")
      "[ t" (list #'web-mode-tag-previous            :which-key "Tag: Previous")
      "] T" (list #'web-mode-element-child           :which-key "Child: Next")
      "[ T" (list #'web-mode-element-parent          :which-key "Child: Previous")))

  ;;------------------------------
  ;; `Transient'
  ;;------------------------------

  (defun mantle:meow/keybind/transient:web ()
    "Create the `web' keybinds in `transient' for `meow'."

    ;;---
    ;; Local Leader: Insert...
    ;;---
    (mantle:meow:leader/local:keys web-mode-map
                                   "i" mantle:meow/transient:web/insert)

    ;;---
    ;; Local Leader: Misc.
    ;;---
    (transient-define-prefix mantle:meow/transient:web/misc ()
      "`web-mode' insert menu"
      [["Buffer..."
        ("h" "Rehighlight Buffer" web-mode-reload)
        ("i" "Indent Buffer" web-mode-buffer-indent)
        ;; TODO-meow: Somewhere else? This is an evil key for fold/unfold...
        ;; TODO-meow: like... ([remap func-or-unfunc] . web-mode-fold-or-unfold) in a `:bind' section?
        ("f"  "Fold/Unfold" web-mode-fold-or-unfold)]
       ;; TODO-meow: move to actual movement keys or... somewhere you can spam repeat? Does transient have a no-exit hydra kinda thing?
       ["Movement"
        ("]a" "Attribute: Next" web-mode-attribute-next)
        ("[a" "Attribute: Previous" web-mode-attribute-previous)
        ("]t" "Tag: Next" web-mode-tag-next)
        ("[t" "Tag: Previous" web-mode-tag-previous)
        ("]T" "Child: Next" web-mode-element-child)
        ("[T" "Child: Previous" web-mode-element-parent)]])

    (mantle:meow:leader/local:keys web-mode-map
                                   "l" mantle:meow/transient:web/misc))


  ;;------------------------------
  ;; Actually Create Keybinds:
  ;;------------------------------

  (if (imp:provided? :keybinds 'user 'general 'meow)
      (mantle:meow/keybind/general:web)
    (mantle:meow/keybind/transient:web)))


;;------------------------------
;; Keybinds : Evil
;;------------------------------

(imp:use-package web-mode
  :when  (imp:flag? :keybinds +evil)
  :after (:and evil evil-collection)

  ;;------------------------------
  :general ; evil
  ;;------------------------------
  ;;---
  ;; Attribute
  ;;---
  (:prefix  (keybind:prefix :local "a")
   :states  keybind:leader/local:states
   :keymaps 'web-mode-map
   "b" (list #'web-mode-attribute-beginning :which-key "Beginning")
   "e" (list #'web-mode-attribute-end       :which-key "End")
   "i" (list #'web-mode-attribute-insert    :which-key "Insert")
   "n" (list #'web-mode-attribute-next      :which-key "Next")
   "s" (list #'web-mode-attribute-select    :which-key "Select")
   "k" (list #'web-mode-attribute-kill      :which-key "Kill")
   "p" (list #'web-mode-attribute-previous  :which-key "Previous")
   "t" (list #'web-mode-attribute-transpose :which-key "Transpose"))

  ;;---
  ;; Block
  ;;---
  (:prefix  (keybind:prefix :local "b")
   :states  keybind:leader/local:states
   :keymaps 'web-mode-map
   "b" (list #'web-mode-block-beginning :which-key "Beginning")
   "c" (list #'web-mode-block-close     :which-key "Close")
   "e" (list #'web-mode-block-end       :which-key "End")
   "k" (list #'web-mode-block-kill      :which-key "Kill")
   "n" (list #'web-mode-block-next      :which-key "Next")
   "p" (list #'web-mode-block-previous  :which-key "Previous")
   "s" (list #'web-mode-block-select    :which-key "Select"))

  ;;---
  ;; DOM
  ;;---
  (:prefix  (keybind:prefix :local "d")
   :states  keybind:leader/local:states
   :keymaps 'web-mode-map
   "a" (list #'web-mode-dom-apostrophes-replace :which-key "Replace Apostrophies")
   "d" (list #'web-mode-dom-errors-show         :which-key "Show Errors")
   "e" (list #'web-mode-dom-entities-encode     :which-key "Encode Entities")
   "n" (list #'web-mode-dom-normalize           :which-key "Normalize")
   "q" (list #'web-mode-dom-quotes-replace      :which-key "Replace Quotes")
   "t" (list #'web-mode-dom-traverse            :which-key "Traverse")
   "x" (list #'web-mode-dom-xpath               :which-key "XPath"))

  ;;---
  ;; Element
  ;;---
  (:prefix  (keybind:prefix :local "e")
   :states  keybind:leader/local:states
   :keymaps 'web-mode-map
    "/" (list #'web-mode-element-close                   :which-key "Close")
    "a" (list #'web-mode-element-content-select          :which-key "Select Content")
    "b" (list #'web-mode-element-beginning               :which-key "Beginning")
    "c" (list #'web-mode-element-clone                   :which-key "Clone")
    "d" (list #'web-mode-element-child                   :which-key "Child")
    "e" (list #'web-mode-element-end                     :which-key "End")
    "f" (list #'web-mode-element-children-fold-or-unfold :which-key "Fold/Unfold Children")
    "i" (list #'web-mode-element-insert                  :which-key "Insert")
    "k" (list #'web-mode-element-kill                    :which-key "Kill")
    "m" (list #'web-mode-element-mute-blanks             :which-key "Mute Blanks")
    "n" (list #'web-mode-element-next                    :which-key "Next")
    "p" (list #'web-mode-element-previous                :which-key "Previous")
    "r" (list #'web-mode-element-rename                  :which-key "Rename")
    "s" (list #'web-mode-element-select                  :which-key "Select")
    "t" (list #'web-mode-element-transpose               :which-key "Transpose")
    "u" (list #'web-mode-element-parent                  :which-key "Parent")
    "v" (list #'web-mode-element-vanish                  :which-key "Vanish")
    "w" (list #'web-mode-element-wrap                    :which-key "Wrap"))

  ;;---
  ;; Tag
  ;;---
  (:prefix  (keybind:prefix :local "t")
   :states  keybind:leader/local:states
   :keymaps 'web-mode-map
   "a" (list #'web-mode-tag-attributes-sort :which-key "Sort Attributes")
   "b" (list #'web-mode-tag-beginning       :which-key "Beginning")
   "e" (list #'web-mode-tag-end             :which-key "End")
   "m" (list #'web-mode-tag-match           :which-key "Match")
   "n" (list #'web-mode-tag-next            :which-key "Next")
   "p" (list #'web-mode-tag-previous        :which-key "Previous")
   "s" (list #'web-mode-tag-select          :which-key "Select"))

  ;;---
  ;; Leader: Local
  ;;---
  (:prefix  (keybind:prefix :local)
   :states  keybind:leader/local:states
   :keymaps 'web-mode-map

   "h" (list #'web-mode-reload        :which-key "Rehighlight Buffer")
   "i" (list #'web-mode-buffer-indent :which-key "Indent Buffer"))

  ;;---
  ;; Leaderless
  ;;---
  (:states '(global)
   :keymaps 'web-mode-map
   "M-/" (list #'web-mode-comment-or-uncomment :which-key "Comment/Uncomment"))

  ;; TODO: Does this really need to be bound?!
  ;; (:states '(insert)
  ;;  :keymaps 'web-mode-map
  ;;  "SPC" #'self-insert-command)

  (:states '(normal)
   :keymaps 'web-mode-map
   "za" (list #'web-mode-fold-or-unfold :which-key "Fold/Unfold"))

  (:states '(normal visual)
   :keymaps 'web-mode-map
   "]a" (list #'web-mode-attribute-next     :which-key "Attribute: Next")
   "[a" (list #'web-mode-attribute-previous :which-key "Attribute: Previous")
   "]t" (list #'web-mode-tag-next           :which-key "Tag: Next")
   "[t" (list #'web-mode-tag-previous       :which-key "Tag: Previous")
   "]T" (list #'web-mode-element-child      :which-key "Child: Next")
   "[T" (list #'web-mode-element-parent     :which-key "Child: Previous")))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'config 'user 'dev-env 'languages 'web)

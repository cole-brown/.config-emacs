;;; mantle/config/completion.el --- Ivy, Helm, SMOCE, or... -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2022-07-20
;; Modified:   2022-07-20
;; URL:        https://github.com/cole-brown/.config-emacs
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;  Ivy, Helm, SMOCE, or...
;;
;;; Code:

;;; completion.el --- Change basic keybinds. -*- lexical-binding: t; -*-
;;; general.el --- General Keybinds -*- lexical-binding: t; -*-
;;
;;
;;; Commentary:
;;
;; Change basic keybinds.
;;
;;; Code:


;;------------------------------------------------------------------------------
;; "SMOCE Stack": Selectrum & Friends
;;------------------------------------------------------------------------------
;; There's the standards like `ivy' and `helm', but I want to try newer next gen stuff.
;; Like this:
;;   https://www.reddit.com/r/emacs/comments/ppg98f/comment/hd3hll1/?utm_source=share&utm_medium=web2x&context=3
;; From:
;;   https://www.reddit.com/r/emacs/comments/ppg98f/which_completion_framework_do_you_use_and_why/
;;
;; So... Introducing the "SMOCE Stack":
;;   - https://github.com/radian-software/selectrum#complementary-extensions
;;   - Selectrum:       https://github.com/raxod502/selectrum
;;     - (or Vertico?): https://github.com/minad/vertico
;;   - Marginalia:      https://github.com/minad/marginalia
;;   - Orderless:       https://github.com/oantolin/orderless
;;   - Consult:         https://github.com/minad/consult
;;   - Embark:          https://github.com/oantolin/embark


;;------------------------------------------------------------------------------
;; Selectrum: Better Completion UI
;;------------------------------------------------------------------------------
;; https://github.com/raxod502/selectrum

;;------------------------------
;; Selectrum
;;------------------------------
;; Not a whole lot of configuration...

(imp:use-package selectrum
  ;;--------------------
  :config
  ;;--------------------
  (selectrum-mode +1))

;; TODO: Are there /evil/ Selectrum keybinds?
;;   - https://github.com/radian-software/selectrum#keybindings


;;------------------------------
;; Selectrum + Orderless
;;------------------------------
;; https://github.com/radian-software/selectrum#alternative-2-orderless
;; https://github.com/oantolin/orderless#selectrum

(imp:eval:after (:and selectrum orderless)

  ;; Persist history over Emacs restarts
  (savehist-mode)

  ;; Optional performance optimization
  ;; by highlighting only the visible candidates.
  (customize-set-variable 'orderless-skip-highlighting (lambda () selectrum-is-active))
  (customize-set-variable 'selectrum-refine-candidates-function    #'orderless-filter)
  (customize-set-variable 'selectrum-highlight-candidates-function #'orderless-highlight-matches))


;; TODO: Do I want this:
;;   > "In some cases you may want to consider to use Prescient on top of
;;   > Orderless. Prescient can be used to provide frecency-based sorting (a
;;   > combination of frequency and recency) and history persistence by adding
;;   > the following."
;; TODO: If so:
;; TODO:   1) Uncomment this.
;; TODO:   2) Make an `imp:use-package' section for `prescient'.
;; ;;------------------------------
;; ;; Selectrum + Orderless + Prescient
;; ;;------------------------------
;; (imp:eval:after (:and selectrum orderless prescient)
;;
;;   (customize-set-variable 'selectrum-prescient-enable-filtering nil)
;;   (selectrum-prescient-mode +1)
;;   (prescient-persist-mode +1))


;;------------------------------------------------------------------------------
;; Marginalia: Notes & Info in the Minibuffer Margin
;;------------------------------------------------------------------------------
;; https://github.com/minad/marginalia

(imp:use-package marginalia
  ;; The :init configuration is always executed (Not lazy!)
  ;;--------------------
  :init
  ;;--------------------

  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode)

  ;;--------------------
  :bind
  ;;--------------------
  ;; TODO: What keybind to give this? What does it do?
  ;; Either bind `marginalia-cycle` globally or only in the minibuffer
  (("M-A" . marginalia-cycle)
   :map minibuffer-local-map
   ("M-A" . marginalia-cycle)))


;;------------------------------------------------------------------------------
;; Orderless: Orderless, Space-Separated Completion Style
;;------------------------------------------------------------------------------
;; https://github.com/oantolin/orderless

(imp:use-package orderless

  ;;--------------------
  :custom
  ;;--------------------
  ;; Selectrum suggests only `orderless' when using Selectrum & Orderless together.
  ;; Orderless suggests `orderless' and `basic'.
  ;; Orderless explains, so go with Orderless' settings?
  ;;
  ;;   > The `basic' completion style is specified as fallback in addition to
  ;;   > `orderless' in order to ensure that completion commands which rely on
  ;;   > dynamic completion tables, e.g., `completion-table-dynamic' or
  ;;   > `completion-table-in-turn', work correctly. Furthermore the `basic'
  ;;   > completion style needs to be tried /first/ (not as a fallback) for
  ;;   > TRAMP hostname completion to work. In order to achieve that, we add an
  ;;   > entry for the `file' completion category in the
  ;;   > `completion-category-overrides' variable. In addition, the
  ;;   > `partial-completion' style allows you to use wildcards for file
  ;;   > completion and partial paths, e.g., '/u/s/l' for '/usr/share/local'.
  ;;     - https://github.com/oantolin/orderless#overview
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'config 'user 'completion)

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
;; Not a whole lot of configuration

;;------------------------------
;; Selectrum
;;------------------------------

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

(imp:eval:after (:and selectrum orderless)

  ;; Tell Emacs/`selectrum' to use `orderless' for completion.
  (customize-set-variable 'completion-styles '(orderless))

  ;; Persist history over Emacs restarts
  (savehist-mode)

  ;; Optional performance optimization
  ;; by highlighting only the visible candidates.
  (customize-set-variable 'orderless-skip-highlighting (lambda () selectrum-is-active))
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
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'config 'user 'completion)

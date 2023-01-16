;;; dlv/init.el --- Directory Local Variables -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; Homepage:   https://github.com/cole-brown/.config-doom
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Code-Defined Directory Local Variables
;; Code-Defined File Local Variables
;;
;; ------------------------------
;; NOTE: Namespaces
;; ---
;; dlv has three 'namespace' prefixes:
;;   `dlv:'       - public/API functions, variables, etc
;;   `int<dlv>:'  - private/internal functions, variables, etc
;;   `test<dlv>:' - Emacs ERT functions, variables, etc
;; ------------------------------
;;
;; TODO: describe `imp' flags
;; Flags:
;;   - +debug
;;   - NOTE: Currently considering these mutually exclusive
;;     - -enabled
;;     - +enabled/safe
;;     - +enabled/all
;;     - +enabled/flag
;;   - -display
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Set-Up.
;;------------------------------------------------------------------------------

(imp:path:root/set :dlv
               (imp:path:current:dir)
               (imp:file:current))


;;------------------------------------------------------------------------------
;; Load Files.
;;------------------------------------------------------------------------------

(imp:timing
    :dlv
    (imp:file:current)
    (imp:path:current:dir)

  ;;------------------------------
  ;; Debugging
  ;;------------------------------

  (imp:load :feature  '(:dlv debug)
            :filename "debug")
  ;; Initialize debugging before going any further.
  (int<dlv>:debug:init)

  ;;------------------------------
  ;; Required
  ;;------------------------------

  (imp:load :feature  '(:dlv path)
            :filename "path")
  (imp:load :feature  '(:dlv class)
            :filename "class")
  (imp:load :feature  '(:dlv dlv)
            :filename "dlv")

  ;;------------------------------
  ;; Optional Files
  ;;------------------------------

  ;; Always load unless specifically removed.
  (unless (imp:flag? :dlv -display)
    (imp:load :feature  '(:dlv +display)
              :filename "+display")))


;;------------------------------------------------------------------------------
;; Optional Functionality
;;------------------------------------------------------------------------------

;; Only run the optional functionality checks/enables when loading this file.
(when (imp:provide:loading?)
  ;;------------------------------
  ;; Enable/disable DLVs?
  ;;------------------------------
  ;; Check for a feature flag for how to enable DLVs.
  ;; NOTE: Currently considering these flags mutually exclusive.
  (cond ((imp:flag? :dlv -enabled) ;; Not enabled == disabled.
         (dlv:enable :disable))
        ((imp:flag? :dlv +enabled/safe) ;; Only safe DLVs allowed!
         (dlv:enable :safe))
        ((imp:flag? :dlv +enabled/all) ;; Always allow anything - potentially dangerous!
         (dlv:enable :all))
        ((imp:flag? :dlv +enabled/prompt) ;; Always ask the user.
         (dlv:enable :prompt))
        ;; Default: Always enable DLVs unless specifically told not to.
        (t
         (dlv:enable :enable))))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide:with-emacs :dlv)

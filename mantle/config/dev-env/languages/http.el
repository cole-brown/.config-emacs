;;; mantle/config/dev-env/http.el --- Hyper Text Markup Language -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2023-01-04
;; Modified:   2023-01-04
;; URL:        https://github.com/cole-brown/.config-emacs
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; Hyper Text Markup Language!
;; Its... not really about text or markup anymore; it's a whole 'nuther OS.
;;
;;; Code:


(imp:require :keybind)


;;------------------------------------------------------------------------------
;; HTTP Status Codes
;;------------------------------------------------------------------------------

;; https://github.com/for-GET/know-your-http-well
(imp:use-package know-your-http-well
  :commands (http-header http-method http-relation http-status-code)

  ;; TODO-keybinds: Put these keybinds under the leader somewhere?
  ;; M-x http-header ;; content-type
  ;; M-x http-method ;; post | POST
  ;; M-x http-relation ;; describedby
  ;; M-x http-status-code ;; 500
  ;; M-x http-status-code ;; not_found | NOT_FOUND
  )


;;------------------------------------------------------------------------------
;; REST Interface Testing
;;------------------------------------------------------------------------------
;; https://github.com/pashky/restclient.el
;; Lots of helpful usage there.

;; Found restclient via:
;;   https://emacs.stackexchange.com/questions/2427/how-to-test-rest-api-with-emacs

;; Bare essentials blog post:
;;   https://jakemccrary.com/blog/2014/07/04/using-emacs-to-explore-an-http-api/

;; Once installed, you can prepare a text file with queries.
;;
;; restclient-mode is a major mode which does a bit of highlighting and supports a few keybinds:
;;
;; TODO-keybinds: Put these keybinds under the leader somewhere?
;;     C-c C-c: runs the query under the cursor, tries to pretty-print the response (if possible)
;;     C-c C-r: same, but doesn't do anything with the response, just shows the buffer
;;     C-c C-v: same as C-c C-c, but doesn't switch focus to other window
;;     C-c C-p: jump to the previous query
;;     C-c C-n: jump to the next query
;;     C-c C-.: mark the query under the cursor
;;     C-c C-u: copy query under the cursor as a curl command
;;     C-c C-g: start a helm session with sources for variables and requests (if helm is available, of course)
;;     C-c n n: narrow to region of current request (including headers)
;;     TAB: hide/show current request body, only if
;;     C-c C-a: show all collapsed regions
;;
;; See example files:
;;   - "mantle/config/dev-env/languages/http.example.restclient"
;;   - https://github.com/pashky/restclient.el/blob/master/examples/httpbin
(imp:use-package restclient
  ;;------------------------------
  :mode
  ;;------------------------------
  ;; Set some file extensions to use restclient in
  (("\\.http\\'"         . restclient-mode)
   ("\\.restclient\\'"   . restclient-mode)
   ;; restclient puts the response buffer into html mode automatically,
   ;; but sometimes I save that response as this extension.
   ("\\.restresponse\\'" . html-mode)))


;; TODO-autocomplete: Do I have a way to add to completions for... whatever I use for completions?
;; ;; auto-completion for company in restclient mode
;; (imp:use-package company-restclient
;;   :after (restclient company)

;;   ;;------------------------------
;;   :init
;;   ;;------------------------------
;;   (add-to-list 'company-backends 'company-restclient))


;; Org-Babel support for `restclient'
;; See their README for examples.
;; https://github.com/alf/ob-restclient.el
(imp:use-package ob-restclient
  ;;------------------------------
  :config
  ;;------------------------------
  (org-babel-do-load-languages 'org-babel-load-languages '((restclient . t))))



;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'config 'user 'dev-env 'languages 'http)

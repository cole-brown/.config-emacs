;;; emacs/str/init.el --- String Helpers -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2021-09-10
;; Modified:   2022-04-13
;;
;;; Commentary:
;;
;; Helpful string functions.
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Set up imp.
;;------------------------------------------------------------------------------

(imp:path:root/set :str
                   (imp:path:current:dir)
                   "init.el")


;;------------------------------------------------------------------------------
;; Set up custom vars.
;;------------------------------------------------------------------------------

(defgroup str:group nil
  "String manipulation functions."
  :prefix "str:"
  :group 'tools)


;;------------------------------------------------------------------------------
;; Load files.
;;------------------------------------------------------------------------------

(imp:timing
    :str
    "init.el"
    (imp:path:current:dir)

  (imp:load :feature  '(:str normalize)
            :filename "normalize")
  (imp:load :feature  '(:str regex)
            :filename "regex")
  (imp:load :feature  '(:str buffer)
            :filename "buffer")
  (imp:load :feature  '(:str string)
            :filename "string")
  (imp:load :feature  '(:str propertize)
            :filename "propertize")
  (imp:load :feature  '(:str hash)
            :filename "hash")

  ;; Requires 'normalize', 'regex', and 'string'.
  (unless (imp:flag? :str -case)
    (imp:load :feature  '(:str +case)
              :filename "+case")

    (unless (imp:flag? :str -hydra)
      (imp:load :feature  '(:str +hydra +case)
                :filename "+case-hydra")))

  ;; Requires 'string'.
  (unless (imp:flag? :str -random)
    (imp:load :feature  '(:str +random)
              :filename "+random"))

  ;; End load timing.
  )


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide:with-emacs :str)

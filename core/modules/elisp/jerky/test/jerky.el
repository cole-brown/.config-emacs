;;; jerky/test/jerky.el --- Tests for "jerky/jerky.el" -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2022-06-15
;; Modified:   2022-06-15
;; URL:        https://github.com/cole-brown/.config-emacs
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;  Tests for "jerky/jerky.el".
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Test Requirements
;;------------------------------------------------------------------------------

(imp:test:load :feature:post '(:jerky test base)
               :filename     "base")


;; ╔═════════════════════════════╤═══════════╤═════════════════════════════════╗
;; ╟─────────────────────────────┤ ERT TESTS ├─────────────────────────────────╢
;; ╠════════════════════╤════════╧═══════════╧════════╤════════════════════════╣
;; ╟────────────────────┤ Does the Jerky taste right? ├────────────────────────╢
;; ╚════════════════════╧═════════════════════════════╧════════════════════════╝


;;------------------------------------------------------------------------------
;; Tests: Jerky Key/Value Repo
;;------------------------------------------------------------------------------

;;------------------------------
;; int<jerky>:parse
;;------------------------------

(ert-deftest test<jerky/jerky>::int<jerky>:parse ()
  "Test that `int<jerky>:parse' behaves appropriately."
  (test<jerky>:fixture
      ;;===
      ;; Test name, setup & teardown func.
      ;;===
      "test<jerky/jerky>::int<jerky>:parse"
      nil
      nil

    ;;===
    ;; Run the test.
    ;;===

    (should (equal
             '("foo/bar/baz" :namespace qux :value 1 :docstr nil)
             (int<jerky>:parse '(foo bar baz :namespace qux :value 1)
                               ;; t means "default of `:namespace', `:value', and `:docstr', please"
                               t)))

    (should (equal
             '("foo/bar/baz" :namespace qux :value 1 :docstr nil :baz "hello")
             (int<jerky>:parse '(foo bar baz :namespace qux :value 1 :DNE nil :baz "hello")
                               ;; `:namespace', `:value', and `:docstr'...
                               ;;   ...plus `:baz' and `:DNE'.
                               t :baz :DNE)))))


;; TODO: test remaining functions:
;; int<jerky>:namespaces:create
;; int<jerky>:namespace:valid?
;; int<jerky>:namespace:entry/namespace:get
;; int<jerky>:namespace:entry/title:get
;; int<jerky>:namespace:entry/docstr:get
;; int<jerky>:namespace:entry/fallback:get
;; int<jerky>:namespace:entry:get
;; int<jerky>:namespace:entry:set
;; int<jerky>:namespace:set
;; jerky:namespace:create
;; jerky:namespace:has
;; int<jerky>:namespace/ordered
;; jerky:namespace:get
;; int<jerky>:key:normalize
;; jerky:key:string
;; int<jerky>:repo:get
;; int<jerky>:repo/key:get
;; int<jerky>:repo/record:get
;; int<jerky>:repo/record/namespace:get
;; int<jerky>:record/namespace:get
;; int<jerky>:record/value:get
;; int<jerky>:record/docstr:get
;; jerky:get
;; int<jerky>:repo/record/namespace:set
;; int<jerky>:repo/key:set
;; int<jerky>:repo/record:set
;; int<jerky>:repo:set
;; int<jerky>:repo/update
;; jerky:set
;; int<jerky>:search/filter
;; jerky:has


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :jerky 'test 'jerky)

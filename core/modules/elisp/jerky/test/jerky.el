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
             '("foo/bar/baz" :namespace qux :value 1 :docstr nil :DNE nil :baz "hello")
             (int<jerky>:parse '(foo bar baz :namespace qux :value 1 :baz "hello")
                               ;; `:namespace', `:value', and `:docstr'...
                               ;;   ...plus `:baz' and `:DNE'.
                               t :baz :DNE)))))


;;------------------------------
;; int<jerky>:namespace:valid?
;;------------------------------

(ert-deftest test<jerky/jerky>::int<jerky>:namespace:valid? ()
  "Test that `int<jerky>:namespace:valid?' behaves appropriately."
  (test<jerky>:fixture
      ;;===
      ;; Test name, setup & teardown func.
      ;;===
      "test<jerky/jerky>::int<jerky>:namespace:valid?"
      nil
      nil

    ;;===
    ;; Run the test.
    ;;===

    (should (int<jerky>:namespace:valid? :jeff))
    (should-not (int<jerky>:namespace:valid? 'jeff t))
    (should-error (int<jerky>:namespace:valid? 'jeff))))


;;------------------------------
;; int<jerky>:namespace:entry:get
;;------------------------------

(ert-deftest test<jerky/jerky>::int<jerky>:namespace:entry:get ()
  "Test that `int<jerky>:namespace:entry:get' behaves appropriately."
  (test<jerky>:fixture
      ;;===
      ;; Test name, setup & teardown func.
      ;;===
      "test<jerky/jerky>::int<jerky>:namespace:entry:get"
      nil
      nil

    ;;===
    ;; Run the test.
    ;;===

    (let ((entry (int<jerky>:namespace:entry:get :default)))
      (should entry)
      (should (listp entry))

      (should (keywordp (nth 0 entry)))
      (should (stringp (nth 1 entry)))
      (should (stringp (nth 2 entry)))
      (should (listp (nth 3 entry)))

      (should (eq :default
                  (nth 0 entry)))
      (should (string= "Default/Fallback Namespace"
                       (nth 1 entry)))
      (should (string= "Default namespace for jerky. Other namespaces default to this for fallbacks."
                       (nth 2 entry)))
      (should (eq 1
                  (length (nth 3 entry))))
      (should (eq :no-fallback
                  (nth 0 (nth 3 entry)))))))


;;------------------------------
;; int<jerky>:namespace:entry/namespace:get
;;------------------------------

(ert-deftest test<jerky/jerky>::int<jerky>:namespace:entry/namespace:get ()
  "Test that `int<jerky>:namespace:entry/namespace:get' behaves appropriately."
  (test<jerky>:fixture
      ;;===
      ;; Test name, setup & teardown func.
      ;;===
      "test<jerky/jerky>::int<jerky>:namespace:entry/namespace:get"
      nil
      nil

    ;;===
    ;; Run the test.
    ;;===

    (let ((entry (int<jerky>:namespace:entry:get :default)))
      (should entry)
      (should (listp entry))

      (should (keywordp (nth 0 entry)))

      (should (eq :default
                  (nth 0 entry)))

      (should (eq :default
                  (int<jerky>:namespace:entry/namespace:get entry))))))


;;------------------------------
;; int<jerky>:namespace:entry/title:get
;;------------------------------

(ert-deftest test<jerky/jerky>::int<jerky>:namespace:entry/title:get ()
  "Test that `int<jerky>:namespace:entry/title:get' behaves appropriately."
  (test<jerky>:fixture
      ;;===
      ;; Test name, setup & teardown func.
      ;;===
      "test<jerky/jerky>::int<jerky>:namespace:entry/title:get"
      nil
      nil

    ;;===
    ;; Run the test.
    ;;===

    (let ((entry (int<jerky>:namespace:entry:get :default)))
      (should entry)
      (should (listp entry))

      (should (stringp (nth 1 entry)))

      (should (string= "Default/Fallback Namespace"
                       (nth 1 entry)))

      (should (string= "Default/Fallback Namespace"
                       (int<jerky>:namespace:entry/title:get entry))))))


;;------------------------------
;; int<jerky>:namespace:entry/docstr:get
;;------------------------------

(ert-deftest test<jerky/jerky>::int<jerky>:namespace:entry/docstr:get ()
  "Test that `int<jerky>:namespace:entry/docstr:get' behaves appropriately."
  (test<jerky>:fixture
      ;;===
      ;; Test name, setup & teardown func.
      ;;===
      "test<jerky/jerky>::int<jerky>:namespace:entry/docstr:get"
      nil
      nil

    ;;===
    ;; Run the test.
    ;;===

    (let ((entry (int<jerky>:namespace:entry:get :default)))
      (should entry)
      (should (listp entry))

      (should (stringp (nth 2 entry)))

      (should (string= "Default namespace for jerky. Other namespaces default to this for fallbacks."
                       (nth 2 entry)))

      (should (string= "Default namespace for jerky. Other namespaces default to this for fallbacks."
                       (int<jerky>:namespace:entry/docstr:get entry))))))


;;------------------------------
;; int<jerky>:namespace:entry/fallback:get
;;------------------------------

(ert-deftest test<jerky/jerky>::int<jerky>:namespace:entry/fallback:get ()
  "Test that `int<jerky>:namespace:entry/fallback:get' behaves appropriately."
  (test<jerky>:fixture
      ;;===
      ;; Test name, setup & teardown func.
      ;;===
      "test<jerky/jerky>::int<jerky>:namespace:entry/fallback:get"
      nil
      nil

    ;;===
    ;; Run the test.
    ;;===

    (let ((entry (int<jerky>:namespace:entry:get :default)))
      (should entry)
      (should (listp entry))

      (should (listp (nth 3 entry)))
      (should (listp (int<jerky>:namespace:entry/fallback:get entry)))

      (should (eq 1
                  (length (nth 3 entry))))
      (should (eq 1
                  (length (int<jerky>:namespace:entry/fallback:get entry))))

      (should (eq :no-fallback
                  (nth 0 (nth 3 entry))))
      (should (eq :no-fallback
                  (nth 0 (int<jerky>:namespace:entry/fallback:get entry)))))))


;;------------------------------
;; int<jerky>:namespace:entry:set
;;------------------------------

(ert-deftest test<jerky/jerky>::int<jerky>:namespace:entry:set ()
  "Test that `int<jerky>:namespace:entry:set' behaves appropriately."
  (test<jerky>:fixture
      ;;===
      ;; Test name, setup & teardown func.
      ;;===
      "test<jerky/jerky>::int<jerky>:namespace:entry:set"
      nil
      nil

    ;;===
    ;; Run the test.
    ;;===

    (let ((namespace :test)
          (title "Test Namespace Title")
          (docstr "Hello there.")
          (fallbacks '(:foo :default))
          entry)

      (setq entry (int<jerky>:namespace:entry:set namespace title docstr fallbacks))

      (should entry)
      (should (listp entry))

      (should (eq namespace
                  (int<jerky>:namespace:entry/namespace:get entry)))

      (should (string= title
                       (int<jerky>:namespace:entry/title:get entry)))

      (should (string= docstr
                       (int<jerky>:namespace:entry/docstr:get entry)))

      (should (listp (int<jerky>:namespace:entry/fallback:get entry)))
      (should (= 2 (length (int<jerky>:namespace:entry/fallback:get entry))))
      (should (eq :foo
                  (nth 0 (int<jerky>:namespace:entry/fallback:get entry))))
      (should (eq :default
                  (nth 1 (int<jerky>:namespace:entry/fallback:get entry)))))))




;; TODO: test remaining functions:
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

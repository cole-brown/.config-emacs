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


;;------------------------------
;; int<jerky>:namespace:set
;;------------------------------

(ert-deftest test<jerky/jerky>::int<jerky>:namespace:set ()
  "Test that `int<jerky>:namespace:set' behaves appropriately."
  (test<jerky>:fixture
      ;;===
      ;; Test name, setup & teardown func.
      ;;===
      "test<jerky/jerky>::int<jerky>:namespace:set"
      nil
      nil

    ;;===
    ;; Run the test.
    ;;===

    ;;------------------------------
    ;; Test create.
    ;;------------------------------
    (let* ((namespace :test:00)
           (title "Test Namespace Title 00")
           (docstr "Hello there, 00.")
           (fallbacks '(:default))
           (entry (int<jerky>:namespace:entry:set namespace title docstr fallbacks))
           entry/updated
           entry/get)

      (setq entry/updated (int<jerky>:namespace:set entry))
      (should entry/updated)
      (should (listp entry/updated))

      (setq entry/get (int<jerky>:namespace:entry:get namespace))
      (should entry/get)
      (should (listp entry/get))

      (should (eq namespace
                  (int<jerky>:namespace:entry/namespace:get entry/updated)))
      (should (eq (int<jerky>:namespace:entry/namespace:get entry/updated)
                  (int<jerky>:namespace:entry/namespace:get entry/get)))

      (should (string= title
                       (int<jerky>:namespace:entry/title:get entry/updated)))
      (should (string= (int<jerky>:namespace:entry/title:get entry/updated)
                       (int<jerky>:namespace:entry/title:get entry/get)))

      (should (string= docstr
                       (int<jerky>:namespace:entry/docstr:get entry/updated)))
      (should (string= (int<jerky>:namespace:entry/docstr:get entry/updated)
                       (int<jerky>:namespace:entry/docstr:get entry/get)))

      (should (listp (int<jerky>:namespace:entry/fallback:get entry/updated)))
      (should (listp (int<jerky>:namespace:entry/fallback:get entry/get)))
      (should (= 1 (length (int<jerky>:namespace:entry/fallback:get entry/updated))))
      (should (= 1 (length (int<jerky>:namespace:entry/fallback:get entry/get))))
      (should (eq :default
                  (nth 0 (int<jerky>:namespace:entry/fallback:get entry/updated))))
      (should (eq :default
                  (nth 0 (int<jerky>:namespace:entry/fallback:get entry/get)))))

    ;; Need another one so we can have fallbacks...
    (let* ((namespace :test:01)
           (title "Test Namespace Title 01")
           (docstr "Hello there, 01.")
           (fallbacks '(:default))
           (entry (int<jerky>:namespace:entry:set namespace title docstr fallbacks))
           entry/updated)

      (setq entry/updated (int<jerky>:namespace:set entry))
      (should entry/updated)
      (should (listp entry/updated))

      (setq entry/get (int<jerky>:namespace:entry:get namespace))
      (should entry/get)
      (should (listp entry/get))

      (should (eq namespace
                  (int<jerky>:namespace:entry/namespace:get entry/updated)))
      (should (eq (int<jerky>:namespace:entry/namespace:get entry/updated)
                  (int<jerky>:namespace:entry/namespace:get entry/get)))

      (should (string= title
                       (int<jerky>:namespace:entry/title:get entry/updated)))
      (should (string= (int<jerky>:namespace:entry/title:get entry/updated)
                       (int<jerky>:namespace:entry/title:get entry/get)))

      (should (string= docstr
                       (int<jerky>:namespace:entry/docstr:get entry/updated)))
      (should (string= (int<jerky>:namespace:entry/docstr:get entry/updated)
                       (int<jerky>:namespace:entry/docstr:get entry/get)))

      (should (listp (int<jerky>:namespace:entry/fallback:get entry/updated)))
      (should (listp (int<jerky>:namespace:entry/fallback:get entry/get)))
      (should (= 1 (length (int<jerky>:namespace:entry/fallback:get entry/updated))))
      (should (= 1 (length (int<jerky>:namespace:entry/fallback:get entry/get))))
      (should (eq :default
                  (nth 0 (int<jerky>:namespace:entry/fallback:get entry/updated))))
      (should (eq :default
                  (nth 0 (int<jerky>:namespace:entry/fallback:get entry/get)))))

    ;;------------------------------
    ;; Test update/overwrite.
    ;;------------------------------
    (let* ((namespace :test:00)
           (title "New Test Namespace Title")
           (docstr "New Docstr.")
           (fallbacks '(:test:01 :default))
           (entry (int<jerky>:namespace:entry:set namespace title docstr fallbacks))
           entry/updated
           entry/get)

      (setq entry/updated (int<jerky>:namespace:set entry))
      (should entry/updated)
      (should (listp entry/updated))

      (setq entry/get (int<jerky>:namespace:entry:get namespace))
      (should entry/get)
      (should (listp entry/get))

      (should (eq namespace
                  (int<jerky>:namespace:entry/namespace:get entry/updated)))
      (should (eq (int<jerky>:namespace:entry/namespace:get entry/updated)
                  (int<jerky>:namespace:entry/namespace:get entry/get)))

      (should (string= title
                       (int<jerky>:namespace:entry/title:get entry/updated)))
      (should (string= (int<jerky>:namespace:entry/title:get entry/updated)
                       (int<jerky>:namespace:entry/title:get entry/get)))

      (should (string= docstr
                       (int<jerky>:namespace:entry/docstr:get entry/updated)))
      (should (string= (int<jerky>:namespace:entry/docstr:get entry/updated)
                       (int<jerky>:namespace:entry/docstr:get entry/get)))

      (should (listp (int<jerky>:namespace:entry/fallback:get entry/updated)))
      (should (listp (int<jerky>:namespace:entry/fallback:get entry/get)))
      (should (= 2 (length (int<jerky>:namespace:entry/fallback:get entry/updated))))
      (should (= 2 (length (int<jerky>:namespace:entry/fallback:get entry/get))))
      (should (eq :test:01
                  (nth 0 (int<jerky>:namespace:entry/fallback:get entry/updated))))
      (should (eq :test:01
                  (nth 0 (int<jerky>:namespace:entry/fallback:get entry/get))))
      (should (eq :default
                  (nth 1 (int<jerky>:namespace:entry/fallback:get entry/updated))))
      (should (eq :default
                  (nth 1 (int<jerky>:namespace:entry/fallback:get entry/get)))))

    ;;------------------------------
    ;; Test delete.
    ;;------------------------------
    (let* ((namespace :test:00)
           (title "New Test Namespace Title")
           (docstr "New Docstr.")
           (fallbacks '(:default))
           (entry (int<jerky>:namespace:entry:set namespace title docstr fallbacks))
           entry/updated
           entry/get)

      ;;---
      ;; Check returned entry and also entry from get.
      ;;---
      ;; DELETE!!!
      (setq entry/updated (int<jerky>:namespace:set entry int<jerky>:action/delete))
      (should-not entry/updated)

      (setq entry/get (int<jerky>:namespace:entry:get namespace))
      (should-not entry/get))

    ;;------------------------------
    ;; Test create without fallbacks.
    ;;------------------------------
    ;; Should get `:default' as the fallback.
    (let* ((namespace :test:nil-fallbacks)
           (title "Test Nil Fallbacks")
           (docstr "null docstr")
           (fallbacks nil)
           (entry (int<jerky>:namespace:entry:set namespace title docstr fallbacks))
           entry/updated
           entry/get)

      (setq entry/updated (int<jerky>:namespace:set entry))
      (should entry/updated)
      (should (listp entry/updated))

      (setq entry/get (int<jerky>:namespace:entry:get namespace))
      (should entry/get)
      (should (listp entry/get))

      (should (eq namespace
                  (int<jerky>:namespace:entry/namespace:get entry/updated)))

      (should (string= title
                       (int<jerky>:namespace:entry/title:get entry/updated)))
      (should (string= (int<jerky>:namespace:entry/title:get entry/updated)
                       (int<jerky>:namespace:entry/title:get entry/get)))

      (should (string= docstr
                       (int<jerky>:namespace:entry/docstr:get entry/updated)))
      (should (string= (int<jerky>:namespace:entry/docstr:get entry/updated)
                       (int<jerky>:namespace:entry/docstr:get entry/get)))

      (should (listp (int<jerky>:namespace:entry/fallback:get entry/updated)))
      (should (listp (int<jerky>:namespace:entry/fallback:get entry/get)))
      (should (= 1 (length (int<jerky>:namespace:entry/fallback:get entry/updated))))
      (should (= 1 (length (int<jerky>:namespace:entry/fallback:get entry/get))))
      (should (eq :default
                  (nth 0 (int<jerky>:namespace:entry/fallback:get entry/updated))))
      (should (eq :default
                  (nth 0 (int<jerky>:namespace:entry/fallback:get entry/get)))))))


;;------------------------------
;; jerky:namespace:create
;;------------------------------

(ert-deftest test<jerky/jerky>::jerky:namespace:create ()
  "Test that `jerky:namespace:create' behaves appropriately."
  (test<jerky>:fixture
      ;;===
      ;; Test name, setup & teardown func.
      ;;===
      "test<jerky/jerky>::jerky:namespace:create"
      nil
      nil

    ;;===
    ;; Run the test.
    ;;===

    ;;------------------------------
    ;; Create w/o fallbacks.
    ;;------------------------------
    (let* ((namespace :test:00)
           (title "Zeroith Namespace Title")
           (docstr "Hello there, Double Ought.")
           (fallbacks '(:default)) ;; What we want as fallbacks from not supplying any fallbacks.
           entry/created)

      ;; Don't even supply `:fallbacks' - should get `:default'
      (setq entry/created (jerky:namespace:create namespace
                                                  :title title
                                                  :docstr docstr))
      (should entry/created)
      (should (listp entry/created))

      (should (eq namespace
                  (int<jerky>:namespace:entry/namespace:get entry/created)))

      (should (string= title
                       (int<jerky>:namespace:entry/title:get entry/created)))

      (should (string= docstr
                       (int<jerky>:namespace:entry/docstr:get entry/created)))

      (should (listp (int<jerky>:namespace:entry/fallback:get entry/created)))
      (should (= 1 (length (int<jerky>:namespace:entry/fallback:get entry/created))))
      (should (eq :default
                  (nth 0 (int<jerky>:namespace:entry/fallback:get entry/created)))))

    ;;------------------------------
    ;; Create w/ nil fallbacks.
    ;;------------------------------
    (let* ((namespace :test:01)
           (title "First Namespace Title")
           (docstr "Hello there, Number One.")
           (fallbacks nil)
           entry/created)

      (setq entry/created (jerky:namespace:create namespace
                                                  :title title
                                                  :docstr docstr
                                                  :fallbacks fallbacks))
      (should entry/created)
      (should (listp entry/created))

      (should (eq namespace
                  (int<jerky>:namespace:entry/namespace:get entry/created)))

      (should (string= title
                       (int<jerky>:namespace:entry/title:get entry/created)))

      (should (string= docstr
                       (int<jerky>:namespace:entry/docstr:get entry/created)))

      (should (listp (int<jerky>:namespace:entry/fallback:get entry/created)))
      (should (= 1 (length (int<jerky>:namespace:entry/fallback:get entry/created))))
      (should (eq :default
                  (nth 0 (int<jerky>:namespace:entry/fallback:get entry/created)))))


    ;;------------------------------
    ;; Create w/ fallbacks.
    ;;------------------------------
    (let* ((namespace :test:01)
           (title "First Namespace Title")
           (docstr "Hello there, Number One.")
           (fallbacks '(nil :foo nil :bar nil)) ;; nils should get filtered out.
           entry/created)

      (setq entry/created (jerky:namespace:create namespace
                                                  :title title
                                                  :docstr docstr
                                                  :fallbacks fallbacks))
      (should entry/created)
      (should (listp entry/created))

      (should (eq namespace
                  (int<jerky>:namespace:entry/namespace:get entry/created)))

      (should (string= title
                       (int<jerky>:namespace:entry/title:get entry/created)))

      (should (string= docstr
                       (int<jerky>:namespace:entry/docstr:get entry/created)))

      (should (listp (int<jerky>:namespace:entry/fallback:get entry/created)))
      (should (= 2 (length (int<jerky>:namespace:entry/fallback:get entry/created))))
      (should (eq :foo
                  (nth 0 (int<jerky>:namespace:entry/fallback:get entry/created))))
      (should (eq :bar
                  (nth 1 (int<jerky>:namespace:entry/fallback:get entry/created)))))))


;;------------------------------
;; jerky:namespace:has
;;------------------------------

(ert-deftest test<jerky/jerky>::jerky:namespace:has ()
  "Test that `jerky:namespace:has' behaves appropriately."
  (test<jerky>:fixture
      ;;===
      ;; Test name, setup & teardown func.
      ;;===
      "test<jerky/jerky>::jerky:namespace:has"
      nil
      nil

    ;;===
    ;; Run the test.
    ;;===

    ;;------------------------------
    ;; Create namespaces to test for.
    ;;------------------------------
    (should (jerky:namespace:create :test:00
                                    :title "title 00"
                                    :docstr "docstr 00"))
    (should (jerky:namespace:create :test:01
                                    :title "title 01"
                                    :docstr "docstr 01"))

    ;;------------------------------
    ;; Test existance of namespaces.
    ;;------------------------------
    (should (jerky:namespace:has :test:00))
    (should (jerky:namespace:has :test:01))
    (should (jerky:namespace:has :default))

    (should-not (jerky:namespace:has :foo))
    (should-not (jerky:namespace:has :bar))
    (should-not (jerky:namespace:has :test:02))))


;;------------------------------
;; int<jerky>:namespace/ordered
;;------------------------------

(ert-deftest test<jerky/jerky>::int<jerky>:namespace/ordered ()
  "Test that `int<jerky>:namespace/ordered' behaves appropriately."
  (test<jerky>:fixture
      ;;===
      ;; Test name, setup & teardown func.
      ;;===
      "test<jerky/jerky>::int<jerky>:namespace/ordered"
      nil
      nil

    ;;===
    ;; Run the test.
    ;;===

    ;; Non-existant namespaces can't be fallbacks.
    (should-error (jerky:namespace:create :test:dne
                                    :title "title 02"
                                    :docstr "docstr 02"
                                    :fallbacks '(:foo :bar)))

    ;;------------------------------
    ;; Create namespaces to test for.
    ;;------------------------------
    ;; Just default fallback.
    (should (jerky:namespace:create :test:00
                                    :title "title 00"
                                    :docstr "docstr 00"))

    ;; Multiple fallbacks: :test:00 & :default
    (should (jerky:namespace:create :test:01
                                    :title "title 01"
                                    :docstr "docstr 01"
                                    :fallbacks '(:test:00)))

    (should (jerky:namespace:create :test:02
                                    :title "title 02"
                                    :docstr "docstr 02"
                                    :fallbacks (list jerky:namespace/no-fallback)))

    ;; Cascading fallbacks.
    (should (jerky:namespace:create :test:03/with-default
                                    :title "title 03"
                                    :docstr "docstr 03"
                                    :fallbacks '(:test:02 :test:01 :default)))

    (should (jerky:namespace:create :test:03/no-default
                                    :title "title 03"
                                    :docstr "docstr 03"
                                    :fallbacks '(:test:02 :test:01)))

    ;;------------------------------
    ;; Test Fallback Ordering.
    ;;------------------------------

    ;; If namespace isn't found, we should get the default.
    (should (equal '(:default)
                   (int<jerky>:namespace/ordered :does-not-exist)))

    ;; The rest should exist and so returned ordered list should start with the
    ;; requested namespace.

    ;; This is the default: a namespace and `:default' fallback.
    (should (equal '(:test:00 :default)
                   (int<jerky>:namespace/ordered :test:00)))

    ;; These are non-defaults.
    (should (equal '(:test:01 :test:00 :default)
                   (int<jerky>:namespace/ordered :test:01)))
    (should (equal '(:test:02)
                   (int<jerky>:namespace/ordered :test:02)))
    (should (equal '(:test:03/with-default :test:02 :test:01 :test:00 :default)
                   (int<jerky>:namespace/ordered :test:03/with-default)))
    (should (equal '(:test:03/no-default :test:02 :test:01 :test:00)
                   (int<jerky>:namespace/ordered :test:03/no-default)))))


;;------------------------------
;; int<jerky>:key:normalize
;;------------------------------

(ert-deftest test<jerky/jerky>::int<jerky>:key:normalize ()
  "Test that `int<jerky>:key:normalize' behaves appropriately."
  (test<jerky>:fixture
      ;;===
      ;; Test name, setup & teardown func.
      ;;===
      "test<jerky/jerky>::int<jerky>:key:normalize"
      nil
      nil

    ;;===
    ;; Run the test.
    ;;===

    ;; nil remains nil...
    (should-not (int<jerky>:key:normalize nil))

    ;; ...but a list with nil in it becomes empty string.
    (should (string= ""
                     (int<jerky>:key:normalize '(nil))))

    (should (string= "a/b"
                     (int<jerky>:key:normalize "a/b")))
    (should (string= (int<jerky>:key:normalize "a/b")
                     (int<jerky>:key:normalize '("a/b"))))

    (should (string= "a/b/c"
                     (int<jerky>:key:normalize '("a/b" "c"))))
    (should (string= "base/a/b/c"
                     (int<jerky>:key:normalize '(:base "a/b" "c"))))))


;;------------------------------
;; jerky:key:string
;;------------------------------

(ert-deftest test<jerky/jerky>::jerky:key:string ()
  "Test that `jerky:key:string' behaves appropriately."
  (test<jerky>:fixture
      ;;===
      ;; Test name, setup & teardown func.
      ;;===
      "test<jerky/jerky>::jerky:key:string"
      nil
      nil

    ;;===
    ;; Run the test.
    ;;===

    (should (string= ""
                     (jerky:key:string nil)))

    (should (string= "a/b"
                     (jerky:key:string "a/b")))
    (should (string= (jerky:key:string "a/b")
                     (jerky:key:string '("a/b"))))

    (should (string= "a/b/c"
                     (jerky:key:string '("a/b" "c"))))
    (should (string= "base/a/b/c"
                     (jerky:key:string '(:base "a/b" "c"))))))




;; TODO: test remaining functions:
;; int<jerky>:repo:get
;; int<jerky>:repo/key:get
;; int<jerky>:repo/record:get
;; int<jerky>:repo/record/namespace:get
;; int<jerky>:record/namespace:get
;; int<jerky>:record/value:get
;; int<jerky>:record/docstr:get
;; jerky:get
;; jerky:namespace:get
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

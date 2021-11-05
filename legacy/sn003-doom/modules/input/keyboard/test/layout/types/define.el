;; -*- no-byte-compile: t; lexical-binding: t; -*-
;;; input/keyboard/test/layout/define.el

;;------------------------------------------------------------------------------
;; Test Layout
;;------------------------------------------------------------------------------

;;---
;; Test Files:
;;---
;; "test/layout/base.el" will load "test/base.el" and all tested files from "test/__.el" level.
(load! "base.el")

;;---
;; Keyboard Layout Files:
;;---
(test<keyboard>:utils/path:load "layout/types/define.el")


;;------------------------------------------------------------------------------
;; Set-Up / Tear-Down
;;------------------------------------------------------------------------------

(defun test<keyboard/layout/define>:setup (_)
  "Set-up for 'layout/define.el' tests."
  ;; Start off without any keywords defined.
  (setq int<keyboard>:layout/types:keywords nil))


(defun test<keyboard/layout/define>:teardown (test-name)
  "Tear-down for 'layout/define.el' tests.")


;; ╔═════════════════════════════╤═══════════╤═════════════════════════════════╗
;; ╟─────────────────────────────┤ ERT TESTS ├─────────────────────────────────╢
;; ╠═════════════════════╤═══════╧═══════════╧════════╤════════════════════════╣
;; ╟─────────────────────┤ Input: Keyboards & Layouts ├────────────────────────╢
;; ╚═════════════════════╧════════════════════════════╧════════════════════════╝


;;------------------------------------------------------------------------------
;; Tests: Define Keyboard Layout Type Keywords
;;------------------------------------------------------------------------------

;;------------------------------
;; int<keyboard>:layout/types:normalize->func
;;------------------------------

(ert-deftest test<keyboard>::int<keyboard>:layout/types:normalize->func ()
  "Test that `int<keyboard>:layout/types:normalize->func' behaves appropriately."
  (test<keyboard>:fixture
      ;;===
      ;; Test name, setup & teardown func.
      ;;===
      "test<keyboard/alist>::int<keyboard>:layout/types:normalize->func"
      ;; Clear out keybinds before test.
      #'test<keyboard/layout/define>:setup
      #'test<keyboard/layout/define>:teardown


    ;;===
    ;; Run the test.
    ;;===
    ;; Shouldn't have anything defined yet.
    (should-not int<keyboard>:layout/types:keywords)

    ;;------------------------------
    ;; Function -> Function
    ;;------------------------------

    (should (eq #'ignore
                (int<keyboard>:layout/types:normalize->func #'ignore)))

    (let ((func #'ignore))
      (should (eq func
                  (int<keyboard>:layout/types:normalize->func func))))

    (let ((func #'does-not-exist-yet))
      (should (eq func
                  (int<keyboard>:layout/types:normalize->func func))))


    ;;------------------------------
    ;; Keyword -> Function
    ;;------------------------------

    ;; Create some keyword->func pairs...
    ;; Some for real functions, some for funcs that don't exist (yet).
    (setq int<keyboard>:layout/types:keywords '((:dne-0    . #'does-not-exist-yet)
                                                (:dne-1    . #'hello-there)
                                                (:exists-0 . #'ignore)
                                                (:exists-1 . #'identity)))
    ;; Make sure the alist is what we're expecting.
    (let ((keyword/assoc (assoc :dne-0 int<keyboard>:layout/types:keywords)))
      (should (eq :dne-0
                  (nth 0 keyword/assoc)))
      (should (eq 'function
                  (nth 1 keyword/assoc)))
      (should (eq 'does-not-exist-yet
                  (nth 2 keyword/assoc))))

    ;; Make sure our not-functions and functions are as they should be.
    (should-error (funcall (nth 1 (alist-get :dne-0 int<keyboard>:layout/types:keywords))))
    (should-error (funcall (nth 1 (alist-get :dne-1 int<keyboard>:layout/types:keywords))))
    ;; Make sure our functions don't error or anything eggregious.
    (should-not (funcall (nth 1 (alist-get :exists-0 int<keyboard>:layout/types:keywords))))
    (should (eq :multipass
                (funcall (nth 1 (alist-get :exists-1 int<keyboard>:layout/types:keywords)) :multipass)))

    (should (equal '(function does-not-exist-yet)
                   (int<keyboard>:layout/types:normalize->func :dne-0)))
    (should (equal '(function hello-there)
                   (int<keyboard>:layout/types:normalize->func :dne-1)))
    (should (equal '(function ignore)
                   (int<keyboard>:layout/types:normalize->func :exists-0)))
    (should (equal '(function identity)
                   (int<keyboard>:layout/types:normalize->func :exists-1)))

    ;; Make sure that trying to normalize an unknown keyword is an error.
    (should-error (int<keyboard>:layout/types:normalize->func :sir-not-appearing-in-this-alist))))


;;------------------------------
;; int<keyboard>:layout/types:valid/keyword?
;;   - (and int<keyboard>:layout/types:keyword/valid-name-regex)
;;------------------------------

(ert-deftest test<keyboard>::int<keyboard>:layout/types:valid/keyword? ()
  "Test that `int<keyboard>:layout/types:valid/keyword?' behaves appropriately."
  (test<keyboard>:fixture
      ;;===
      ;; Test name, setup & teardown func.
      ;;===
      "test<keyboard/alist>::int<keyboard>:layout/types:valid/keyword?"
      #'test<keyboard/layout/define>:setup
      #'test<keyboard/layout/define>:teardown


    ;;===
    ;; Run the test.
    ;;===
    (let ((var/invalid/type    :invalid)
          (var/invalid/keyword :layout:invalid-keyword)
          (var/valid/type      :common)
          (var/valid/type-2    :emacs)
          (var/valid/keyword   :layout:common:valid-for-common)
          (var/valid/keyword-2 :layout:emacs:valid-for-emacs))

    ;;------------------------------
    ;; 'Not a (Valid) Type' == invalid
    ;;------------------------------
    (should-not (int<keyboard>:layout/types:valid/keyword? t :layout:evil:test))
    (should-not (int<keyboard>:layout/types:valid/keyword? nil :layout:common:thing))
    (should-not (int<keyboard>:layout/types:valid/keyword? ':evil :layout:common:thing))

    ;;------------------------------
    ;; 'Not a (Valid) Keyword' == invaild
    ;;------------------------------

    (should-not (int<keyboard>:layout/types:valid/keyword? :evil t))
    (should-not (int<keyboard>:layout/types:valid/keyword? :evil nil))
    (should-not (int<keyboard>:layout/types:valid/keyword? :evil ':quoted-keyword/invalid))
    (should-not (int<keyboard>:layout/types:valid/keyword? :evil ':layout:evil:quoted-keyword/valid))
    (let ((var/invalid/keyword :layout:invalid-keyword)
          (var/valid/type :common))



    ;;------------------------------
    ;; Is our keyword validator valid?
    ;;------------------------------


    (let ((func #'ignore))
      (should (eq func
                  (int<keyboard>:layout/types:valid/keyword? func))))

    (let ((func #'does-not-exist-yet))
      (should (eq func
                  (int<keyboard>:layout/types:valid/keyword? func))))


    ;;------------------------------
    ;; Keyword -> Function
    ;;------------------------------

    ;; Create some keyword->func pairs...
    ;; Some for real functions, some for funcs that don't exist (yet).
    (setq int<keyboard>:layout/types:keywords '((:dne-0    . #'does-not-exist-yet)
                                                (:dne-1    . #'hello-there)
                                                (:exists-0 . #'ignore)
                                                (:exists-1 . #'identity)))
    ;; Make sure the alist is what we're expecting.
    (let ((keyword/assoc (assoc :dne-0 int<keyboard>:layout/types:keywords)))
      (should (eq :dne-0
                  (nth 0 keyword/assoc)))
      (should (eq 'function
                  (nth 1 keyword/assoc)))
      (should (eq 'does-not-exist-yet
                  (nth 2 keyword/assoc))))

    ;; Make sure our not-functions and functions are as they should be.
    (should-error (funcall (nth 1 (alist-get :dne-0 int<keyboard>:layout/types:keywords))))
    (should-error (funcall (nth 1 (alist-get :dne-1 int<keyboard>:layout/types:keywords))))
    ;; Make sure our functions don't error or anything eggregious.
    (should-not (funcall (nth 1 (alist-get :exists-0 int<keyboard>:layout/types:keywords))))
    (should (eq :multipass
                (funcall (nth 1 (alist-get :exists-1 int<keyboard>:layout/types:keywords)) :multipass)))

    (should (equal '(function does-not-exist-yet)
                   (int<keyboard>:layout/types:valid/keyword? :dne-0)))
    (should (equal '(function hello-there)
                   (int<keyboard>:layout/types:valid/keyword? :dne-1)))
    (should (equal '(function ignore)
                   (int<keyboard>:layout/types:valid/keyword? :exists-0)))
    (should (equal '(function identity)
                   (int<keyboard>:layout/types:valid/keyword? :exists-1)))

    ;; Make sure that trying to normalize an unknown keyword is an error.
    (should-error (int<keyboard>:layout/types:valid/keyword? :sir-not-appearing-in-this-alist))))

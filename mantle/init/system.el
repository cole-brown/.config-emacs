;;; mantle/init/system.el --- System Initialization -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2022-06-09
;; Timestamp:  2024-10-02
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; System Initialization
;; ┌──────────────┐ Init & Config Help for Multiple Systems ┌──────────────┐
;; │                     What computer is this anyways?                    │
;; └──────────────────────┘ (probably the wrong one) └─────────────────────┘
;;
;;; Code:


(imp:require :system 'multiplexer)


;;------------------------------------------------------------------------------
;; /This/ System Right Now.
;;------------------------------------------------------------------------------
;; Find the Answer to the Ultimate Question:
;;----------
;; "Where the hell am I?", obviously.
;; Or maybe "Who the hell am I?"...

;; Set our system's hash.
(system:multiplexer:hash)
;; (system:multiplexer:get :hash 'this)
;; (jerky:get 'system 'hash)

(nub:debug
 :innit
 (imp:path:current:file/relative :root)
 '(:init :system :multiplexer :secret)
 "This system's hash is: %S (== %S)"
 (system:multiplexer:hash)
 (system:secret:hash))


;;------------------------------------------------------------------------------
;; All Systems: Define Systems
;;------------------------------------------------------------------------------

;;------------------------------
;; home/2017/desk
;;   Host OS:  Windows 10
;;   Guests:   Misc Linux via WSL2
;;------------------------------

;;---
;; home/2017/desk::ab48e5-886ff
;;   via WSL2
;;---
(system:multiplexer:define :hash "ab48e5-886ff0"
                           :domain "home"
                           :date "2017"
                           :type "desk"
                           :description "(Windows/WSL) Home desktop PC built in 2017."
                           :path/secret/root "~/.config/spydez/secret")

;;---
;; home/2017/desk::02d29a-8bdef1
;;   via Windows 10
;;   (formerly known as: home/2017/desk::5730ce-91e149)
;;---
(system:multiplexer:define :hash "02d29a-8bdef1" ;; Before I renamed it: "5730ce-91e149"
                           :domain "home"
                           :date "2017"
                           :type "desk"
                           :description "(Windows) Home desktop PC built in 2017."
                           :path/secret/root "~/.secret.d")


;;------------------------------
;; work/2021/lap
;;   Host OS:  Ubuntu 20.04 LTS
;;   Guests:   Windows 10
;;------------------------------

;;---
;; work/2021/lap::e29fbd-0d21c7
;;  Host OS: Ubuntu 20.04
;;---
(system:multiplexer:define :hash "e29fbd-0d21c7"
                           :domain "work"
                           :date "2021"
                           :type "lap"
                           :description "Dell XPS 13 laptop"
                           :path/secret/root "~/ocean/vault/.config/secret")

;;---
;; work/2021/vm::d3dbd4-fa7934
;;   Guest VM: Windows 10
;;     via Ubuntu 20.04
;;---
(system:multiplexer:define :hash "d3dbd4-fa7934"
                           :domain "work"
                           :date "2021"
                           :type "vm"
                           :description "Windows 10 VM"
                           :path/secret/root (path:abs user-emacs-directory ".." ".secret.d"))


;;------------------------------
;; NOTE: New Systems!
;;------------------------------

;; Open Emacs. Check "mis:init:messages" or "*Messages*" buffer. You should see something like:
;;   > Warning (emacs): [WARNING ]: system:secret:init: [SKIP]: Cannot load secret ’init’; invalid system secrets.
;;   >                                 Validation Result:
;;   >           :success nil
;;   >            :reason "Secrets root directory for this system (work/2021/lap::e29fbd-0d21c7) is invalid: nil"
;;   >              :hash "e29fbd-0d21c7"
;;   >                :id "work/2021/lap::e29fbd-0d21c7"
;;   >     :path/dir/root nil
;;   >     :path/dir/load nil
;;   >   :path/dir/system nil
;;   >    :path/file/load "init"
;;   >    :path/file/name "init.el"
;;
;; Add the new system using strings:
;;   - `:domain' - "home", "work", other domain
;;   - `:date'   - year, or YYYY-MM-DD
;;   - `:type'   - "desk", "lap", "vm", "tablet", etc
;;
;; For example:
;;    (system:multiplexer:define :hash "012345-abcdef"
;;                               :domain "work"
;;                               :date "2025"
;;                               :type "vr"
;;                               :description "Linux VR Computer"
;;                               :path/secret/root (path:canonicalize user-emacs-directory ".." "secret"))
;;
;; Then add the system by creating another `system:multiplexer:define' call with the
;; string ID created.


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'init 'system)

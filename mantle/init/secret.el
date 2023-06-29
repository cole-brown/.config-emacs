;;; mantle/init/secret.el --- What secrets? -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <https://github.com/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; URL:        https://github.com/cole-brown/.config-emacs
;; Created:    2021-07-11
;; Timestamp:  2023-06-29
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;; ┌────────────────────────┐ Initialize My Secrets ┌────────────────────────┐
;; │               You're not allowed to know what they are.                 │
;; └─────────────────────────────┘ (shhh...) └───────────────────────────────┘
;;
;;; Code:


(imp:require :system 'secret)


;;--------------------------------------------------------------------------------
;; Secret Init
;;--------------------------------------------------------------------------------

(nub:debug
 :innit
 (imp:path:current:file/relative :root)
 '(:init :system :multiplexer :secret)
 "This system's hash is: %S (== %S)"
 (system:multiplexer:hash)
 (system:secret:hash))


;; Go get our secrets if we have the system set up for it.
;; Only do this if we have:
;;   - A hash & id for this computer.
;;   - A valid root init.el for secrets.
;; secrets/init.el will do the per-computer stuff.
(system:secret:init :secret 'init)


(nub:debug
 :innit
 (imp:path:current:file/relative :root)
 '(:init :system :multiplexer :secret)
 "System %S has secrets? %S"
 (system:secret:hash)
 (system:secret:has))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'init 'secret)

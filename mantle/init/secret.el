;;; init/secret.el -*- lexical-binding: t; -*-


(imp:require :system 'secret)


;;--------------------------Initialize Our Secrets.-----------------------------
;;--               You're not allowed to know what they are.                  --
;;---------------------------------(shhh...)------------------------------------


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
(imp:provide :mantle 'init 'user 'secret)

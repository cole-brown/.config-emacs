;;; mantle/config/spotify.el --- MUSIC! -*- lexical-binding: t; -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2023-04-28
;; Modified:   2023-04-28
;; URL:        https://github.com/cole-brown/.config-emacs
;;
;; These are not the GNU Emacs droids you're looking for.
;; We can go about our business.
;; Move along.
;;
;;; Commentary:
;;
;;  Configure Smudge, which is a Spotify controller.
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Unicode Media Control Symbols
;;------------------------------------------------------------------------------
;; - https://en.wikipedia.org/wiki/Media_control_symbols

;; NOTE: They're not a width that plays nice with monospaced fonts...
;; Hence the tabs for alignment...
;;
;;   U+23F5  ⏵ 	 U+25B6 ▶/▶ 		 Play
;;   U+23F8  ⏸/⏸ 	        		 Pause
;;   U+23EF  ⏯/⏯ 	        		 Play/Pause toggle
;;   U+23F4  ⏴ 	 U+25C0 ◀/◀ 		 Reverse
;;   U+23F9  ⏹ 	 U+25A0 ■ 		 Stop
;;   U+23EA  ⏪/⏪ 	        		 Back (Fast Backwards)
;;   U+23E9  ⏩/⏩ 	        		 Fast forward
;;   U+23EE  ⏮/⏮ 	        		 Skip to the start or previous file/track/chapter
;;   U+23ED  ⏭/⏭ 	        		 Skip to the end or next file/track/chapter
;;   U+23FA  ⏺/⏺ 	        		 Record
;;   U+23CF  ⏏/⏏ 	        		 Eject
;;   U+1F500 🔀/🔀 	        		 Shuffle
;;   U+1F501 🔁/🔁 	U+1F503 🔃/🔃 	 Repeat (indefinitely)
;;   U+1F502 🔂/🔂 	        		 Repeat once
;;   U+2139  ℹ/ℹ 	        		 Info
;;   U+1F504 🔄/🔄 	        		 Reload, Refresh
;; See also:
;;   http://xahlee.info/comp/unicode_computing_symbols.html


;;------------------------------------------------------------------------------
;; Smudge/Spotify Helpers
;;------------------------------------------------------------------------------
;; TODO:smudge: Make a "smudge-api.el" file and do a pull request?
;;   - Well, there already is a file named that in my smudge repo so... Put these there or some other file?

(defun int<smudge>:response (keyword msg &rest args)
  "Print MSG and ARGS via `message' for a response from a `smudge' async func.

Prefix messages with \"Spotify: \"

Return KEYWORD"
  (when msg
    (message "Spotify: %s"
             (if args
                 (apply #'format msg args)
               msg)))
  keyword)


(defun async<smudge>:device:select (&optional name)
  "Select device NAME (default `(system-name)') as the active Spotify device.

TODO:smudge: Better error handling from Smudge would be nice...
Prints a(n error) message if:
  - NAME is not a string.
  - Spotify user does not have a premium subscription.
  - NAME is not in Spotify's list of devices.
  - NAME not found on the list of Spotify devices.

TODO:smudge: Better error handling from Smudge would be nice...
             Am I allowed to raise error signals?

Returns `:error' or nil."
  (let ((desired:name (or name (system-name)))
        ;; Did we end up finding a device to try to select?
        (desired:active? nil))
    (if (not (stringp desired:name))
        (int<smudge>:response :error
                              "Desired device name must be a string, got: NAME: %S, DESIRED:NAME: %S"
                              name
                              desired:name)

      ;; Check the Spotify user has enough permissions.
      (smudge-api-current-user
       (lambda (user)
         (if (not (string= (gethash 'product user) "premium"))
             (int<smudge>:response :error
                                   "This feature requires a Spotify premium subscription.")

           ;; Get devices currently known to Spotify.
           (smudge-api-device-list
            (lambda (json)
              (if-let ((devices (gethash 'devices json)))
                  (dolist (device devices)
                    (when-let* ((device:name    (smudge-device-get-device-name device))
                                (device:id      (smudge-device-get-device-id device))
                                (device:select? (and (stringp device:name)
                                                     (string= device:name desired:name))))
                      ;; Note the state we found it in.
                      (setq desired:active? (smudge-device-get-device-is-active device))

                      ;;------------------------------
                      ;; Found a matching device; select it.
                      ;;------------------------------
                      (smudge-api-transfer-player
                       device:id
                       (lambda (_)
                         (setq smudge-selected-device-id device:id)
                         (if desired:active?
                             (int<smudge>:response nil
                                                   "Device '%s' already active. Reselected anyways."
                                                   device:name)
                           (int<smudge>:response nil
                                                 "Device '%s' selected."
                                                 device:name))))))

                (int<smudge>:response :error
                                      "No devices are available."))))
           nil))))))
;; (async<smudge>:device:select)


(defun async<smudge>:device:active? (name callback)
  "Call CALLBACK with nil/non-nil result of checking if device NAME is active.

CALLBACK should be a function with a signature like:
  - (defun CALLBACK (boolean) ...)
  - (lambda (boolean) ...)
That is, it receives a nil/non-nil result for whether the Spotify device NAME is
active.

The CALLBACK's nil/non-nil input is based on NAME, which should be nil,
`:any', or a string:
  - If NAME is a string, it is whether that exact Spotify device name is active.
  - If `:any', it is whether _any_ Spotify device is active.
  - If nil, it is whether \"this\" Spotify device is active.
    - Where \"this\" is what the string return of function `system-name' names.

This function always returns nil. Your CALLBACK gets the result."
  (let ((desired:name (or name
                          (system-name))))
    ;;------------------------------
    ;; Error Checks
    ;;------------------------------
    ;; Invalid CALLBACK?
    (cond ((not (functionp callback))
           (error "async<smudge>:device:active?: CALLBACK must be a function. CALLBACK: %S"
                  callback))

          ;; Invalid NAME input?
          ((not (or (eq desired:name :any)
                    (stringp desired:name)))
           (funcall callback
                    (int<smudge>:response nil
                                          "Desired device name must be %s, got: NAME: %S, DESIRED:NAME: %S"
                                          "a string or `:any'"
                                          name
                                          desired:name)))
          ;;------------------------------
          ;; Check if device is active.
          ;;------------------------------
          ;; Make the async Smudge calls.
          (t
           ;; Check the Spotify user has enough permissions.
           (smudge-api-current-user
            (lambda (user)
              (if (not (string= (gethash 'product user) "premium"))
                  (funcall callback
                           (int<smudge>:response nil
                                                 "This feature requires a Spotify premium subscription."))

                ;; Get devices currently known to Spotify.
                (smudge-api-device-list
                 (lambda (json)
                   (let ((devices (gethash 'devices json))
                         ;; "Don't care about the rest." flag
                         device:found
                         ;; CALLBACK's input.
                         devices:active)
                     (dolist (device devices)
                       ;; Only bother when we need to keep searching and we've got an active device to check.
                       (when-let ((keep-searching (not device:found))
                                  (device:name    (smudge-device-get-device-name device))
                                  (device:id      (smudge-device-get-device-id device))
                                  (device:active  (smudge-device-get-device-is-active device)))
                         ;; Any device? Push active device names to CALLBACK's input.
                         (cond ((eq desired:name :any)
                                (push device:name devices:active))

                               ;; Specific device check.
                               ((and (stringp device:name)
                                     (string= device:name desired:name))
                                ;; Mark that we don't care about the rest.
                                (setq device:found t)
                                (push device:name devices:active))

                               ;; Don't care about this device.
                               (t
                                nil))))

                     ;; Return result.
                     (if devices:active
                         (funcall callback devices:active)
                       (funcall callback
                                (int<smudge>:response nil
                                                      "No devices are active."))))))))))))

  ;; Always return nil so I don't confuse myself again with this async func.
  nil)
;; (async<smudge>:device:active? nil (lambda (active?) (message "active? %S" active?)))
;; (async<smudge>:device:active? :any (lambda (active?) (message "active? %S" active?)))


(defun async<smudge>:device:activate (name)
  "Tell Spotify to transfer to/activate device NAME.

Always returns nil."
  ;; TODO:smudge: input validity check
  (smudge-api-device-list
   (lambda (json)
     "Activate NAME device from list, if possible."
     (let (names/known)
       (if-let* ((devices     (gethash 'devices json))
                 (device      (pop devices))
                 (continue    t))
           ;;------------------------------
           ;; Got one or more devices; process them.
           ;;------------------------------
           (progn
             (while (and device continue)
               ;; Only bother if we have the vars and it's the device name we want.
               (when-let ((device/name   (smudge-device-get-device-name device))
                          (device/id     (smudge-device-get-device-id device))
                          (device/active (smudge-device-get-device-is-active device))
                          (activate?     (string= name device/name)))
                 ;; Add to list of known devices.
                 (if (eq names/known t)
                     (setq names/known (list device/name))
                   (push device/name names/known))

                 (if device/active
                     ;; Already active - just say that?
                     (progn
                       ;; NOTE: Could add a "force activate" param or just ignore
                       ;; `device/active' if there are bugs/issues that need worked
                       ;; around, but this seems smart right now?
                       (int<smudge>:response name
                                             "Device '%s' already active."
                                             name)
                       (setq continue nil))

                   ;; Activate (transfer to) this desired device.
                   (smudge-api-transfer-player
                    device/id
                    ;; This is what Smudge does in `smudge-device-select-active', so I think we do it here?
                    (lambda (json)
                      (setq smudge-selected-device-id device-id)
                      (int<smudge>:response name
                                            "Device '%s' selected" name)))
                   ;; We've found and activated the device; stop the loop.
                   (setq continue nil)))

               ;; Finish up by selecting next device to process.
               (setq device (pop devices)))

             ;;------------------------------
             ;; Feedback: Did we activate anything?
             ;;------------------------------
             (if continue
                 ;; Device requested wasn't in the list.
                 (int<smudge>:response nil
                                       "Desired device '%s' isn't available right now. Available devices: %s"
                                       name
                                       (or names/known "none"))
               (int<smudge>:response name
                                     "Active Device: '%s'."
                                     name)))

         ;;------------------------------
         ;; No devices from Spotify!
         ;;------------------------------
         (int<smudge>:response nil
                               "No devices.")))))

  ;; Async function - always return nil.
  nil)


(defun async<smudge>:init (&rest plist)
  "Initialize Smudge & Spotify, as necessary, using PLIST.

PLIST can have:
  - `:smudge-mode'
    - non-nil (default if key not supplied)
      - Enables `global-smudge-remote-mode' if needed.
    - nil
      - Don't bother `global-smudge-remote-mode'.

  - `:device'
    - nil (default if key not supplied)
      - Activate function `system-name' return value as the Spotify device.
    - `preserve'
      - If any device is active, leave it. Otherwise activate function
        `system-name'.
    - STRING
      - Activate STRING as the Spotify device."
  (let* ((func/name "async<smudge>:init")
         (enable/smudge-mode (if (plist-member plist :smudge-mode)
                                 (plist-get plist :smudge-mode)
                               ;; Default to enabling the mode if no key supplied.
                               :enable))
         (device/this        (system-name))
         (device/valid       '("DEVICE-NAME-STRING" device nil)) ;; Only for printed in error message.
         (enable/device      (if (plist-member plist :device)
                                 (plist-get plist :device)
                               ;; Default to activating this system if no key supplied.
                               device/this)))

    ;;------------------------------
    ;; Activate Device
    ;;------------------------------
    ;; If anything is active, ok. Otherwise activate this device.
    (cond ((eq enable/device 'preserve)
           (async<smudge>:device:active? :any
                                         (lambda (active)
                                           "Activate this device if any device is not ACTIVE."
                                           (unless active
                                             (async<smudge>:device:activate device/this)))))

          ;; Enable a specific device by name.
          ((stringp enable/device)
           ;; Activate if not active already.
           (async<smudge>:device:activate device/this))

          ;; Already converted `nil' into `system-name' string, so...
          ;; Don't know; error.
          (t
           (error "%s: Unknown `:device' option! Allowed: %S, Got: %S"
                  func/name device/valid enable/device)))

    ;;------------------------------
    ;; Global Smudge Remote Mode
    ;;------------------------------
    (when enable/smudge-mode
      (global-smudge-remote-mode 1))
    ;; Just ignore `enable/smudge-mode' == nil; don't disable the mode in an init func.
    ))


(defmacro async<smudge>:device:when-active (desired-name &rest body)
  "Evaluate BODY when DESIRED-NAME Spotify device is active.

DESIRED-NAME must be a string of the Spotify device's name.

Variables available during BODY:
  - `macro:devices'       - list of all devices
  - `macro:device'        - desired device
  - `macro:device/name'   - DESIRED-NAME, but from Spotify
  - `macro:device/id'     - device's ID
  - `macro:device/active' - boolean

Always returns nil."
  (declare (indent 1))
  `(progn
     (smudge-api-device-list
      (lambda (macro:json)
        "Run the BODY if DESIRED-NAME is active."
        (when-let* ((macro:desired-name ,desired-name)
                    (macro:devices      (gethash 'devices macro:json))
                    (macro:device       (pop macro:devices))
                    (macro:continue     t))
          ;;------------------------------
          ;; Got one or more devices; process them.
          ;;------------------------------
          (while (and macro:device macro:continue)
            ;; Only bother if we have the vars and it's the device name we want.
            (when-let ((macro:device/name   (smudge-device-get-device-name macro:device))
                       (macro:device/id     (smudge-device-get-device-id macro:device))
                       (macro:device/active (smudge-device-get-device-is-active macro:device))
                       (macro:run-body?     (string= macro:desired-name macro:device/name)))
              ;; Run it.
              (setq macro:continue nil)
              ,@body)

            ;; Selecting next device to process in loop.
            (setq macro:device (pop macro:devices))))))
     nil))
;; (async<smudge>:device:when-active (system-name) (message "hello there"))


;;------------------------------------------------------------------------------
;; Smudge (Spotify Controller)
;;------------------------------------------------------------------------------

  (defvar innit:path:package:smudge (path:join innit:path:packages:user "smudge")
    "`use-package' doesn't like having a function call for `:load-path', thus this.")


;; https://github.com/danielfm/smudge/
(imp:use-package smudge
  ;;---
  ;; Git Repo, Local & Unmanaged
  ;;---
  ;; This is my own fork of the repo for this package, so...
  ;;   1) Don't try to install.
  :ensure nil
  ;;   2) Here's where it is; add this dir to the `load-path'.
  :load-path innit:path:package:smudge

  ;;---
  ;; Git Repo, Managed by Straight
  ;;---
  ;; Or we could use straight if we don't care about being in the middle of
  ;; whatever branch we happen to be working on:
  ;; :straight (:type git
  ;;            :host github
  ;;            :repo "cole-brown/smudge")
  ;;            ;; :repo "danielfm/smudge")

  ;;---
  ;; Normal Package Management
  ;;---
  ;; Or we could get it from MELPA by just not saying anything.

  ;;------------------------------
  :custom
  ;;------------------------------

  ;;---
  ;; Connection Settings
  ;;---
  (smudge-transport 'connect)
  (smudge-oauth2-callback-port "14576")
  (smudge-oauth2-client-id     (plist-get secret:key:spotify :id))
  (smudge-oauth2-client-secret (plist-get secret:key:spotify :secret))

  ;; This has to make an API call every time, so... don't overdo it and throttle yourself.
  (smudge-player-status-refresh-interval 10) ; Refresh rate in seconds.

  ;;---
  ;; Display
  ;;---

  ;; Put status in title-bar instead of modeline
  (smudge-status-location 'title-bar)

  ;; smudge-player-status-format: default is "[%p: %a - %t ◷ %l %r%s]"
  ;;   - https://github.com/danielfm/smudge#customizing-the-player-status
  (smudge-player-status-format
   ;; TODO:smudge: Elapsed/current time?
   "%r%s%p ╠═ %a ═╬═ %t ═╣ ◷%l")
  (smudge-title-bar-separator "  ┅┅  ")
  ;; frame-title-format

  ;; TODO:smudge: Is this long enough?
  ;; I got a lot more room in the titlebar than the modeline...
  (smudge-player-status-truncate-length 30) ;; default: 15

  ;;---
  ;; Status Text
  ;;---
  ;; Defaults are a grab-bag of words ("Playing"/"Stopped") and letters ("R"/"-").

  ;; Do I have unicode media icons?
  ;;   Play:         ▶ / ▶️
  ;;   Pause:        ⏸ / ⏸️
  ;;   Play/Pause:   ⏯ / ⏯️
  ;;   Stop:         ⏹ / ⏹️
  ;;   Skip Back:    ⏮ / ⏮️
  ;;   Skip Forward: ⏭ / ⏭️
  ;;   Shuffle:      🔀 / 🔀️
  ;;   Repeat:       🔁 / 🔁️
  ;;   'Blank/Off':  ◻

  ;; Unicode Icons:
  (smudge-player-status-playing-text       "▶️")
  (smudge-player-status-paused-text        "⏸")
  (smudge-player-status-stopped-text       "⏹")
  (smudge-player-status-repeating-text     "🔁")
  (smudge-player-status-not-repeating-text "")   ;; "◻️")
  (smudge-player-status-shuffling-text     "🔀")
  (smudge-player-status-not-shuffling-text "")   ;; "◻")

  ;; ASCII:
  ;; (smudge-player-status-playing-text       "p")
  ;; (smudge-player-status-paused-text        "-")
  ;; (smudge-player-status-stopped-text       "x")
  ;; (smudge-player-status-repeating-text     "R")
  ;; (smudge-player-status-not-repeating-text "-")
  ;; (smudge-player-status-shuffling-text     "S")
  ;; (smudge-player-status-not-shuffling-text "-")


  ;;------------------------------
  :config
  ;;------------------------------

  (defun mantle:workday:end/spotify ()
    "Stop Smudge & do any needed clean up so Emacs doesn't freeze overnight.

Pause Spotify and close Smudge.

Smudge used to end up with zombie connections when left open overnight... Don't
know if it still does, but this was the solution:
  - Stop/pause Spotify
  - Turn off global Smudge minor mode"
    (interactive)

    (let ((progress-reporter (make-progress-reporter (format "[%s] Spotify clean-up..."
                                                             (datetime:string/get 'rfc-3339 'datetime))
                                                     0
                                                     100)))

      (async<smudge>:device:when-active
          (system-name)
        ;;---
        ;; [2022-02-18] Cleaner Version?
        ;;---
        (smudge-connect-player-pause)
        (progress-reporter-update progress-reporter 50)

        ;; TODO:smudge: Do I need to clean up any buffers?
        ;; (buffer:kill:matching ...)?

        ;; ;;---
        ;; ;; [2019-10-22] Ye Olde Version
        ;; ;;   Do I need the condition case still?
        ;; ;;---
        ;; (condition-case-unless-debug err
        ;;   (progn
        ;;     (smudge-connect-player-pause)
        ;;     (global-smudge-remote-mode -1)
        ;;     ;; TODO:smudge: Do I need to clean up any buffers?
        ;;     ;; (buffer:kill:matching ...)?
        ;;     )
        ;; ;; Catch signaled error 'error': downgrade to just message.
        ;; ;; [2019-10-22]: This is just theory as spotify can get cranky if
        ;; ;; connected but device was left paused...
        ;; (error
        ;;  ;; Downgrade.
        ;;  (message "[ERROR]: mantle:workday:end/spotify: Received error signal:" err)))
        )

      ;; Always disable Smudge mode?
      (global-smudge-remote-mode -1)
      (progress-reporter-done progress-reporter)))
  ;; (mantle:workday:end/spotify)


  ;; TODO:smudge: Is this hack still required?
  ;; HACK: This is a defconst so it doesn't update when you set a different port number.
  ;; So... Just force it to update.
  (setq smudge-api-oauth2-callback (concat "http://localhost:" smudge-oauth2-callback-port smudge-oauth2-callback-endpoint))

  ;;---
  ;; Keybinds
  ;;---
  ;; Keybind: See config/keybinds/spotify.el
  ;; (define-key smudge-mode-map (kbd "C-c .") 'smudge-command-map)
  )


;;--------------------------------------------------------------------------------
;; Keybinds : Meow
;;--------------------------------------------------------------------------------

(imp:use-package smudge
  ;;---
  ;; Package Source
  ;;---
  ;; NOTE: Use same package source as above?
  ;; This is my own fork of the repo for this package, so...
  ;;   1. Don't try to install.
  :ensure nil
  ;;   2. Here's where it is; add this dir to the `load-path'.
  :load-path innit:path:package:smudge

  ;;---
  ;; Load Conditions/Ordering
  ;;---
  :when  (imp:flag? :keybinds +meow)
  :after (:and meow pretty-hydra)


  ;;------------------------------
  :config
  ;;------------------------------

  (defvar int<smudge>:hydra/title
    ;; TODO: Need any height/v-adjust?
    (mantle:user:icon/font-awesome "spotify" ;; Font Awesome icon name
                                   "Spotify" ;; Text after icon
                                   ;; Icon Settings
                                   :color:icon "limegreen"
                                   ;; :height 1
                                   ;; :v-adjust -0.05
                                   )
    "Propertized string with icon & name for Spotify pretty-hydra.")
  ;; int<smudge>:hydra/title
  ;; (insert int<smudge>:hydra/title)


  ;; Could have plist args if I need to customize?
  (defun int<smudge>:title ()
    "Get title string for `pretty-hydra'."
    ;; Icon & "Spotify"
    (concat int<smudge>:hydra/title
            "\n"
            ;; Player Status formatted string.
            smudge-controller-player-status))


  ;; TODO: pretty-hydra needs to know about unicode character widths in order to make nice tables? :sad:
  ;;   - In the mean time, move the icon stuff to last in the columns.
  ;;
  ;; TODO: pretty-hydra: Would be nice if there was an `:enabled' that could grey things out & ignore keybind if not true.
  ;;
  ;; TODO: Update the 'now playing' status line more often?
  ;;   - Currently it only updates when a key is pressed, then the hydra is close/reopened.
  ;;   - Use `:formatter' arg/function?
  ;;     - https://github.com/jerrypnz/major-mode-hydra.el#pretty-hydra-define
  (pretty-hydra-define int<smudge>:hydra
    (:quit-key "g"
     :color blue ;; Default to `:exit t' for all heads.
     :title (int<smudge>:title))

    ;;------------------------------
    ;; Search: Playlists, Artist, etc.
    ;;------------------------------
    ("Search"
     (("h"
       smudge-my-playlists
       "My Playlists")

      ("t"
       smudge-featured-playlists
       "Featured Playlists")

      ("n"
       smudge-user-playlists
       "User Playlists")

      ("s"
       smudge-track-search
       "Search for Track"))

     ;;------------------------------
     ;; Manage: Devices
     ;;------------------------------
     "Manage"
     (("d" smudge-select-device
       "Select Device")

      ("i" (async<smudge>:init)
       ;; Can't have non-monospaced characters in this column... throws off the later columns.
       ;; "🚀 Initialize"
       "Initialize"
       :exit nil)

      ("x" (mantle:workday:end/spotify)
       "Pause & Quit")

      ;; TODO: A 'refresh' which makes a call to get a new status string, then redraws the hydra when the result comes back?
      ;; "🔄 Refresh Status"
      ;;   - TODO: Would putting a zero-width space in there make pretty-hydra align things correctly due to the double-wide glyph?
      )

     ;;------------------------------
     ;; Control: Play, Volume, etc.
     ;;------------------------------
     "Track"
     ;; TODO: Toggle/choose from play or pause: "▶" "⏸"
     (("p"
       smudge-controller-toggle-play
       "⏯"
       :exit nil)
      ;; Could do?:
      ;;   :toggle #'func-that-gets-current-state
      ;;   :exit nil

      ("u" ;; character forward
       smudge-controller-next-track
       "⏭"
       :exit nil)

      ("o" ;; character backwards
       smudge-controller-previous-track
       "⏮"
       :exit nil)

      (";"
       smudge-controller-toggle-shuffle
       "🔀"
       ;; TODO: :toggle #'func-that-gets-current-state
       )

      ("q"
       smudge-controller-toggle-repeat
       "🔁"
       ;; TODO: :toggle #'func-that-gets-current-state
       ))

     "Volume"
     (("." ;; line previous
       smudge-controller-volume-up
       "🔊"
       :exit nil)

      ("e" ;; line next
       smudge-controller-volume-down
       "🔉"
       :exit nil)

      ;; TODO: choose from mute or unmute: "🔇" "🔈"
      ("x"
       smudge-controller-volume-mute-unmute
       (if (smudge-cache-api-is-muted :name (system-name))
           "🔈 Unmute"
         "🔇 Mute")
       :toggle (smudge-cache-api-is-muted :name (system-name))
       :exit nil))))
  ;; (int<smudge>:hydra/body)


  (defun int<smudge>:hydra-maybe-init ()
    "Initialize to this device if nothing is active, then open the hydra."
    (interactive)
    (unless smudge-selected-device-id
      (async<smudge>:init :device 'preserve))
    (int<smudge>:hydra/body))


  ;;------------------------------
  :general
  ;;------------------------------

  (keybind:leader/global:def
    :infix (keybind:infix "u") ; ...."apps & stuff"
    "u" (list #'int<smudge>:hydra-maybe-init :which-key (concat int<smudge>:hydra/title " Remote"))))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :mantle 'config 'spotify)

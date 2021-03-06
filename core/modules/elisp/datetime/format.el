;;; elisp/datetime/format.el -*- lexical-binding: t; -*-


;;------------------------------Formatting Time---------------------------------
;;--                 Dates, Times, Datetimes, Timedates...                    --
;;------------(Format, not reformat... Don't, like, delete time.)---------------

(imp:require :elisp 'utils)
(imp:require :jerky)


;;------------------------------------------------------------------------------
;; Getters, Setters.
;;------------------------------------------------------------------------------

;;--------------------
;; General
;;--------------------

(defun datetime:string/get (&rest args)
  "Return a formatted datetime string based on ARGS.

Use `datetime:format/get' to get a datetime format and then calls
`format-time-string' with format and options to get a time string.

Splits ARGS out into a 'name' and a 'keyword-args' plist.
The 'name' is everything that comes before any of our keywords.

_Optional_ keywords are:
  :time - A time (compatible with `format-time-string') to use instead of now.
  :zone - A timezone to use instead of the local one.

Return string from `format-time-string'."
  (let* ((name-and-kwargs (elisp:parse:args+kwargs args :time :zone))
         (name   (car name-and-kwargs))
         (kwargs (cdr name-and-kwargs))
         (time   (plist-get kwargs :time))
         (zone   (plist-get kwargs :zone)))
    (format-time-string (apply #'datetime:format/get name) time zone)))
;; (datetime:string/get 'iso-8601 'long)


(defun datetime:format/get (&rest name)
  "Return a datetime format string by NAME.

NAME can be strings, symbols, or list(s) of such.

Prepends '(datetime format) to the NAME before asking `jerky' so that all values
are stored under that \"namespace\" or tree branch."
  (apply #'jerky:get 'datetime 'format name))


(defun datetime:format/set (&rest args)
  "Set a datetime format string.

Splits ARGS out into a 'name' and 'keyword-args'.

The 'name' is everything that comes before any of our keywords. Prepends
'(datetime format) to the 'name' before asking `jerky' so that all values are
stored under that \"namespace\" or tree branch.

'keywords-args' are:
  `:value'  - datetime format string
  `:docstr' - documentation string"
  (apply #'jerky:set 'datetime 'format args))


;;------------------------------------------------------------------------------
;; Some useful formats.
;;------------------------------------------------------------------------------

(defun datetime:init ()
  "Set the predefined datetime format strings into jerky."
  ;;--------------------
  ;; ISO-8601 / RFC-3339 Formats
  ;;--------------------

  ;;---
  ;; Date Only
  ;;---
  ;; ISO-8601: short (date only; no timestamp)
  (datetime:format/set 'iso-8601 'date ;; was: 'iso-8601 'short
                       :value  "%Y-%m-%d"
                       :docstr "ISO-8601/RFC-3339; date-only.")
  ;; aka: 'rfc-3339 'date
  (datetime:format/set 'rfc-3339 'date
                       :value  "%Y-%m-%d"
                       :docstr "ISO-8601/RFC-3339; date-only.")

  ;; ISO-8601: But meh. No separators makes it much less useful to the brain.
  (datetime:format/set 'yyyymmdd
                       :value  "%Y%m%d"
                       :docstr "Why would you use this?! Give my eyes a break.")

  ;;---
  ;; Date & Time
  ;;---
  ;; RFC-3339: full (space separator)
  (datetime:format/set 'rfc-3339 'datetime
                       :value  "%Y-%m-%d %H:%M:%S"
                       :docstr "RFC-3339; datetime with space separator for in text.")

  ;; ISO-8601: full ('T' separator; for logs, etc)
  (datetime:format/set 'iso-8601 'datetime
                       :value  "%Y-%m-%dT%T%z"
                       :docstr "ISO-8601; full datetime with 'T' separator for in logs, etc.")

  ;;--------------------
  ;; Org-Mode Formats
  ;;--------------------

  ;; org-mode inactive: useful even when not in org-mode.
  (datetime:format/set 'org 'inactive 'date ;; was: 'org-inactive
                       :value  "[%Y-%m-%d]"
                       :docstr "org-time-stamp-inactive sans day name e.g.: [2022-06-01]")

  ;; org-mode inactive: useful even when not in org-mode.
  (datetime:format/set 'org 'inactive 'date-day
                       :value  "[%Y-%m-%d %a]"
                       :docstr "org-time-stamp-inactive e.g.: [2022-06-01 Wed]")

  ;; org-mode inactive: useful even when not in org-mode.
  (datetime:format/set 'org 'inactive 'rfc-3339
                       :value  "[%Y-%m-%d %H:%M:%S]"
                       :docstr "org-time-stamp-inactive but RFC-3339 e.g.: [2022-06-01 22:05:11]")

  ;; org-mode inactive: useful even when not in org-mode.
  (datetime:format/set 'org 'inactive 'full
                       :value  "[%Y-%m-%d %a %H:%M:%S]"
                       :docstr "org-time-stamp-inactive with time e.g.: [2022-06-01 Wed 22:05:11]")

  ;; Could do the same for active, but I use them less outside org files...

  ;;--------------------
  ;; Misc Formats
  ;;--------------------

  ;; Filename: full ('-T-' date/time separator & '-' time field separator; for filenames)
  (datetime:format/set 'file 'datetime
                       :value  "%Y-%m-%d-T-%H-%M-%S"
                       :docstr (concat "ISO-8601/RFC-3339...ish format, but works on Windows. "
                                       "Can strip hypens to get to ISO-8601 basic "
                                       "format (YYYYMMDDThhmmss)."))

  ;; USA Date: Pretty much the worst format ever invented.
  (datetime:format/set 'usa 'date
                       :value  "%m-%d-%Y"
                       :docstr (concat "Bad, US American format. "
                                       "Just terrible - why do we use this? "
                                       "I am ashamed."))

  ;; USA Date w/ 24 hr time: Pretty much the worst format ever invented, plus
  ;; extra confusion because no one understands 24 hour time over here...
  (datetime:format/set 'usa 'datetime
                       :value  "%m-%d-%Y %H:%M:%S"
                       :docstr (concat "Bad, US American format. "
                                       "Just terrible - why do we use this? "
                                       "I am ashamed.")))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :datetime 'format)

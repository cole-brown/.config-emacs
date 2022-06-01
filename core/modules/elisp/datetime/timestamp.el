;;; elisp/datetime/timestamp.el -*- lexical-binding: t; -*-

(imp:require :datetime 'format)


;;-------------------------------Stamping Time----------------------------------
;;--                 Dates, Times, Datetimes, Timedates...                    --
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; Timestamp Functions (Also Datestamp)
;;------------------------------------------------------------------------------

;;--------------------
;; Interactive: Insert Timestamp
;;--------------------


(defun datetime:timestamp:insert (&rest name)
  "Insert a timestamp NAME into current buffer at point.

Timestamp is of current time and is inserted into current buffer at point."
  (interactive)
  (insert (apply #'datetime:string/get name)))


(defun datetime:cmd:timestamp:insert/rfc-3339 ()
  "Insert a full (date & time) rfc-3339 formatted timestamp.

Timestamp is of current time and is inserted into current buffer at point."
  (interactive)
  (datetime:timestamp:insert 'rfc-3339 'datetime))


(defun datetime:cmd:timestamp:insert/iso-8601 ()
  "Insert a full (date & time) ISO-8601 formatted timestamp.

Timestamp is of current time and is inserted into current buffer at point."
  (interactive)
  (datetime:timestamp:insert 'iso-8601 'datetime))


(defun datetime:cmd:timestamp:insert/org ()
  "Insert a timestamp formatted \"[yyyy-mm-dd]\".

Timestamp is of current time and is inserted into current buffer at point."
  (interactive)
  (datetime:timestamp:insert 'org 'inactive 'date))



;;--------------------
;; Misc
;;--------------------

;; Was used in an old weekly-status template; may be useful again some day?
(defun datetime:timestamp:next-friday (format)
  "Return next Friday's date as FORMAT `format-time-string' string."
  (let ((today (nth 6 (decode-time (current-time)))))
    (format-time-string
     format
     (time-add
      (current-time)
      (days-to-time
       (if (eq 5 today) ; saturday is only day bigger than friday
           6
         (- 5 today)))))))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(imp:provide :datetime 'timestamp)

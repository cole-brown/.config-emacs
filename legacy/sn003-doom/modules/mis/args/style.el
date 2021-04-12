;;; mis/args/style.el -*- lexical-binding: t; -*-


(-m//require 'internal 'mlist)


;;------------------------------------------------------------------------------
;; Constants
;;------------------------------------------------------------------------------

(defconst -m//styles
  '(:width
    :margin
    :border
    :padding
    :align
    :boxed)
  "Valid mis :style section keywords.")


(defconst -m//style/alignments
  '(:left
    :center
    :right)
  "Valid mis :align keywords.")


;;------------------------------------------------------------------------------
;; General Getters/Setters
;;------------------------------------------------------------------------------

(defun -m//style/get (key mlist &optional default)
  "Get a style value from this mlist.
"
  (-m//section/get key :style mlist -m//styles default))


(defun -m//style/set (key value mlist)
  "Set a style value in this mlist.
"
   (-m//section/set key value :style mlist -m//styles))


(defun -m//style/first (key mlists &optional default)
  "Get first style match for KEY in MLISTS. Returns DEFAULT if no matches.

Use `:mis/nil', `:mis/error', etc for DEFAULT if you have a \"nil is valid\"
situation in the calling code.
"
  (-m//mlists/get.first key :style mlists -m//styles default))
;; (-m//style/first :border '((:mis t :string (:mis :string :trim t))) :mis/nil)
;; (-m//style/first :border '((:mis t :string (:mis :string :trim t))) " ")
;; (-m//style/first :border '((:mis t :style (:mis :style :border "x"))) "error dude")

;;------------------------------------------------------------------------------
;; Field Setters
;;------------------------------------------------------------------------------

(defun mis/style/width (width &optional mlist)
  "Sets a style width. Returns an mlist.
"
  (-m//style/set :width width mlist))


(defun mis/style/margin (margin &optional mlist)
  "Sets a style margin. Returns an mlist.
"
  (-m//style/set :margin margin mlist))


(defun mis/style/border (border &optional mlist)
  "Sets a style border. Returns an mlist.
"
  (-m//style/set :border border mlist))
;; (mis/style/border "x")
;;   -> (:mis t :style (:mis :style :border "x"))


(defun mis/style/padding (padding &optional mlist)
  "Sets a style padding. Returns an mlist.
"
  (-m//style/set :padding padding mlist))


(defun mis/style/align (alignment &optional mlist)
  "Sets an alignment. Returns an mlist.
"
  (unless (memq alignment -m//style/alignments)
    (error "mis/style/align: '%S' is not a valid alignment. Choices are: %S"
           alignment
           -m//style/alignments))
  (-m//style/set :align alignment mlist))
;; (mis/style/align :center)


(defun mis/style/boxed (boxed &optional mlist)
  "Sets 'boxed' style flag based on BOXED.

Will be set to t if BOXED is non-nil, else set to nil.

Returns an mlist.
"
  (-m//style/set :boxed (not (null boxed)) mlist))


;;------------------------------------------------------------------------------
;; Alignment
;;------------------------------------------------------------------------------

(defun -m//style/align (string mlists)
  "Align STRING based on alignment, padding settings in MLISTS.

Returns a string of length WIDTH, padding with spaces (default) or characters
from :string/padding styles in MLISTS."
  (declare (pure t) (side-effect-free t))

  (message "align: %S, string: '%S', width: %S, padding: %S"
           (-m//style/first :align mlists :mis/nil)
           string
           (-m//line/width mlists)
           (-m//style/first :padding mlists " "))
  (let ((aligned (-m//style/align/to (-m//style/first :align mlists :mis/nil)
                                     string
                                     (-m//line/width mlists)
                                     (-m//style/first :padding mlists " "))))
    (if (-m//return/invalid? aligned '(:mis/nil nil))
        ;; No alignment supplied or no string or something...
        ;; Return empty string.
        ""
      aligned)))
;; (-m//style/align "foo" (list (mis/style/align :center) (mis/style/padding "x")))
;; (-m//style/align " foo " (list (mis/style/align :center)))
;; (-m//style/align " foo " (list (mis/style/align :center) (mis/style/padding "-")))


(defun -m//style/align/to (align string width padding)
  "Choose the proper alignment function for the ALIGN keyword.

Calls that function with the rest of the params and returns its value."
  (declare (pure t) (side-effect-free t))
  (cond ((eq align :center)
         (-m//style/align/to.center string width padding))
        ((eq align :left)
         (-m//style/align/to.left string width padding))
        ((eq align :right)
         (-m//style/align/to.right string width padding))
        (t
         :mis/error)))
;; (-m//style/align/to :center "foo" fill-column "-")
;; (-m//style/align/to :center " foo " fill-column "-")
;; (-m//style/align/to :left "foo" fill-column "-")
;; (-m//style/align/to :right "foo" fill-column "-")


(defun -m//style/align/to.center (string width padding)
  "Pad string to WIDTH with PADDING characters so that it is centered.

If STRING is too long, returns it (as-is/un-truncated)."
  (declare (pure t) (side-effect-free t))
  (let ((pad-amt (max 0 (- width (length string)))))
    (concat
     (make-string (ceiling pad-amt 2) (string-to-char padding))
     string
     (make-string (floor pad-amt 2) (string-to-char padding)))))


(defun -m//style/align/to.left (string width padding)
  "Pads STRING with PADDING on the left up to WIDTH.

If STRING is too long, returns it (as-is/un-truncated)."
  (declare (pure t) (side-effect-free t))
  (let ((pad-amt (max 0 (- width (length string)))))
    (concat (make-string pad-amt (string-to-char padding))
            string)))


(defun -m//style/align/to.right (string width padding)
  "Pads STRING with PADDING on the right up to WIDTH.

If STRING is too long, returns it (as-is/un-truncated)."
  (declare (pure t) (side-effect-free t))
  (let ((pad-amt (max 0 (- width (length string)))))
    (concat string
            (make-string pad-amt (string-to-char padding)))))


;;------------------------------------------------------------------------------
;; Style Output
;;------------------------------------------------------------------------------

(defun mis//out.style/width.get (mout)
  "Get :style/width from MOUT list.

Returns :mis/nil if none."
  (-m//out/entry.get :width mout))


(defun mis//out.style/width.set (value mout)
  "Set :style/width to VALUE in MOUT list.

Returns updated MOUT list."
  (-m//out/entry.set :width value mout))


(defun mis//out.style/margin.get (mout)
  "Get :style/margin from MOUT list.

Returns :mis/nil if none."
  (-m//out/entry.get :margin mout))


(defun mis//out.style/margin.set (value mout)
  "Set :style/margin to VALUE in MOUT list.

Returns updated MOUT list."
  (-m//out/entry.set :margin value mout))


(defun mis//out.style/border.get (mout)
  "Get :style/border from MOUT list.

Returns :mis/nil if none."
  (-m//out/entry.get :border mout))


(defun mis//out.style/border.set (value mout)
  "Set :style/border to VALUE in MOUT list.

Returns updated MOUT list."
  (-m//out/entry.set :border value mout))


(defun mis//out.style/padding.get (mout)
  "Get :style/padding from MOUT list.

Returns :mis/nil if none."
  (-m//out/entry.get :padding mout))


(defun mis//out.style/padding.set (value mout)
  "Set :style/padding to VALUE in MOUT list.

Returns updated MOUT list."
  (-m//out/entry.set :padding value mout))


(defun mis//out.style/align.get (mout)
  "Get :style/align from MOUT list.

Returns :mis/nil if none."
  (-m//out/entry.get :align mout))


(defun mis//out.style/align.set (value mout)
  "Set :style/align to VALUE in MOUT list.

Returns updated MOUT list."
  (-m//out/entry.set :align value mout))


(defun mis//out.style/boxed.get (mout)
  "Get :style/boxed from MOUT list.

Returns :mis/nil if none."
  (-m//out/entry.get :boxed mout))


(defun mis//out.style/boxed.set (value mout)
  "Set :style/boxed to VALUE in MOUT list.

Returns updated MOUT list."
  (-m//out/entry.set :boxed value mout))


;;------------------------------------------------------------------------------
;; The End.
;;------------------------------------------------------------------------------
(-m//provide 'args 'style)
(provide 'mis/args/style)

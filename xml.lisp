(in-package :webparser)

(defun escape-for-xml (out-stream str)
  "Write str to out-stream with < \" > & replaced with &lt; &quot; &gt; &amp;.
   If out-stream is nil, return the output as a string instead."
  (unless out-stream
    (return-from escape-for-xml
      (with-output-to-string (s) (escape-for-xml s str))))
  (unless (stringp str)
    (setf str (format nil "~s" str)))
  (let ((esc-pos (position-if (lambda (c) (member c '(#\< #\" #\> #\&))) str)))
    (cond
      (esc-pos
        (let* ((esc-char (elt str esc-pos))
	       (before (subseq str 0 esc-pos))
	       (after (subseq str (1+ esc-pos))))
	  (format out-stream "~a~a"
	      before
	      (ecase esc-char
		(#\< "&lt;")
		(#\" "&quot;")
		(#\> "&gt;")
		(#\& "&amp;")
		))
	  (escape-for-xml out-stream after)
	  ))
      (t
        (format out-stream "~a" str))
      )))

(defun format-xml-start (out-stream sexp)
  "Like format-xml, but only take one sexp, and only output the start tag.
   Return two values: the list of children from sexp, and the tag name string."
  (let ((tag-name (car sexp)))
    (typecase tag-name
      (symbol
	(setf tag-name (string-downcase (symbol-name tag-name))))
      (string
	nil)
      (otherwise
	(error "Expected symbol or string for tag name, but got ~s"
	       tag-name))
      )
    (format out-stream "<~a" tag-name)
    ;; attributes
    (loop for remaining = (cdr sexp) then (cddr remaining)
	  while (and remaining (car remaining) (cdr remaining)
		     (keywordp (car remaining)))
	  when (cadr remaining)
	  do
	    (format out-stream " ~(~a~)=\"" (symbol-name (car remaining)))
	    (escape-for-xml out-stream (format nil "~a" (cadr remaining)))
	    (format out-stream "\"")
	  finally
	    (unless remaining
	      (format out-stream " /"))
	    (format out-stream ">")
	    (return (values remaining tag-name))
	  )))

(defun format-xml-end (out-stream tag-name)
  "Write an XML end tag."
  (format out-stream "</~a>" tag-name))

(defun format-xml (out-stream &rest xml-sexps)
  "Format S-expressions as XML. The S-expressions should be of the form:
     (tag-name :attribute \"value\" ...
       \"child text\" (child-tag ...) ...)
   Which turns into:
     <tag-name attribute=\"value\">child text<child-tag ... /> ...</tag-name>
   Tag and attribute name symbols are downcased; strings are not. If you need
   an upper case tag name, use a string instead of a symbol. Attribute names
   must be keywords and are always downcased.
   Note that this doesn't allow you to format arbitrary S-expressions as XML,
   nor does it allow you to output arbitrary XML via S-expressions. It's only
   meant for use with the kind of XML that WebParser outputs.
   "
  (dolist (sexp xml-sexps)
    (etypecase sexp
      (string ; text
        (escape-for-xml out-stream sexp))
      (list ; element
        (multiple-value-bind (remaining tag-name)
	    (format-xml-start out-stream sexp)
	  (when remaining
	    (apply #'format-xml out-stream remaining)
	    (format-xml-end out-stream tag-name)
	    )))
      )))


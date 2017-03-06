;;;;
;;;; web-parser.lisp
;;;;
;;;; George Ferguson, ferguson@cs.rochester.edu,  6 Jul 2006
;;;; Time-stamp: <Thu Jul  6 16:52:20 EDT 2006 ferguson>
;;;;
;;;; Toplevel load file for the web-ready version of the parser.
;;;; Includes toplevel function run by dump (see Makefile)
;;;;

(unless (find-package :trips)
  (load (make-pathname :directory '(:relative :up "config" "lisp")
		       :name "trips")))

;(load #!TRIPS"src;Parser;defsys")
;(dfc:load-component :parser)
;; not actually used?
;;(load #!TRIPS"src;IM;defsys")
;;(dfc:load-component :im)
;
;(load #!TRIPS"src;WebParser;lf-to-rdf")
;(load #!TRIPS"src;WebParser;tree-to-xml")
;(load #!TRIPS"src;WebParser;parse-to-xml")

(load #!TRIPS"src;WebParser;defsys")
(dfc:load-component :parser)
(dfc:load-component :webparser)

(in-package :webparser)

(defun run-cgi ()
  (let* ((*package* (find-package :webparser))
	 (params (get-cgi-params))
	 (input (cdr (assoc "input" params :test #'string=)))
	 (treecontents (or (cdr (assoc "treecontents" params :test #'string=))
	                   "phrase"))
	 (treeformat (or (cdr (assoc "treeformat" params :test #'string=))
	 		 "LinGO"))
	 (lfformat (or (cdr (assoc "lfformat" params :test #'string=))
	               "svg"))
	 (debug (cdr (assoc "debug" params :test #'string=)))
	 )
    ;; MIME header
    (format t "Content-Type: text/xml~%~%")
    (parse-and-output-xml input treecontents treeformat lfformat debug t)
    (format t "~%") ; make sure we print the last line
    ;; May need to quit explicitly (can't hurt)
    (trips:exit 0)))

(defun get-cgi-params ()
  (let ((method (trips:get-env "REQUEST_METHOD")))
    (cond
     ((string= method "GET")
      (get-get-params))
     ((string= method "POST")
      (get-post-params))
     (t
      (error "unknown REQUEST_METHOD: ~A" method)))))
      
(defun get-get-params ()
  "Return the parameters of a CGI GET request. This is done by decoding
the value of the QUERY_STRING environment variable."
  (let* ((query-string (trips:get-env "QUERY_STRING"))
	 (stream (make-string-input-stream (or query-string ""))))
    (decode-urlencoded-content stream (length query-string))))

(defun get-post-params ()
  "Return the parameters of a CGI POST request. This is done by decoding
CONTENT_LENGTH characters read from standard input.
NOTE: This does not yet handle multipart/form-data forms."
  (let* ((content-length (parse-int (trips:get-env "CONTENT_LENGTH")))
	 (content-type (trips:get-env "CONTENT_TYPE")))
    (cond
     ((string= content-type "application/x-www-form-urlencoded")
      (decode-urlencoded-content *standard-input* content-length))
     (t
      (error "unknown CONTENT_TYPE: ~A" content-type)))))

(defun test-params (s)
  (decode-urlencoded-content (make-string-input-stream s) (length s)))

(defun decode-urlencoded-content (stream content-length)
  "Reads CONTENT-LENGTH characters from the given STREAM and parses them as
urlencoded parameters."
  (labels ((read-a-char ()
	     "Read and return the next char from STREAM, decrementing CONTENT-LENGTH."
	     (when (> content-length 0)
	       (decf content-length)
	       (read-char stream)))
	   (decode-token (stop-char)
	     "Read, decode, and return the next token up to STOP-CHAR."
	     (let ((token (make-array '(0) :element-type 'character :adjustable t :fill-pointer 0)))
	       (loop
		with c
		while (and (setq c (read-a-char)) (not (eql c stop-char)))
		do (case c
		     (#\+
		      ;; + => space
		      (vector-push-extend #\Space token))
		     (#\%
		      ;; %XX => hex digit
		      (let* ((c1 (read-a-char))
			     (d1 (hex-value c1))
			     (c2 (read-a-char))
			     (d2 (hex-value c2)))
			(cond
			 ((and d1 d2)
			  ;; Well-formed hex escape: convert to char
			  (let ((c (code-char (+ (* d1 16) d2))))
			    (vector-push-extend c token)))
			 (t
			  ;; Incorrect hex escape: take chars literally
			  (vector-push-extend c token)
			  (when c1
			    (vector-push-extend c1 token))
			  (when c2
			    (vector-push-extend c2 token))))))
		     (t
		      ;; Normal character
		      (vector-push-extend c token))))
	       ;; Return resulting token
	       token))
	   )
    (loop
     while (> content-length 0)
     collect (cons (or (decode-token #\=) "")
		   (or (decode-token #\&) "")))))
(defun hex-value (c)
  "Return the value of the given character as a hexadecimal digit. Characters
outside the range 0-9a-fA-F will return NIL."
  (case c
    ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
     (- (char-code c) (char-code #\0)))
    ((#\a #\b #\c #\d #\e #\f)
     (+ (- (char-code c) (char-code #\a)) 10))
    ((#\A #\B #\C #\D #\E #\F)
     (+ (- (char-code c) (char-code #\A)) 10))
    (t
     nil)))

(defun parse-int (s)
  "Return the integer denoted by the string S. Parsing terminates at the
first non-digit."
  (loop with n = 0
	for i from 0 to (1- (length s))
	while (member (char s i) '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
	do (setq n (+ (* n 10) (- (char-code (char s i)) (char-code #\0))))
	finally (return n)))

(defun now ()
  "Returns an MM/DD/YYYY HH:MM:SS timestamp."
  (multiple-value-bind (sec min hour date month year dow dstp tz)
      (get-decoded-time)
    (declare (ignore dow dstp tz))
    (format nil "~2,'0D/~2,'0D/~4,'0D ~2,'0D:~2,'0D:~2,'0D"
	    date month year hour min sec)))

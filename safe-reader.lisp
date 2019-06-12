(in-package :webparser)

;;;;
;;;; safe-reader.lisp - read a safe subset of Lisp safely
;;;;

(defun whitespace-char-p (c)
  "Is c a whitespace character? (why Common Lisp doesn't have this already I'll never know...)"
  (member c '(#\Space #\Tab #\Newline #\Return)))

#| old
(defun string-safe-to-read-p (str)
  "Is the given string safe to call read-from-string on?"
  (and
    ;; contains some non-whitespace characters
    (some (lambda (c) (not (whitespace-char-p c))) str)
    ;; contains only simple symbols, parens, and whitespace
    (every
      (lambda (c) 
	(or (alpha-char-p c)
	    (digit-char-p c)
	    (whitespace-char-p c)
	    (member c '(#\- #\_ #\$ #\( #\)))))
      str)
    ;; TODO check paren balance?
    ))
|#

(defun kqml-non-token-char-p (c)
  (member c '(#\' #\` #\" #\# #\( #\))))

(defun read-escaped-until (s end-char)
  "Read characters until end-char is the next character to be read. Cause an
   error if the stream ends before then. If a backslash is encountered, escape
   the next character."
  (loop for next-char = (peek-char nil s)
        until (char= next-char end-char)
	when (char= next-char #\\)
	  do (setf next-char (read-char s))
	collect (read-char s) into char-list
	finally (return (format nil "~{~c~}" char-list))
	))

(declaim (ftype function read-sexpr-safely))

(define-condition unmatched-close-paren (error) ())

(defun read-list-safely (s)
  (read-char s) ; discard open paren
  (loop for item =
    (handler-case (read-sexpr-safely s)
      (unmatched-close-paren ()
	(read-char s) ; discard close paren
	(return ret)))
    collect item into ret)
  )

(defun read-name (s)
  "Read the package-name or symbol-name part of a symbol (no colons), as a
   string, upcased unless pipequoted."
  (case (peek-char nil s nil)
    (#\|
      (let (ret)
	(read-char s) ; discard open pipequote
	(setf ret (read-escaped-until s #\|))
	(read-char s) ; discard close pipequote
	(when (some #'kqml-non-token-char-p ret)
	  (error "pipequoted lisp symbol contains KQML non-token character(s): ~s" ret))
	ret))
    (otherwise
      (loop for next-char = (peek-char nil s nil)
            while (and next-char
	               (or (alpha-char-p next-char)
			   (digit-char-p next-char)
			   (member next-char '(#\+ #\- #\_ #\$ #\*)))) ; TODO more?
	    collect (read-char s) into char-list
	    finally (return (format nil "~:@(~{~c~}~)" char-list))
	    ))
    ))

(defun read-keyword-safely (s)
  (read-char s) ; discard :
  (intern (read-name s) :keyword))

(defun read-symbol-safely (s)
  (let ((first-name (read-name s))
        (next-char (peek-char nil s nil)))
    (cond
      ((and next-char (char= next-char #\:))
        (let ((pkg (find-package first-name)))
	  (unless pkg
	    (error "unknown package ~s" first-name))
	  (read-char s) ; discard first :
	  (unless (char= (read-char s) #\:)
	    (error "missing second colon between symbol package and symbol name"))
	  (let ((second-name (read-name s)))
	    (intern second-name pkg))
	  ))
      (t ; no package
        (intern first-name))
      )))

(defun read-digits (s)
  "Read digits as chars, until a non-digit is the next char to be read."
  (loop for next-char = (peek-char nil s nil)
        while (and next-char (digit-char-p next-char))
	collect (read-char s)
	))

(defun read-number-safely (s)
  "Read an integer or decimal number, or fall back on reading a symbol if a
   sign is not followed by a digit. Assumes the next char to be read is a sign
   or a digit."
  (case (peek-char nil s)
    ((#\- #\+)
      (let ((sign (read-char s)))
	(cond
	  ((digit-char-p (peek-char nil s)) ; FIXME am I allowed to peek and then unread?
	    ;; sign followed by digit; read the absolute value, apply the sign
	    (let ((abs-num (read-number-safely s)))
	      (if (char= sign #\-)
		(- abs-num)
		abs-num
		)))
	  (t
	    ;; sign not followed by digit; read it as a symbol instead
	    (unread-char #\- s)
	    (read-symbol-safely s)
	    )
	  )
	))
    (otherwise ; digit
      ;; read digits, then if there is a decimal point, read it and more
      ;; digits, then use the regular lisp reader to read the chars we just
      ;; read (i.e. read something matching /^\d*(\.\d*)?$/)
      (let ((chars (read-digits s))
            (next-char (peek-char nil s nil)))
        (when (and next-char (char= next-char #\.))
	  (nconc chars (list (read-char s)) (read-digits s)))
	(read-from-string (format nil "~{~c~}" chars))
	))
    ))

(defun read-string-safely (s)
  (let (ret)
    (read-char s) ; discard open "
    (setf ret (read-escaped-until s #\"))
    (read-char s) ; discard close "
    ret))

(defun read-sexpr-safely (s)
  "Read an S-expression safely (no evaluation, no excessive reader macros, just
   plain old lists and atoms). Cause an error if the expression being read is
   not known to be safely readable."
  (let ((c (peek-char nil s)))
    (cond
      ((char= c #\()
        (read-list-safely s))
      ((char= c #\))
        (error 'unmatched-close-paren))
      ((char= c #\:)
        (read-keyword-safely s))
      ((whitespace-char-p c)
        (read-char s)
	(read-sexpr-safely s))
      ((or (alpha-char-p c) (member c '(#\_ #\$ #\|)))
        (read-symbol-safely s))
      ((or (digit-char-p c) (member c '(#\- #\+)))
        (read-number-safely s))
      ((char= c #\")
        (read-string-safely s))
      (t
        (error "unsafe character ~s" c))
      )))

(defun read-safely-from-string (str)
  (with-input-from-string (s str) (read-sexpr-safely s)))


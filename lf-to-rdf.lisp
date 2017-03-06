(in-package :webparser)

;;; See also convert-*-to-dot in ../NewIM/Manager.lisp, and old version of
;;; lf-to-rdf in ../LFEvaluator/lf-to-rdf.lisp.

(defun plural-lf-role-to-rdf-stream (role value s &key remove-final-s append-symbol append-index)
  "Split a plural role like :mods or :and into separate singular roles while
   writing to an RDF stream."
  (let ((prefix (string role)))
    (when remove-final-s
      (setf prefix (subseq prefix 0 (- (length prefix) 1))))
    (loop for v in value
          for i upfrom 0
	  for suffix =
	    (cond
	      (append-symbol (format nil "-~a" append-symbol))
	      ((and append-index (> i 0)) (format nil "~a" i))
	      (t "")
	      )
	  do (format s "    <role:~a~a rdf:resource=\"#~a\" />~%"
	             prefix suffix (escape-for-xml nil v))
	  )))

(defun lf-to-rdf-stream (lf-terms s)
  "Write RDF representing the given LF terms to the given stream."
  (format s "<rdf:RDF
  xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"
  xmlns:role=\"http://www.cs.rochester.edu/research/trips/role#\"
  xmlns:LF=\"http://www.cs.rochester.edu/research/trips/LF#\">~%")
  (dolist (term (convert-to-package lf-terms))
    (let ((indicator (first term))
	  (id (second term))
	  (type-word (third term))
	  (roles (cdddr term))
	  type word)
      ;; interpret the different kinds of third element
      (if (consp type-word)
	(cond
	  ((eq :* (car type-word))
	    (setf type (second type-word))
	    (setf word (third type-word))
	    )
	  ((eq 'set-of (car type-word))
	    (setf type 'set)
	    (setf roles (append (list :of (second type-word)) roles))
	    )
	  (t
	    (error "Unknown type of third element of a term: ~s" type-word))
	  )
	;; not a list
	(setf type type-word)
	)
      (format s "  <rdf:Description rdf:ID=\"~a\">~%" (escape-for-xml nil id))
      (format s "    <LF:indicator>~a</LF:indicator>~%"
              (escape-for-xml nil indicator))
      (format s "    <LF:type>~a</LF:type>~%" (escape-for-xml nil type))
      (when word
	(format s "    <LF:word>~a</LF:word>~%" (escape-for-xml nil word)))
      (loop while roles
	    for role = (pop roles) then (pop roles)
	    for value = (pop roles) then (pop roles)
	    do
	(cond
	  ((eq :TMA role)
	    ;; expand TMA into separate roles
	    (dolist (pair (reverse value))
	      (push (second pair) roles)
	      (push (intern (symbol-name (first pair)) :keyword) roles)
	      ))
	  ;; put these in the LF namespace since they're not really roles
	  ((member role '(:start :end))
	    (format s "    <LF~(~s~)>~a</LF~(~s~)>~%"
	            role (escape-for-xml nil value) role))
	  ;; plural roles
	  ((member role '(:members :mods))
	    (plural-lf-role-to-rdf-stream role value s
	        :remove-final-s t))
	  ((member role '(:and :or))
	    (plural-lf-role-to-rdf-stream role value s
	        :append-symbol 'element))
	  ((and (eq role :sequence) (listp value))
	    (plural-lf-role-to-rdf-stream role value s
	        :append-index t))
	  ((eq role :acts)
	    (plural-lf-role-to-rdf-stream role value s
	        :remove-final-s t :append-index t))
	  ; should never happen in this application
	  ; ((eq role :domain-info)
	  ;   )
	  ((is-trips-variable value)
	    ;; write the role as a resource reference
	    (format s "    <role~s rdf:resource=\"#~a\" />~%"
	            role (escape-for-xml nil value)))
	  ;; TODO separate out parts of role values like
	  ;; (:* length-unit meter) for :UNIT and (:* possibility may) for
	  ;; :modality
	  (t
	    ;; write the role as a string
	    (format s "    <role~s>~a</role~s>~%"
	            role (escape-for-xml nil value) role))
	  ))
      (format s "  </rdf:Description>~%")
      ))
  (format s "</rdf:RDF>~%")
  )


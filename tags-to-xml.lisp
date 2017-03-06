(in-package :webparser)

(defvar +dsi-arg-types+ '( ; should be defconstant, but that doesn't work
  (term
    :id foreign-symbol
    :name string
    :score float
    :matches matches ; special case
    :species string
    :dbxrefs (list-of foreign-symbol)
    :mappings mappings ; special case
    :ont-types (list-of symbol)
    :member-type t
    :members (list-of t)
    :pmod t
    )
  (specialist
    :eui symbol
    :cat symbol
    :citation-form string
    :matches matches ; special case
    :complements (list-of string)
    :nominalization (list-of dsi)
    :nominalization-of (list-of dsi)
    :abbreviation (list-of dsi)
    :abbreviation-of (list-of dsi)
    :acronym (list-of dsi)
    :acronym-of (list-of dsi)
    )
  (mutation
    :type symbol
    :old dsi
    :aa-index integer
    :lower dsi
    :upper dsi
    :new dsi ; might also be a (list-of dsi), fudged below
    )
  (aa-site
    :name string
    :letter string
    :index integer
    )
  (amino-acid
    :name string
    :letter string
    )
  (mirna
    :type symbol
    :number string
    :species string
    )
  (umls
    :cui symbol
    :concept string
    :preferred string
    :semantic-types (list-of symbol)
    :semantic-groups (list-of symbol)
    :nci-codes (list-of symbol)
    :nci-isa (list-of symbol)
    :snomedct-codes (list-of symbol)
    :snomedct-isa (list-of symbol)
    :ancestors (list-of dsi)
    :sources (list-of symbol)
    )
  )
  "An assoc list from domain-specific-info types to keyword-argument lists
   mapping their arguments to the arguments' types. This also provides the
   order in which arguments should appear in the XML output (except that
   attributes must come before child elements).")

(defun dsi-to-xml-stream (dsi s)
  "Turn an individual :domain-specific-info structure from TextTagger into XML."
  (loop with kind = (car dsi)
        with attrs = nil
	with children = nil
        for arg-types = (cdr (assoc kind +dsi-arg-types+)) then (cddr arg-types)
	while arg-types
	for arg = (first arg-types)
	for arg-str = (symbol-name arg)
	for singular-arg =
	  (if (char-equal #\s (elt arg-str (- (length arg-str) 1)))
	    (intern (subseq arg-str 0 (- (length arg-str) 1)))
	    arg)
	for type = (second arg-types)
	for val = (find-arg-in-act dsi arg)
	do (when val
	     (cond
	       ;; simple types turn directly into XML attributes
	       ((member type '(integer float string symbol t))
	         (push arg attrs)
		 (push val attrs)
		 )
	       ((eq type 'foreign-symbol) ; package, one colon, no pipequotes
	         (push arg attrs)
	         (push (format nil "~a:~a" (package-name (symbol-package val)) (symbol-name val)) attrs)
		 )
	       ;; nested :domain-specific-info structures turn into children
	       ;; (some dsi-typed things can also be (list-of dsi)-typed, so
	       ;; check for that too)
	       ((and (eq type 'dsi) (symbolp (car val)))
	         (push
	           (with-output-to-string (c)
		     (multiple-value-bind (chldrn tag-name)
		         (format-xml-start c (list arg ""))
			 (declare (ignore chldrn))
		       (dsi-to-xml-stream val c)
		       (format-xml-end c tag-name)
		       ))
		   children))
	       ((or (eq type 'dsi) (equalp type '(list-of dsi)))
	         (push
	           (with-output-to-string (c)
		     (multiple-value-bind (chldrn tag-name)
		         (format-xml-start c (list arg ""))
			 (declare (ignore chldrn))
		       (dolist (v val)
			 (dsi-to-xml-stream v c))
		       (format-xml-end c tag-name)
		       ))
		   children))
	       ;; lists of symbols get comma-separated in attributes
	       ((equalp type '(list-of symbol))
	         (push arg attrs)
	         (push (format nil "~{~a~^,~}" val) attrs)
		 )
	       ((equalp type '(list-of foreign-symbol))
	         (push arg attrs)
		 (push (format nil "~{~a~^,~}"
		 	       (mapcar
			         (lambda (v)
			           (format nil "~a:~a"
				        (package-name (symbol-package v))
					(symbol-name v)))
			         val))
		       attrs)
		 )
	       ;; lists of strings or unspecified type turn into multiple
	       ;; children with text contents
	       ((or (equalp type '(list-of string)) (equalp type '(list-of t)))
	         (dolist (v val)
		   (push (format nil "    <~(~a~)>~a</~(~a~)>~%"
		                 singular-arg (escape-for-xml nil v)
				 singular-arg)
		         children)))
	       ;; mappings are weird
	       ((eq type 'mappings)
	         (dolist (v val)
		   (let* ((thru-id (second (find-arg-in-act v :through)))
		          (thru-pkg (package-name (symbol-package thru-id)))
			  (thru-name (symbol-name thru-id)))
		     (push (with-output-to-string (c)
		             (format-xml c
			       `(map :to
			              ,(format nil "~a" (find-arg-in-act v :to))
			             :through
				      ,(format nil "~a:~a" thru-pkg thru-name)
				     )))
			   children))))
	       ;; matches are weird too
	       ((eq type 'matches)
		 ;; matches happen to be directly usable as XML S-expressions,
		 ;; because their arguments all become attributes
		 (dolist (v val)
		   (push (with-output-to-string (c)
			   (format-xml c v))
			 children)))
	       (t (error "Unknown domain-specific-info argument type: ~s" type))
	       ))
	finally
	  (multiple-value-bind (chldrn tag-name)
	      (format-xml-start s `(,kind ,@(nreverse attrs) ,@children))
	      (declare (ignore chldrn))
	    (when children
	      (format s "~{~a~}" (nreverse children))
	      (format-xml-end s tag-name)
	      ))
	))

(defun sense-info-to-xml-stream (sense-info s)
  "Turn an individual sense info structure from TextTagger into XML."
  ;; TODO also use dsi-to-xml-stream for sense-info itself? want to omit <domain-specific-info> wrapping
  (loop with xml = (list 'sense-info)
        for k in '(:penn-parts-of-speech :trips-parts-of-speech :ont-types :wn-sense-keys :alternate-spellings)
	for v = (find-arg sense-info k)
	when v
	do
	  (push k xml)
	  (push (format nil "~{~a~^,~}" v) xml)
	finally
	  (destructuring-bind
	      (&key score domain-specific-info &allow-other-keys)
	      sense-info
	    (when score
	      (push :score xml)
	      (push score xml)
	      )
	    (when domain-specific-info
	      (push "" xml))
	    (format-xml-start s (nreverse xml))
	    (when domain-specific-info
	      (dolist (dsi (mapcan #'cdr domain-specific-info))
		(dsi-to-xml-stream dsi s))
	      (format-xml-end s "sense-info")
	      )
	    )
	))

(defun tags-to-xml-stream (tags s)
  "Turn the list of tags (messages contents from TextTagger) into XML."
  (dolist (tag tags)
    (destructuring-bind (kind lex &key frame sense-info penn-cats score &allow-other-keys) tag
      (ecase kind
	((word prefix)
	  (multiple-value-bind (children tag-name)
	      (format-xml-start s
	        `(,kind :lex ,lex
		  :start ,(first frame) :end ,(second frame)
		  ,@sense-info))
	      (declare (ignore children))
	    (when sense-info
	      (dolist (si sense-info)
	        (sense-info-to-xml-stream si s))
	      (format-xml-end s tag-name)
	      )))
	(prefer
	  (format-xml s
	    `(prefer :text ,lex :start ,(first frame) :end ,(second frame)
	      :penn-cats ,(format nil "~{~a~^,~}" penn-cats)
	      ,@(when score `(:score ,score))
	      )))
	))))


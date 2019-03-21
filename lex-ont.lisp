(defpackage :weblex (:use :common-lisp))
(in-package :weblex)
(load #!TRIPS"src;LexiconManager;Code;WebLex;lisp2xml.lisp")
(load #!TRIPS"src;LexiconManager;Code;WebLex;onttypes2xml.lisp")
(load #!TRIPS"src;LexiconManager;Code;WebLex;words2xml.lisp")

(defun get-sense-source (gwd-w pos ont-type template)
  "Get the s-expression from the lisp source file that defines the sense of the
   word gwd-w associated with the given pos, ont-type, and template name, or
   nil if it can't be found. Also add the default template if necessary."
  (let* ((*package* (find-package :weblex))
         (first-word (if (consp gwd-w) (car gwd-w) gwd-w))
         (filename (string-downcase (symbol-name first-word)))
         (path
	   (when (webparser::name-p filename)
	     (probe-file (make-pathname
	       :defaults #!TRIPS"src;LexiconManager;Data;new;"
	       :name filename :type "lisp")))))
    (setf template (intern (symbol-name template))) ; webparser:: -> weblex::
    (when path
      ;(format t "reading lexicon file ~s~%" path)
      (with-open-file (f path :direction :input)
        (loop named defs
	      for def = (read f nil)
	      while def
	      when (eq 'define-words (car def))
	      do
	        (let ((dw-pos (getk def :pos))
		      (default-templ (list (getk def :templ)))
		      (words (getk def :words)))
		  ;(format t "  define-words :pos ~s :templ ~s~%" dw-pos default-templ)
		  (when (eq pos dw-pos)
		    (dolist (word-with-senses words)
		      (let* ((dw-word (car word-with-senses))
		             (dw-word-flat ; get rid of parens around particles
			       (if (consp dw-word)
			         (util::flatten dw-word)
				 dw-word))
		             (senses (geta (cdr word-with-senses) 'senses)))
		        ;(format t "    word ~s~%" dw-word)
		        (when (equalp gwd-w dw-word-flat)
			  (dolist (sense senses)
			    (let ((dw-ont-type (geta1 sense 'lf-parent))
			          (dw-templ (geta sense 'templ)))
			      ;(format t "      sense ~s ~s~%" dw-ont-type dw-templ)
			      (when (and (eq ont-type dw-ont-type)
			                 (eq template
					     (first (or dw-templ
							default-templ))))
			        ;(format t "        bingo!~%~s~%" sense)
				(unless dw-templ
				  (push (cons 'templ default-templ) sense))
			        (return-from defs sense)
				))))))))
	      finally (return-from defs nil)
	      )))))

(in-package :webparser)
(in-component :webparser)

(defun format-xml-header (s &key (encoding "UTF-8") xsl doctype dtd)
  (format s "<?xml version=\"1.0\" encoding=~s?>~%" encoding)
  (when xsl
    (format s "<?xml-stylesheet type=\"text/xsl\" href=~s?>~%" xsl))
  (cond
    ((equalp doctype "html")
      (format s "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">~%"))
    (dtd
      (format s "<!DOCTYPE ~a SYSTEM ~s>~%" doctype dtd))
    )
  )

(defun lex-autocomplete (q)
  `(http 200 :content-type "text/plain; charset=utf-8" :content ))

(defun get-word-modified-date (word)
  (if (name-p word)
    (let* ((word-path (make-pathname :defaults #!TRIPS"src;LexiconManager;Data;new;" :name (string-downcase word) :type "lisp"))
           (word-true-path (probe-file word-path)))
      (if word-true-path
        (get-file-change-date-string (format nil "~a" word-true-path))
	(get-build-date)
	))
    (get-build-date)
    ))

(defun sense-key-to-id (sk)
  "Convert a sense key to an XML ID by converting %s and :s to -s."
  (substitute #\- #\: (substitute #\- #\% sk)))

(defun id-to-sense-key (id)
  "Inverse of sense-key-to-ncname, if id was a well-formed sense key; otherwise
   nil."
  (and (>= (count #\- id) 5) ; has 5 fields prefixed with -
       (destructuring-bind (ss-type lex-filenum lex-id head head-id)
           (last (util:split-string id :on #\-) 5)
	 (and (= 1 (length ss-type))
	      (member ss-type '("1" "2" "3" "4" "5") :test #'string=)
	      (= 2 (length lex-filenum))
	      (every #'digit-char-p lex-filenum)
	      (= 2 (length lex-id))
	      (every #'digit-char-p lex-id)
	      (or (and (= 0 (length head)) (= 0 (length head-id)))
	          (and (< 0 (length head)) (= 2 (length head-id))
		       (every #'digit-char-p head-id)))
	      ;; well formed; make the sense key
	      (let* ((code-len
	               (+ 1 (length ss-type)
		          1 (length lex-filenum)
			  1 (length lex-id)
			  1 (length head)
			  1 (length head-id)))
	             (lemma-len (- (length id) code-len))
		     (lemma (subseq id 0 lemma-len)))
	        (format nil "~a%~{~a~^:~}"
		    lemma (list ss-type lex-filenum lex-id head head-id)))
	      ))))

(defun synset-to-ancestorses (ss)
  "Get the lists of ancestors of the given synset (including the synset
   itself), each ordered from root to leaf."
  (let* ((wf::*parent-offset-list* nil))
    (wf::get-hierarchy wf::wm ss)))

(defun synset-to-ont-types (ss)
  (remove-duplicates
      (mapcar #'wf::convert-hierarchy (synset-to-ancestorses ss))))

;; FIXME:
;; The tree view on the ont browser page is designed for a single-inheritance
;; hierarchy (i.e. a tree), but WordNet has multiple inheritance (including
;; diamond inheritance like that of elephant%1:05:00::, under both pachyderm
;; and proboscidean, which are both under placental_mammal). This means that
;; some synsets should appear in multiple places in the displayed tree, but
;; they don't because those places would have the same id attribute, so only
;; one of them can exist. We try to find the path to the root to expand based
;; on how a WN synset is mapped using wordnet-ancestors-of-mapped-synset, but
;; this isn't guaranteed to expand the path that actually exists in the display
;; if there is more than one such path.
(defun wordnet-ancestors-of-mapped-synset (ss ont-type)
  "Get the list of ancestor synsets between the (indirectly) mapped synset ss
   and the corresponding synset that is directly mapped to ont-type,
   inclusive, ordered from the directly mapped synset to the given ss. If ss
   does not map to ont-type, return nil."
  (loop for ancestors in (synset-to-ancestorses ss)
        for ot = (wf::convert-hierarchy ancestors)
	when (eq ont-type ot)
        return
	  (member ont-type
		  (if (eq 'wf::|s| (wf::get-ss-type ss)) ; satellite adj
		    (list (wf::get-head-adjective wf::wm ss) ss) ; head is anc
		    ancestors)
		  :test #'member ; so meta!
		  ;; FIXME? this is inefficient, and potentially wrong if there
		  ;; is more than one ancestor directly mapped to ont-type
		  ;; (maybe with an ancestor mapped to a different ont-type
		  ;; between two of them)
		  :key #'synset-to-ont-types)
	finally (return nil)
	))

(defvar *trips2wn-pos* '(
  (w::n "noun")
  (w::v "verb")
  (w::adj "adj")
  (w::adv "adv")
  ) "inverse of wf::convert-wordnet-pos-to-trips")

(defun maybe-wordnetify-sense-xml (gwd-w w_str pos xml)
  "If the sense was looked up from WordNet instead of our lexicon, return a new
   version of the sense XML with the WordNet sense that was used to map it to
   the ONT type from the original sense XML as the new class, and any
   intermediate synsets as extra ancestors. Also include the gloss from WN."
  (let* ((ont-type
           (intern (string-upcase (util:find-arg-in-act xml :onttype)) :ont))
	 (in-trips-p (member (list gwd-w pos) (lxm::get-words-from-lf ont-type)
			     :test #'equalp)))
    (if in-trips-p
      xml
      ; else not in TRIPS, therefore in WN
      (loop with old-ancestors = (util:find-arg-in-act xml :ancestors)
            with wn-pos = (second (assoc pos *trips2wn-pos*))
	    for ss in (wf::get-synsets wf::wm w_str wn-pos)
	    do
	      (let ((wn-anc-sss
		      (wordnet-ancestors-of-mapped-synset ss ont-type)))
	        (when wn-anc-sss
		  (let* ((wn-anc-ids (reverse (mapcar
			   (lambda (anc-ss)
			     (sense-key-to-id (first
			         (wf::sense-keys-for-synset anc-ss))))
			   wn-anc-sss)))
		         (wn-class-id (pop wn-anc-ids))
			 (new-words (format nil "~{~a~^,~}" (wf::wn-lemmas ss)))
			 (new-ancestors
			   (format nil "~{~a,~}~(~a~),~a"
			       wn-anc-ids ont-type old-ancestors)))
		    (return
		      `(class :onttype ,wn-class-id
			      :words ,new-words
			      :ancestors ,new-ancestors
			      :gloss
				;; I'm pretty sure WN 3.0 doesn't have multiple
				;; glosses separated by | like this, but that's
				;; how WF reads them, so just in case, I'll put
				;; the | back here
			        ,(format nil "~{~a~^ | ~}"
					 (slot-value ss 'wf::glosses))
			      ,@(nthcdr 7 xml)
			      )))))
	    finally (return xml) ; just in case we don't find it for some reason
	    ))))

(defun lex-xml (q)
  (unless q (setf q "word"))
  (let* (
         ;; the search word in different forms:
	 ;; list of w:: symbols
         (w-list (mapcar (lambda (str)
			   (intern (string-upcase str) :w))
			 (util:split-string q)))
	 ;; the w arg for lxm::get-word-def (w:: symbol for 1 word; list of w::
	 ;; symbols for multiword)
	 ;; FIXME? particle verbs like (w::look (w::up))
         (gwd-w (if (= 1 (length w-list)) (car w-list) w-list))
	 ;; single w:: symbol, with hyphens between words
         (w-sym (intern (format nil "~{~a~^-~}" w-list) :w))
	 ;; single lowercase string, with underscores between words
	 (w_str (weblex::normalize-word-name w-list))
	 (word-defs
	   (let ((*package* (find-package :webparser)))
	     (send-and-wait `(request :receiver lxm :content
		 (get-word-def ,gwd-w nil))))))
    (if (and (= 1 (length word-defs))
	     (equalp `(:* ont::referential-sem ,w-sym)
		     (second (assoc 'w::lf (cddr (fourth (car word-defs)))))))
      '(http 404) ;; referential-sem means not found
      `(http 200 ;; else found
	:content-type "text/xml; charset=utf-8"
	:content ,(with-output-to-string (s)
	  (format-xml-header s :xsl "../style/word.xsl"
			       :doctype "WORD" :dtd "../word.dtd")
	  (format s "~a"
	    (weblex::convert-lisp-to-xml
	      (weblex::sort-pos-and-classes
	        `(weblex::word :name ,w_str
			       :modified ,(get-word-modified-date q)
			       ,@(when lxm::*use-trips-and-wf-senses*
			           '(:use-trips-and-wf-senses "T"))
		  ,@(loop for def in word-defs
			  for (pct pos . feats) = (fourth def)
			  for (colon-star ont-type lemma) =
			    (second (assoc 'w::lf feats))
			  for template = (second (assoc 'w::template feats))
			  when (string-equal (symbol-name lemma)
					     (util:replace-space-with-hyphen q))
			  collect
			    `((weblex::pos :name ,pos
			       ,(maybe-wordnetify-sense-xml gwd-w w_str pos
			          (weblex::sense-to-xml
				    (or (weblex::get-sense-source
					    gwd-w pos ont-type template)
					`((weblex::lf-parent ,ont-type)
					  (weblex::templ ,template)))
				    nil))))
			  into xmls
			  finally (return 
			    (mapcar
			      (lambda (pos-xml)
			        ;; add morph
			        (let ((pos (third pos-xml)))
				  `(weblex::pos :name ,pos
				    ,@(weblex::get-morphs-xmls gwd-w pos)
				    ,@(cdddr pos-xml))))
			      (reduce #'weblex::merge-pos-xml-lists xmls
				      :initial-value nil)))
			  )))))
	  )
	)
      )
    ))

(defun ont-autocomplete (q)
  `(http 200 :content-type "text/plain; charset=utf-8" :content ))

(defun arguments-has-all-roles (arguments roles)
  (every
    (lambda (r)
      (member r arguments :key #'om::sem-argument-role))
    roles))

(defun ont-type-has-all-roles (ont-type roles)
  (arguments-has-all-roles
      (slot-value
          (gethash ont-type (om::ling-ontology-lf-table om::*lf-ontology*))
	  'om::arguments)
      roles))

(defun ont-types-for-roles (q)
  `(http 200
    :content-type "text/xml; charset=utf-8"
    :content ,(with-output-to-string (s)
      (format-xml-header s)
      (loop with roles =
	      (mapcar
	        (lambda (role-str)
		  (intern (string-upcase role-str) :ont))
		(util:split-string q :on #\,))
            with ont-types = nil
	    with ancestors = nil
	    for desc being the hash-values of
	      (om::ling-ontology-lf-table om::*lf-ontology*)
	    do
	      (with-slots (om::name om::arguments om::parent) desc
		(when (arguments-has-all-roles om::arguments roles)
		  (unless (ont-type-has-all-roles om::parent roles)
		    (push om::name ont-types)
		    (dolist (anc (reverse (cons om::name
						(om::get-parents om::name))))
		      (pushnew anc ancestors)))
		  ))
	    finally (return
	      (format-xml s `(ont-types-for-roles
		:ancestors ,(format nil "~(~{~a~^,~}~)" (reverse ancestors))
		:ont-types-with-roles ,(format nil "~(~{~a~^,~}~)" ont-types))))
	    ))))

(defun ont-xhtml-page (&optional footer)
  `(html
    (head
      (title "TRIPS+WN Ontology Browser")
      (style :type "text/css" "ul { padding-left: 1em; }")
      (script :type "text/javascript" :src "../style/onttype.js" "")
      (script :type "text/javascript" :src "../jquery/jquery-latest.js" "")
      (link :rel "stylesheet" :href "../jquery/jquery.autocomplete.css"
	    :type "text/css")
      (script :type "text/javascript"
	      :src "../jquery/jquery.bgiframe.min.js" "")
      (script :type "text/javascript"
	      :src "../jquery/jquery.dimensions.js" "")
      (script :type "text/javascript"
              :src "../jquery/jquery.autocomplete.js" "")
      (script :type "text/javascript" "
$(document).ready(function(){
$('#search-input').autocomplete('lex-ont?side=ont&ret=autocomplete');
loadONTLI('root');
});
")
      )
    (body
      (h1 "TRIPS+WN Ontology Browser")
      (form :action "lex-ont"
	(input :type "hidden" :name "side" :value "ont")
	(div
	  (input :type "text" :size "40" :name "q" :id "search-input")
	  (input :type "submit" :value "Search for ONT type")
	  ))
      (form :onsubmit "return filterByRoles()"
        (div
	  (input :type "text" :size "40" :name "roles")
	  (input :type "submit" :value "Filter by roles")
	  ))
      (ul :style "list-style: none; padding-left: 0em"
	(li :id "root"))
      ,@footer
      )
    ))

(defun ont-xml (q)
  (let ((sk (id-to-sense-key q)))
    (cond
      (sk
        (let ((ss (wf::get-synset-from-sense-key wf::wm sk)))
	  (if ss
	    `(http 200
	      :content-type "text/xml; charset=utf-8"
	      :content ,(with-output-to-string (s)
	        (format-xml-header s :xsl "../style/onttype.xsl"
				     :doctype "ONTTYPE" :dtd "../onttype.dtd")
		(format-xml s
		  `("ONTTYPE" :name ,q
		     ;; words (except stoplisted)
		     ,@(mapcan
		         (lambda (lemma)
			   (unless (wf::stoplist-p
				       (first (wf::sense-key-for-word-and-synset
						  lemma ss)))
			     `(("WORD" :name ,lemma))))
			 (wf::wn-lemmas ss))
		     ;; children (except stoplisted or mapped elsewhere)
		     ,@(let ((ont-types (synset-to-ont-types ss)))
			 (mapcan
			   (lambda (pair)
			     (let* ((child-ss (second pair))
				    (child-ots (synset-to-ont-types child-ss))
				    (child-sks
				      (wf::sense-keys-for-synset child-ss))
				    (child-id
				      (sense-key-to-id (first child-sks))))
			       (unless (or (every #'wf::stoplist-p child-sks)
					   (null (intersection ont-types
							       child-ots)))
				 `(("CHILD" :name ,child-id)))))
			   (wf::get-pointers-by-relationship wf::wm ss
			     (if (eq 'wf::|a| (wf::get-ss-type ss))
			       "&" ; similar to (satellite of head adj)
			       "~" ; hyponym
			       ))
			   ))))
	        ))
	    `(http 404))))
      (q
	(let* ((ont-type (intern (string-upcase q) :ONT))
	       (name (string-downcase q))
	       (lf-table (om::ling-ontology-lf-table om::*lf-ontology*))
	       (desc (gethash ont-type lf-table)))
	  (if desc
	    (let ((xml (weblex::lf-description-xml desc)))
	      ;; set the modified time to the dump build date instead of now
	      (setf (second (member :modified xml)) (get-build-date))
	      ;; remove wordnet mappings and add them as children instead
	      (multiple-value-bind (mappings other-stuff)
		  (split-list
		    (lambda (x)
		      (and (consp x) (eq (car x) 'weblex::mapping)))
		    xml)
		(setf xml (nconc other-stuff
		  (mapcar
		    (lambda (m)
		      `(weblex::child :name
			 ,(sense-key-to-id (weblex::getk m :name))))
		    mappings))))
	      `(http 200
		:content-type "text/xml; charset=utf-8"
		:content ,(with-output-to-string (s)
		  (format-xml-header s :xsl "../style/onttype.xsl"
				       :doctype "ONTTYPE" :dtd "../onttype.dtd")
		  (format s "~a" (weblex::convert-lisp-to-xml xml))
		  )
		))
	    '(http 404))))
      (t
        '(http 404))
      )))

(defun ont-html (q)
  (let* ((sk (when q (id-to-sense-key q)))
         (ss (when sk (wf::get-synset-from-sense-key wf::wm sk)))
         (ont-type (cond (ss (first (synset-to-ont-types ss)))
		         (q (intern (string-upcase q) :ONT))))
	 (trips-ancestors
	   (when ont-type (reverse (cons ont-type (om::get-parents ont-type)))))
	 (wn-ancestors
	   (when sk
	     (mapcar 
	       (lambda (anc-ss)
	         (sense-key-to-id (first (wf::sense-keys-for-synset anc-ss))))
	       (wordnet-ancestors-of-mapped-synset ss ont-type)
	       )))
	 ;; include ancestors from both trips and wn, but not the class we
	 ;; actually searched for
	 (ancestors (butlast (append trips-ancestors wn-ancestors)))
	 (highlighted 
	   (cond (ss (sense-key-to-id (first (wf::sense-keys-for-synset ss))))
		 (ont-type ont-type)))
	 )
    `(http 200
      :content-type "text/html; charset=utf-8"
      :content ,(with-output-to-string (s)
	(format-xml-header s :doctype "html")
	(format-xml s (ont-xhtml-page
	  (when ancestors
	    `((script :type "text/javascript"
	      ,(format nil "
$(document).ready(function(){
~{  toggleChildren('~(~a~)');~%~}  var highlighted = '~(~a~)';
  window.location.hash = highlighted;
  document.getElementById(highlighted).style.background = '#FFFF7F'
  document.getElementById('root-modified').style.display = ''
});
" ancestors highlighted))))))))))

(defvar *lex-ont-frameset* '(http 200
  :content-type "text/html; charset=utf-8"
  :content "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Frameset//EN\"
   \"http://www.w3.org/TR/html4/frameset.dtd\">
<HTML><HEAD><TITLE>Browse TRIPS+WN Lexicon and Ontology</TITLE></HEAD>
<FRAMESET cols=\"50%, 50%\">
 <FRAME src=\"lex-ont?side=lex\" name=\"lexicon\">
 <FRAME src=\"lex-ont?side=ont\" name=\"ontology\">
</FRAMESET>
</HTML>
"))

(defun handle-lex-ont (msg query)
  (destructuring-bind (&key side ret q 
  		       use-trips-and-wf-senses
		       &allow-other-keys) query
    (reply-to-msg msg 'tell :content
      (let ((*package* wf::*wf-package-var*)) ; nip WF pkg problems in the bud
	(cond
	  ((equalp side "lex")
	    (cond
	      ((equalp ret "autocomplete") (lex-autocomplete q))
	      (t
	        ;; temporarily set utawfs to requested value, saving old value
		;; also temporarily set wfsl really high
	        (let ((old-utawfs lxm::*use-trips-and-wf-senses*)
		      (old-wfsl lxm::*wf-sense-limit*))
		  (setf lxm::*use-trips-and-wf-senses* 
			(when use-trips-and-wf-senses t)
			lxm::*wf-sense-limit* 1000000)
		  (unwind-protect (lex-xml q) ;; do the word lookup
		    ;; reset utawfs to old value
		    (setf lxm::*use-trips-and-wf-senses* old-utawfs
		          lxm::*wf-sense-limit* old-wfsl))))
	      ))
	  ((equalp side "ont")
	    (cond
	      ((equalp ret "autocomplete") (ont-autocomplete q))
	      ((equalp ret "types-for-roles") (ont-types-for-roles q))
	      ((equalp ret "xml") (ont-xml q))
	      (t (ont-html q))
	      ))
	  (t *lex-ont-frameset*)
	  )))))

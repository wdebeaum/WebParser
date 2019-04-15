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

(defun lex-autocomplete (q limit)
  (let (results)
    ;; get matches in our lexicon first
    (loop for bf in (lxm::lexicon-db-base-forms lxm::*lexicon-data*)
	  for w = (make-word-from-anything bf)
	  while (or (not limit) (< (length results) limit))
	  when (or (search q (word-str  w) :test #'string-equal)
		   (search q (word-str_ w) :test #'string-equal)
		   (search q (word-str- w) :test #'string-equal))
	  do (push (word-str_ w) results))
    ;; then if there aren't enough already, try getting WN lemmas
    (loop for pos being the hash-keys of (slot-value wf::wm 'wf::indices)
            using (hash-value index)
	  while (or (not limit) (< (length results) limit))
	  do
      (loop for lemma being the hash-keys of index
            for w = (make-word-from-string lemma)
	    when (or (search q (word-str  w) :test #'string-equal)
		     (search q (word-str_ w) :test #'string-equal)
		     (search q (word-str- w) :test #'string-equal))
	    do (push (word-str_ w) results)))
    `(http 200 :content-type "text/plain; charset=utf-8" :content
       ,(format nil "~(~{~a~%~}~)" (nreverse results)))))

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

;; I'm fed up with all the subtly different representations of words in TRIPS/WN
(defstruct word
	;;	single word	multiword		use
  strs	;	("foo")		("foo" "bar")		run-morphy return
  str	;	"foo"		"foo bar"		query
  str-	;	"foo"		"foo-bar"		(intermediate form)
  str_	;	"foo"		"foo_bar"		WordNet lemmas
  syms	;	(w::foo)	(w::foo w::bar)		(intermediate form)
  sym-	;	w::foo		w::foo-bar		:* forms
  sym_	;	w::foo		w::foo_bar		(intermediate form)
  gwd	;	w::foo		(w::foo w::bar)		get-word-def arg
  dw	;	w::foo		(w::foo w::bar)		define-words arg
  	;			*or* (w::foo (w::bar))	(particle verbs)
  ; also, some words genuinely have a hyphen in them, like "hard-boil"; that
  ; becomes:
  ; strs: ("hard-boil")
  ; str, str-, str_: "hard-boil"
  ; syms, gwd, dw: (w::hard w::punc-minus w::boil)
  ; sym-: w::hard-punc-minus-boil
  ; sym_: w::hard_punc-minus_boil
  )

(defun set-word-syms-from-strs (w)
  "Given a word structure with its str* slots set, set its sym* slots."
  (with-slots (strs str- str_ syms sym- sym_ gwd dw) w
    (setf syms (if (and (= 1 (length strs))
                        (eql (- (length (car strs)) 1)
			     (position #\- (car strs))))
                 ;; looks like a prefix, so keep the final - unchanged
		 (list (intern (string-upcase (car strs)) :w))
		 ;; not a prefix, just use parser's tokenization
                 (mapcan #'parser::tokenize strs)
		 )
	  sym- (intern (format nil "~{~a~^-~}" syms) :w)
	  sym_ (intern (format nil "~{~a~^_~}" syms) :w)
	  gwd  (if (= 1 (length syms)) (car syms) syms)
	  dw   gwd)))

(defun set-word-strs-from-syms (w)
  "Given a word structure with its sym* slots set, set its str* slots."
  (with-slots (strs str str- str_ syms sym- sym_) w
    (setf strs (mapcar #'string-downcase (weblex::undo-punc-symbols syms))
	  str  (format nil "~{~a~^ ~}" strs)
	  str- (format nil "~{~a~^-~}" strs)
	  str_ (format nil "~{~a~^_~}" strs))
    ))

(defun make-word-from-string (str?)
  (let ((w (make-word)))
    (with-slots (strs str str- str_) w
      (cond
	((position #\_ str?)
	   (setf strs (util:split-string str? :on #\_)
		 str  (substitute #\Space #\_ str?)
		 str- (substitute #\- #\_ str?)
		 str_ str?))
; commented out to preserve hyphens in query and in WN lemmas
;	((position #\- str?)
;	   (setf strs (util:split-string str? :on #\-)
;		 str  (substitute #\Space #\- str?)
;		 str- str?
;		 str_ (substitute #\_ #\- str?)))
	(t
	   (setf strs (util:split-string str?)
		 str  str?
		 str- (substitute #\- #\Space str?)
		 str_ (substitute #\_ #\Space str?)))
	))
    (set-word-syms-from-strs w)
    w))

(defun make-word-from-string-list (strs)
  (let ((w (make-word :strs strs
                      :str  (format nil "~{~a~^ ~}" strs)
		      :str- (format nil "~{~a~^-~}" strs)
		      :str_ (format nil "~{~a~^_~}" strs))))
    (set-word-syms-from-strs w)
    w))

(defun make-word-from-symbol (sym?)
  (loop with str =
          ;; lc; s/-/ /g;
          (substitute #\Space #\- (string-downcase (symbol-name sym?)))
	;; s/ punc minus /-/g;
        for start = (search " punc minus " str)
	  then (search " punc minus " str :start2 (1+ start))
	while start
	do (setf str (concatenate 'string
	  (subseq str 0 start)
	  "-"
	  (subseq str (+ start 12))
	  ))
	finally (return (make-word-from-string str))))

(defun make-word-from-symbol-list (syms?)
  (let* ((syms (util:flatten syms?))
	 (w (make-word :syms syms
                       :sym- (intern (format nil "~{~a~^-~}" syms) :w)
		       :sym_ (intern (format nil "~{~a~^_~}" syms) :w)
		       :gwd (if (= 1 (length syms)) (car syms) syms)
		       :dw (if (= 1 (length syms?)) (car syms?) syms?))))
    (set-word-strs-from-syms w)
    w))

(defun make-word-from-list (l)
  (etypecase (car l)
    (string (make-word-from-string-list l))
    (symbol (make-word-from-symbol-list l))
    ))

(defun make-word-from-anything (x)
  (etypecase x
    (symbol (make-word-from-symbol x))
    (string (make-word-from-string x))
    (cons (make-word-from-list x))
    ))

(defstruct word-def
  word ; original (non-base-form?) word used to look up this def
  lemma ; base-form word
  pos ; TRIPS W:: pos symbol
  ont-type ; TRIPS ONT:: type symbol
  template ; TRIPS template name
  nom-of ; word this def is a nominalization of (or nil)
  def-list ; original list form of the def from get-word-def
  (synset nil) ; wf::wordnet-synset (or nil if not from WordNet)
  (sense-key nil) ; sense key string with %: (or nil)
  (wn-path nil) ; synsets on the path from ont-type to synset (including synset)
  )

(defun maybe-set-word-def-synset (d wn-word wn-pos synsets)
  "Try to set the synset, sense-key, and wn-path slots of a word-def struct,
   using one of the given synsets, and the given word and POS."
  (loop for ss in synsets
	for wn-path =
	  (wordnet-ancestors-of-mapped-synset ss (word-def-ont-type d))
	when wn-path
	do (setf (word-def-synset d) ss
		 (word-def-sense-key d)
		   (wf::sense-key-for-word-and-synset wn-word ss)
		 (word-def-wn-path d) wn-path)
	   (return t)
	finally (return nil)
	))

(defvar *trips2wn-pos* '(
  (w::n "noun")
  (w::v "verb")
  (w::adj "adj")
  (w::adv "adv")
  ) "inverse of wf::convert-wordnet-pos-to-trips")

(defun make-word-def-from-list (q def-list)
  "Make a word-def struct from one of the lists returned by get-word-def for a
   definition of a single sense. q is the search query string."
  (destructuring-bind (pct pos . feats)
      (fourth def-list)
      (declare (ignore pct))
    (destructuring-bind (colon-star &optional (ont-type colon-star) (lemma-sym ont-type))
        (second (assoc 'w::lf feats))
      (let* ((w (make-word-from-string q))
             (l (make-word-from-symbol lemma-sym))
	     (d (make-word-def :word w :lemma l :pos pos :ont-type ont-type
			       :template (second (assoc 'w::template feats))
			       :nom-of
				 (let ((n (assoc 'w::nom-of feats)))
				   (when n
				     (make-word-from-anything (second n))))
			       :def-list def-list))
             (gwflf-entry (list (word-gwd l) pos))
	     (in-trips-p (member gwflf-entry (lxm::get-words-from-lf ont-type)
				 :test #'equalp))
	     )
        ;; try to set wordnet slots if it's not in trips
	(unless in-trips-p
	  (let* ((wn-pos (second (assoc pos *trips2wn-pos*)))
	         ;; try the search word first
		 (wn-word (word-str_ w))
	         (word-synsets (when wn-pos
				 (wf::get-synsets wf::wm wn-word wn-pos))))
	    (when (and word-synsets
		       (not (maybe-set-word-def-synset d wn-word wn-pos
						       word-synsets)))
	      ;; search word failed, try morphy (like wf::get-all-synsets does,
	      ;; but this way we can save which lemma actually worked)
	      (loop with morphy-words =
		      (second (assoc wn-pos (wf::run-morphy wf::wm wn-word)
				     :test #'string=))
		    for morphy-word in morphy-words
		    for morphy-synsets =
		      (wf::get-synsets wf::wm morphy-word wn-pos)
		    when (maybe-set-word-def-synset d morphy-word wn-pos
						    morphy-synsets)
		    do (setf (word-def-lemma d)
			     (make-word-from-string morphy-word))
		       (return nil)
		    ))))
	d))))

(defun synset-core-p (ss)
  (some #'wf::is-core-wordnet-sense (wf::sense-keys-for-synset ss)))

(defvar *core-wordnet-ancestors* nil)

(defun synset-core-ancestor-p (ss)
  (unless *core-wordnet-ancestors*
    ;; initialize to a hash table mapping all ancestors of core wordnet synsets
    ;; (including the core synsets themselves) to t
    ;; (but use (lex-filenum . offset) as keys instead of the synset object
    ;; itself, because apparently those aren't always equalp?)
    (setf *core-wordnet-ancestors* (make-hash-table :test #'equalp))
    (loop for sk being the hash-keys of wf::core-wordnet-sense-keys
          for core-ss = (wf::get-synset-from-sense-key wf::wm sk)
	  for ancestorses =
	    (when core-ss ; WTF, WN? why are there "core" senses that aren't even in WN, like annual%3:01:00::?
	      (synset-to-ancestorses core-ss))
	  do (dolist (ancestors ancestorses)
	       (loop for ancestor in ancestors
	             for key = (cons (slot-value ancestor 'wf::lex-filenum)
				     (slot-value ancestor 'wf::offset))
		     do (setf (gethash key *core-wordnet-ancestors*) t)))
	  )
    )
  (gethash (cons (slot-value ss 'wf::lex-filenum)
		 (slot-value ss 'wf::offset))
	   *core-wordnet-ancestors*))

(defun maybe-wordnetify-sense-xml (d xml)
  "If the sense was looked up from WordNet instead of our lexicon, return a new
   version of the sense XML with the WordNet sense that was used to map it to
   the ONT type from the original sense XML as the new class, and any
   intermediate synsets as extra ancestors. Also include the gloss from WN."
  (with-slots (ont-type synset sense-key wn-path) d
    (if (null synset)
      ;; in trips
      `(class :source "trips" ,@(cdr xml))
      ;; else in wordnet
      (let* ((old-ancestors (util:find-arg-in-act xml :ancestors))
             (wn-anc-ids
	       (reverse (mapcar
		 (lambda (anc-ss)
		   (sense-key-to-id (first
		     (wf::sense-keys-for-synset anc-ss))))
		 wn-path)))
	     (wn-class-id (pop wn-anc-ids))
	     (new-words (format nil "~{~a~^,~}" (wf::wn-lemmas synset)))
             (new-ancestors
	       (format nil "~{~a,~}~(~a~),~a"
		   wn-anc-ids ont-type old-ancestors))
	     (gloss
		;; I'm pretty sure WN 3.0 doesn't have multiple glosses
		;; separated by | like this, but that's how WF reads them, so
		;; just in case, I'll put the | back here
		(format nil "~{~a~^ | ~}"
			 (slot-value synset 'wf::glosses)))
	     )
	`(class :source ,(if (synset-core-p synset) "core-wn" "wn")
	        :onttype ,wn-class-id
		:words ,new-words
		:ancestors ,new-ancestors
		:gloss ,gloss
		,@(nthcdr 7 xml)
		)))))

(defvar *prefixes* nil)

(defun remove-prefix (q)
  "Given a query string, return a list of possible query strings with defined
   prefixes removed, sorted from longest to shortest prefix."
  (unless *prefixes* ; lazily set *prefixes* so we know the lexicon is loaded
    (setf *prefixes* (sort
	  (loop for sym in (lxm::lexicon-db-base-forms lxm::*lexicon-data*)
		;; when the base form is a symbol ending with -
		for str = (when (symbolp sym) (symbol-name sym))
		for last-index = (when str (- (length str) 1))
		when (and str (char= #\- (char str last-index)))
		;; collect the lowercase string without the -
		collect (string-downcase (subseq str 0 last-index))
		)
	  #'> :key #'length)))
  (loop for prefix in *prefixes*
        when (and (< (length prefix) (length q))
	             (string-equal prefix (subseq q 0 (length prefix))))
	collect (subseq q (length prefix))
	)
  )

(defvar *show-wn-senses-only-for-core-synsets* nil)

;; Test cases:
	; problems
;; "truck" - in TRIPS and WN as noun and verb
;; "panther" - in WN and not TRIPS
;; "panthers" - needs morphy
	; frame becomes nil? seems a bug in get-word-def, maybe
;; "thing" - is in TRIPS, but is ONT::referential-sem (is also in WN, but maps the same, so we don't see it even with the checkbox checked)
;; "entity" - is in WN and not TRIPS, and legit. maps to ONT::referential-sem
;; "take" - is in WN and TRIPS, lots of senses
;; "taken" - irregular form
;; "greeting" - regular non-base form, in WN and TRIPS, >2 verb senses of "greet" in WN
;; "look up" - particle verb in TRIPS (also in WN, but maps the same)
;; "take care" - non-particle multiword verb in TRIPS, also 3 senses in WN but two of them to the same ont type as the TRIPS sense, so we only get the other one
;; "hard-boil" - in TRIPS (with w::punc-minus) but not WN (with - or _)
;; "still-fish" - in WN but not TRIPS, only with - and not _
;; "bulkier" - satellite adj in WN (with morphy) but not TRIPS
;; "antinutrient" - in neither WN nor TRIPS, but we could separate "anti-" prefix and get "nutrient" from WN and TRIPS
	; we don't have w::anti- defined
;; "homoiconic" - TRIPS has homo- and icon, WN has icon and iconic
(defun lex-xml (q &optional (q-orig q))
  (unless q (setf q "word"))
  (let* ((q-word (make-word-from-string q))
	 (word-defs
	   (mapcar (lambda (dl) (make-word-def-from-list q dl))
		   (let ((*package* (find-package :webparser)))
		     (send-and-wait `(request :receiver lxm :content
			 (get-word-def ,(word-gwd q-word) nil))))))
	 (lemmas
	   (remove-duplicates
	       (mapcar #'word-def-lemma word-defs)
	       :key #'word-str :test #'equalp))
	 )
    (if (and (= 1 (length word-defs)) ; only one definition
             ;; and it's ONT::referential-sem
             (eq 'ont::referential-sem (word-def-ont-type (car word-defs)))
	     ;; and ONT::referential-sem doesn't actually have this word/pos
	     (not (member (list (word-gwd (word-def-lemma (car word-defs)))
	                        (word-def-pos (car word-defs)))
	                  (lxm::get-words-from-lf 'ont::referential-sem)
			  :test #'equalp))
	     ;; and we didn't get it from WordNet
	     (null (word-def-synset (car word-defs)))
	     )
      ;; q not found; try recursing on versions of it with prefixes removed
      (loop for q-root in (remove-prefix q)
            for response = (lex-xml q-root q-orig)
	    unless (= 404 (second response))
	    return response
	    finally (return
	      `(http 404 ;; not found
		:content-type "text/xml; charset=utf-8"
		:content ,(with-output-to-string (s)
		  (format-xml-header s :xsl "../style/word.xsl"
				       :doctype "WORDS" :dtd "../word.dtd")
		  (format-xml s
		    `("WORDS" :q ,q-orig
			      :modified ,(get-word-modified-date q)
			      ,@(when lxm::*use-trips-and-wf-senses*
				  '(:use-trips-and-wf-senses "T"))
			      ,@(when *show-wn-senses-only-for-core-synsets*
				  '(:show-wn-senses-only-for-core-synsets "T"))
			      ("WORD" :name "not found")))
		  ))
	    ))
      `(http 200 ;; else q found
	:content-type "text/xml; charset=utf-8"
	:content ,(with-output-to-string (s)
	  (format-xml-header s :xsl "../style/word.xsl"
			       :doctype "WORDS" :dtd "../word.dtd")
	  (format-xml-start s
	    `("WORDS" :q ,q-orig
		      :modified ,(get-word-modified-date q)
		      ,@(when lxm::*use-trips-and-wf-senses*
			  '(:use-trips-and-wf-senses "T"))
		      ,@(when *show-wn-senses-only-for-core-synsets*
		          '(:show-wn-senses-only-for-core-synsets "T"))
		      ""))
	  (format s "~%")
	  (dolist (lemma lemmas)
	    (format s "~a"
	      (weblex::convert-lisp-to-xml
		(weblex::sort-pos-and-classes
		  `(weblex::word :name ,(word-str_ lemma)
		    ,@(loop with lemma-defs =
		              (remove-if-not
			        (lambda (d)
				  (eq (word-sym_ lemma)
				      (word-sym_ (word-def-lemma d))))
				word-defs)
			    with nom-morphs =
			      (mapcar
			        (lambda (no)
				  `(weblex::morph :cat "nom" :from ,no))
				(remove-duplicates
				    (mapcan
				      (lambda (d)
					(when (word-def-nom-of d)
					  (list (word-str_ (word-def-nom-of d))
					  )))
				      lemma-defs)
				    :test #'string=))
			    for d in lemma-defs
			    collect
			      (with-slots (pos ont-type template) d
				`((weblex::pos :name ,pos
				   ,(maybe-wordnetify-sense-xml d
				      (weblex::sense-to-xml
					(or ;; try to get the original source
					    ;; code for this sense, in order to
					    ;; get examples and template args
					    (weblex::get-sense-source
						(word-dw lemma)
						pos ont-type template)
					    ;; failing that, make our own
					    `((weblex::lf-parent ,ont-type)
					      (weblex::templ ,template)))
					nil)))))
			    into xmls
			    finally (return 
			      (mapcar
				(lambda (pos-xml)
				  ;; add morph
				  (let ((pos (third pos-xml)))
				    `(weblex::pos :name ,pos
				      ,@(weblex::get-morphs-xmls
				          (word-gwd lemma) pos)
				      ,@(when (eq pos 'w::n) nom-morphs)
				      ,@(cdddr pos-xml))))
				(reduce #'weblex::merge-pos-xml-lists xmls
					:initial-value nil)))
			    ))))))
	  (format-xml-end s "WORDS")
	  )
	)
      )
    ))

(defun ont-autocomplete (q limit)
  (let (results)
    ;; get matches in our ontology first
    (loop for ont-type being the hash-keys of
	    (om::ling-ontology-lf-table om::*lf-ontology*)
	  while (or (not limit) (< (length results) limit))
	  when (search q (symbol-name ont-type) :test #'string-equal)
	  do (push ont-type results))
    ;; then if there aren't enough already, try getting WN synsets
    (loop for pos being the hash-keys of (slot-value wf::wm 'wf::indices)
            using (hash-value index)
	  while (or (not limit) (< (length results) limit))
	  do
      (loop for lemma being the hash-keys of index using (hash-value entry) do
        (loop for sso in (slot-value entry 'wf::synset-offsets)
	      for ss = (wf::get-synset wf::wm pos sso) ; cached
	      for sk = (first (wf::sense-keys-for-synset ss))
	      for id = (sense-key-to-id sk)
	      while (or (not limit) (< (length results) limit))
	      when (and (or (search q id :test #'string-equal)
			    (search q sk :test #'string-equal))
			(car (synset-to-ont-types ss)))
	      do (push id results))))
    `(http 200 :content-type "text/plain; charset=utf-8" :content
       ,(format nil "~(~{~a~%~}~)" (nreverse results)))))

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
      (style :type "text/css" ".wn {} ul { padding-left: 1em; }")
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
// work around xml escaping of this:
//$('#search-input').autocomplete('lex-ont?side=ont&ret=autocomplete');
var ont_ac_url = new URL('lex-ont', document.baseURI);
ont_ac_url.searchParams.set('side', 'ont');
ont_ac_url.searchParams.set('ret', 'autocomplete');
$('#search-input').autocomplete(ont_ac_url.href);
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
      (label
        (input :type "checkbox" :onchange "toggleCoreWN(this.checked);")
	" Show only core WN synsets and their ancestors")
      (ul :style "list-style: none; padding-left: 0em"
	(li :id "root"))
      ,@footer
      )
    ))

(defun ont-xml (q)
  (let ((sk (id-to-sense-key q))
	(lf-table (om::ling-ontology-lf-table om::*lf-ontology*)))
    (cond
      (sk
        (let* ((ss (wf::get-synset-from-sense-key wf::wm sk))
	       (anc-ont-type (first (synset-to-ont-types ss)))
	       (anc-desc (when anc-ont-type (gethash anc-ont-type lf-table)))
	       (anc-sem (when anc-desc (slot-value anc-desc 'om::sem)))
	       (anc-args (when anc-desc (slot-value anc-desc 'om::arguments))))
	  (if ss
	    `(http 200
	      :content-type "text/xml; charset=utf-8"
	      :content ,(with-output-to-string (s)
	        (format-xml-header s :xsl "../style/onttype.xsl"
				     :doctype "ONTTYPE" :dtd "../onttype.dtd")
		;; NOTE: we don't use "(format-xml s" here because we want
		;; upper case tag names
		(format s "~a" (weblex::convert-lisp-to-xml
		  `(ONTTYPE :name ,q
		     :source
		       ,(cond
		          ((synset-core-p ss) "core-wn")
			  ((synset-core-ancestor-p ss) "core-wn-ancestor")
			  (t "wn")
			  )
		     ;; sem
		     ,@(when anc-sem
			 (list (weblex::feature-list-xml anc-sem)))
		     ;; arguments
		     ,@(when anc-args
		         (mapcar #'weblex::sem-argument-xml anc-args))
		     ;; words (except stoplisted)
		     ,@(mapcan
		         (lambda (lemma)
			   (unless (wf::stoplist-p
				       (first (wf::sense-key-for-word-and-synset
						  lemma ss)))
			     `((WORD :name ,lemma))))
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
				 `((CHILD :name ,child-id)))))
			   (wf::get-pointers-by-relationship wf::wm ss
			     (if (eq 'wf::|a| (wf::get-ss-type ss))
			       "&" ; similar to (satellite of head adj)
			       "~" ; hyponym
			       ))
			   )))))
	        ))
	    `(http 404 :content-type "text/plain" :content "not found"))))
      (q
	(let* ((ont-type (intern (string-upcase q) :ONT))
	       (name (string-downcase q))
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
	      ;; add :source "trips"
	      (rplacd (cddr xml) (cons :source (cons "trips" (cdddr xml))))
	      `(http 200
		:content-type "text/xml; charset=utf-8"
		:content ,(with-output-to-string (s)
		  (format-xml-header s :xsl "../style/onttype.xsl"
				       :doctype "ONTTYPE" :dtd "../onttype.dtd")
		  (format s "~a" (weblex::convert-lisp-to-xml xml))
		  )
		))
	    '(http 404 :content-type "text/plain" :content "not found"))))
      (t
        '(http 404 :content-type "text/plain" :content "not found"))
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
  document.getElementById(highlighted + '-link').click();
});
" ancestors highlighted))))))))))

(defvar *lex-ont-frameset* '(http 200
  :content-type "text/html; charset=utf-8"
  :content "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Frameset//EN\"
   \"http://www.w3.org/TR/html4/frameset.dtd\">
<HTML><HEAD><TITLE>Browse TRIPS+WN Lexicon and Ontology</TITLE></HEAD>
<FRAMESET cols=\"50%, 50%\">
 <FRAMESET rows=\"50%, 50%\">
  <FRAME src=\"lex-ont?side=lex\" name=\"lexicon\">
  <FRAME src=\"lex-ont?side=ont&ret=xml&q=root\" name=\"ont-type-details\">
 </FRAMESET>
 <FRAME src=\"lex-ont?side=ont\" name=\"ontology\">
</FRAMESET>
</HTML>
"))

(defun handle-lex-ont (msg query)
  (destructuring-bind (&key side ret q limit
  		       use-trips-and-wf-senses
		       show-wn-senses-only-for-core-synsets
		       &allow-other-keys) query
    (when (and limit (every #'digit-char-p limit))
      (setf limit (parse-integer limit)))
    (reply-to-msg msg 'tell :content
      (let ((*package* wf::*wf-package-var*)) ; nip WF pkg problems in the bud
	(cond
	  ((equalp side "lex")
	    (cond
	      ((equalp ret "autocomplete") (lex-autocomplete q limit))
	      (t
		;; temporarily set utawfs and swnsofcs to requested values,
		;; saving old values; also temporarily set wfsl really high
	        (let ((old-utawfs lxm::*use-trips-and-wf-senses*)
		      (old-swnsofcs *show-wn-senses-only-for-core-synsets*)
		      (old-wfsl lxm::*wf-sense-limit*))
		  (setf lxm::*use-trips-and-wf-senses* 
			  (when use-trips-and-wf-senses t)
			*show-wn-senses-only-for-core-synsets*
			  (when show-wn-senses-only-for-core-synsets t)
			lxm::*wf-sense-limit* 1000000)
		  (unwind-protect (lex-xml q) ;; do the word lookup
		    ;; reset utawfs, swnsofcs, and wfsl to old values
		    (setf lxm::*use-trips-and-wf-senses* old-utawfs
			  *show-wn-senses-only-for-core-synsets* old-swnsofcs
		          lxm::*wf-sense-limit* old-wfsl))))
	      ))
	  ((equalp side "ont")
	    (cond
	      ((equalp ret "autocomplete") (ont-autocomplete q limit))
	      ((equalp ret "types-for-roles") (ont-types-for-roles q))
	      ((equalp ret "xml") (ont-xml q))
	      (t (ont-html q))
	      ))
	  (t *lex-ont-frameset*)
	  )))))

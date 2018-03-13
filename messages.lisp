(in-package :webparser)
(in-component :webparser)

(defun word-def-value-to-xml (value)
  (cond
     ((and (symbolp value) (char= #\? (elt (symbol-name value) 0)))
       `(var :name ,(format nil "~(~a~)" value)))
     ((consp value)
       (case (intern (symbol-name (car value)) :keyword)
	 (:% ; constit
	   `(constit :pos ,(second value)
	     ,@(mapcar #'word-def-feature-to-xml (cddr value))))
	 (:? ; variable
	   `(var :name ,(format nil "?~(~a~)" (second value))
	     ,@(mapcar (lambda (o)
			 `(option ,(word-def-value-to-xml o)))
		       (cddr value))))
	 (:$ ; sem feature list
	   `(features
	      (feature-list-type ,(word-def-value-to-xml (second value)))
	      ,@(mapcar #'word-def-feature-to-xml (cddr value))
	      ))
	 (:* ; ont-type word pair
	   `(sense :ont-type ,(second value) :word ,(third value)))
	 (otherwise
	   (format nil "~s" value))
	 ))
     (t
       (format nil "~s" value))
     ))

(defun word-def-feature-to-xml (feature)
  (destructuring-bind (name value) feature
    `(feat :name ,(format nil "~(~a~)" name)
      ,(word-def-value-to-xml value))))

(defun handle-get-word-def (msg query)
  (let ((w (find-arg query :w))
	(xml-header
"<?xml version=\"1.0\"?>
<?xml-stylesheet type=\"text/xsl\" href=\"../style/word-def.xsl\"?>
")
	(xml '(word-def)))
    (when (stringp w)
      (loop 
      	    initially (push :word xml) (push w xml)
	    with gwd-w =
	      (mapcar (lambda (str)
	                (intern (string-upcase str) :w))
	              (util:split-string w))
            for def in (send-and-wait
              `(request :receiver lexiconmanager :content
	        (get-word-def ,gwd-w nil)))
            for (id words pref constit) = def
            do
	      (push
		`(lex-entry
		  :id ,(format nil "~(~a~)" id)
		  :words ,(format nil "~(~{~a~^ ~}~)" words)
		  :pref ,(format nil "~a" pref)
		  :pos ,(format nil "~a" (second constit))
		  (lisp ,(format nil "~s" def))
		  ,@(mapcar #'word-def-feature-to-xml (cddr constit)))
		xml)
	    ))
    (reply-to-msg msg 'tell :content `(http 200
      :content-type "text/xml; charset=utf-8"
      :content ,(concatenate 'string
      		 xml-header
		 (with-output-to-string (s)
		   (format-xml s (reverse xml))))
      ))))

(defun name-char-p (c)
  "Is c a character safe to use in a filename or lisp symbol?"
  ; /[\w-]/
  (or (alpha-char-p c)
      (digit-char-p c)
      (member c '(#\_ #\-) :test #'char=)
      ))

(defun name-p (str)
  "Is the string str safe to use as part of a filename or lisp symbol?"
  (and (< 0 (length str))
       (not (digit-char-p (char str 0)))
       (every #'name-char-p str)
       ))

(defun parse-comma-separated-list (str)
  "Remove whitespace, upcase, split on commas, and intern."
  (mapcar #'intern
          (remove ""
	      (util::split-string
		  (string-upcase (remove-if #'whitespace-char-p str))
		  :on '(#\,))
	      :test #'string=)))

(defun merge-keyword-args (old new)
  "Given two keyword argument lists old and new, return a new keyword argument
   list that has all the values from new, and all the values from old for keys
   that don't also appear in new."
  (let ((new-keys (loop for l = new then (cddr l) while l collect (car l))))
    (append (util:remove-args old new-keys) new)))

(defun handle-http-request (msg args)
  (destructuring-bind (request-method request-uri &key query) args
      (declare (ignore request-method))
    (let* ((slash-pos (position #\/ request-uri :from-end t))
           (uri-basename (if slash-pos (subseq request-uri (1+ slash-pos)) request-uri))
	   (make-paragraph-p (or (member trips::*trips-system* '(:drum :step))
				 (string= uri-basename "cwmsreader")))
	   (do-inference (member uri-basename '("drum-er" "cwmsreader") :test #'string=)))
      (when (string= uri-basename "get-word-def")
        (return-from handle-http-request (handle-get-word-def msg query)))
;      (unless (member uri-basename '("parse" "drum") :test #'string=)
;	(error "bogus request uri: ~s" request-uri))
      (destructuring-bind ( &key input
      			    (component "parser")
			    (extscontents (if (string= uri-basename "cwmsreader") "inf" "raw"))
			    (extsformat "svg")
			    tagsformat
			    (treecontents "phrase")
			    (treeformat "LinGO")
			    (lfformat "svg")
			    debug
			    tag-type
			    input-terms
			    no-sense-words
			    senses-only-for-penn-poss
			    (split-mode "split-clauses")
			    semantic-skeleton-scoring
			    trace-level
			    rule-set
			    &allow-other-keys
			  ) query
	(let* ((slots `(
		 :text ,input
		 :service ,(intern (string-upcase uri-basename) :keyword)
		 :component ,(if (string-equal "texttagger" component)
			       'texttagger 'parser)
		 :interface-options (
		   :stylesheet
		     ,(if (eq :drum trips::*trips-system*)
			"drum-interface.xsl" "parser-interface.xsl")
		   ,@(when do-inference
		     `(:extscontents ,extscontents))
		   :extsformat ,extsformat
		   :tagsformat
		     ,(cond
		       (tagsformat tagsformat)
		       ((string-equal "texttagger" component) "table")
		       (t "hidden")
		       )
		   :treecontents ,treecontents
		   :treeformat ,treeformat
		   :lfformat ,lfformat
		   ,@(when debug '(:debug "on"))
		 )
		 :texttagger-options
		   (
		     ,@(when (and tag-type (not (string= "" tag-type)))
		       (list :tag-type (read-safely-from-string tag-type)))
		     ,@(when (and input-terms (not (string= "" input-terms)))
		       (list :input-terms (read-safely-from-string input-terms)))
		     ,@(when no-sense-words
		       (let ((nsw (parse-comma-separated-list
				      no-sense-words)))
			 (when nsw (list :no-sense-words nsw))))
		     ,@(when senses-only-for-penn-poss
		       (let ((sofpp (parse-comma-separated-list
					senses-only-for-penn-poss)))
			 (when sofpp
			   (list :senses-only-for-penn-poss sofpp))))
		   )
		 ,@(when (and make-paragraph-p
			      (member split-mode
				      '("split-clauses" "split-sentences")
				      :test #'string-equal))
		   (list :split-mode
			 (intern (string-upcase split-mode) :keyword)))
		 :parser-options
		   ,(when (eq :step trips::*trips-system*)
		     `((parser::*semantic-skeleton-scoring-enabled*
			  ,(cond
			    ;; when just loading the initial page, use the
			    ;; default setting
			    ((null input)
			      (init-original-parser-options)
			      (second (assoc 'parser::*semantic-skeleton-scoring-enabled* *original-parser-options*)))
			    ;; otherwise, obey the checkbox
			    (semantic-skeleton-scoring t)
			    (t nil)
			    ))))
		 :extraction-options
		   (
		     ,@(when (and trace-level
				  (< 0 (length trace-level))
				  (every #'digit-char-p trace-level))
		       `(:trace-level ,(read-from-string trace-level)))
		     ,@(when (and rule-set
		     		  (name-p rule-set)
				  (probe-file
				    (format nil *rule-set-path-format* rule-set)
				    ))
		       `(:rule-set ,rule-set))
		     ,@(when do-inference
		       '(:do-inference t))
		   )
		 :requester ,(find-arg-in-act msg :sender)
		 :reply-id ,(find-arg-in-act msg :reply-with)
		 )))
	  (receive-text-from-user
	      (apply (if make-paragraph-p #'make-paragraph #'make-utterance)
		     slots))
	  )))))

;;; from cgi-kqml-bridge.pl

(defcomponent-handler
  '(request &key :content (http . *))
  #'handle-http-request
  :subscribe t)

;;; from IM

(defcomponent-handler
  '(tell &key :content (sentence-lfs . *))
  #'handle-sentence-lfs
  :subscribe t)

(defcomponent-handler
  '(tell &key :content (paragraph-done . *))
  #'handle-paragraph-completed
  :subscribe t)

(defcomponent-handler
  '(tell &key :content (end-of-turn . *))
  #'handle-turn-done
  :subscribe t)

(defcomponent-handler
  '(tell &key :content (interpretation-failed . *))
  #'handle-turn-done
  :subscribe t)

;;; from Parser

(defcomponent-handler
  '(tell &key :content (new-speech-act-hyps . *))
  (lambda (msg args) (handle-new-speech-act msg (first args)))
  :subscribe t)

(defcomponent-handler
  '(tell &key :content (new-speech-act . *))
  #'handle-new-speech-act ; see utterance.lisp
  :subscribe t)

(defcomponent-handler
  '(tell &key :content (failed-to-parse . *))
  ;; make a fake sa from the failed-to-parse message
  (lambda (msg args) (handle-new-speech-act msg `((failed-to-parse ,@args))))
  :subscribe t)

(defcomponent-handler
  '(tell &key :content (paragraph-completed . *))
  #'handle-paragraph-completed
  :subscribe t)

;;; from TextTagger

(defcomponent-handler
  '(tell &key :content (word . *))
  #'handle-texttagger-output
  :subscribe t)

(defcomponent-handler
  '(tell &key :content (prefix . *))
  #'handle-texttagger-output
  :subscribe t)

(defcomponent-handler
  '(tell &key :content (prefer . *))
  #'handle-texttagger-output
  :subscribe t)

(defcomponent-handler
  '(tell &key :sender texttagger :content (started-speaking . *))
  #'handle-started-speaking-from-texttagger
  :subscribe t)

(defcomponent-handler
  '(error &key :comment *)
  #'handle-error-from-texttagger
  :subscribe nil)

; TODO ?
;(defcomponent-handler
;  '(tell &key :sender texttagger :content (utterance . *))
;  #'handle-utterance-from-texttagger
;  :subscribe t)

;;; From WebParser (that's us!)

(defcomponent-handler
  '(request &key :content (retry-finishing . *))
  #'handle-retry-finishing
  :subscribe nil)


(in-package :webparser)
(in-component :webparser)

(defstruct text-unit
  text ; input text
  service ; name of the web service (e.g. :DRUM, :DRUM-ER, :CWMSREADER...)
  component ; parser or texttagger
  interface-options ; extscontents, extsformat, tagsformat, treecontents, treeformat, lfformat, debug
  texttagger-options ; see ../TextTagger/docs/README.xhtml
  parser-options ; see ../Parser/...
  extraction-options ; see ../NewIM/...
  requester ; :sender of the request to parse this utterance, :receiver of the ultimate response
  reply-id ; :reply-with/:in-reply-to
  debug-output-stream ; string-output-stream replacing *standard-output* while parsing
  texttagger-output ; word and prefer messages from TT
  parser-output
  )

(defstruct (utterance (:include text-unit))
  num ; utterance serial number
  )

(defstruct (paragraph (:include text-unit))
  (split-mode :split-clauses)
  uttnums
  ;; IM output
  extractions
  sentence-lfs
  )

(defun ensure-original-tt-parameters ()
  "Make sure we know the default TT options (i.e. that *original-tt-parameters*
   is filled)."
  (unless *original-tt-parameters*
    (setf *original-tt-parameters*
      (remove-arg
	  (cdr (send-and-wait
	      '(request :receiver TextTagger :content (get-parameters))))
	  ;; get-parameters returns this, but it can't be set using
	  ;; set-parameters, only init/fini
          :init-taggers))
    (let ((original-input-terms (send-and-wait
	    '(request :receiver TextTagger :content (get-input-terms)))))
      (when original-input-terms
	(setf *original-tt-parameters* (nconc *original-tt-parameters*
	      (list :input-terms original-input-terms)))))
    ))

(defun set-texttagger-options (opts)
  (multiple-value-bind (params input-terms)
      (remove-arg opts :input-terms)
    (setf params (substitute :default-type :tag-type params))
    (setf input-terms (car input-terms)) ; we know there's at most one :input-terms arg
    (unless (equalp '(ok)
		(send-and-wait `(request :receiver TextTagger :content
		    (set-parameters ,@params))))
      ;; FIXME should these be errors?
      (format t "failed to set TextTagger options ~s~%" opts))
    (unless (equalp '(ok)
		(send-and-wait `(request :receiver TextTagger :content
		    ,(if input-terms
		       `(load-input-terms :input-terms ,input-terms)
		       '(clear-input-terms)))))
      (format t "failed to set TextTagger input-terms ~s~%" input-terms))
    ))

(defun reset-input-terms ()
  (let ((input-terms (find-arg *original-tt-parameters* :input-terms)))
    (unless (equalp '(ok)
		(send-and-wait `(request :receiver TextTagger :content
		    ,(if input-terms
		       `(load-input-terms :input-terms ,input-terms)
		       '(clear-input-terms)))))
      (format t "failed to reset TextTagger input-terms ~s~%" input-terms))))

;; we can't just set this in defsys because that's loaded before system.lisp
;; sets the parser options; instead we wait until we get the first request, and
;; then get the original settings before applying the settings from the request
(defun init-original-parser-options ()
  (unless *original-parser-options*
    (setf *original-parser-options* `(
        (parser::*semantic-skeleton-scoring-enabled*
	  ,parser::*semantic-skeleton-scoring-enabled*)
        ))))

(defun reset-parser-options ()
  (set-parser-options *original-parser-options*))

;; see also parser::initialize-settings in src/Parser/Trips-parser/messages.lisp
(defun set-parser-options (opts)
  (init-original-parser-options)
  (loop	for (k v) in opts
	do (eval `(setf ,k ',v))
	))

(defun reset-extraction-options ()
  (let ((peo *previous-extraction-options*))
    (setf *previous-extraction-options* nil)
    (set-extraction-options peo)
    (setf *previous-extraction-options* nil) ; don't back up
    ))

;; this bogosity to handle the absence of the IM package in the URCS web parser
(let ((im-pkg (find-package :im)))
  (if im-pkg
    (eval (read-from-string "

(defun set-extraction-options (opts)
  (when *previous-extraction-options* ;; automatically reset
    (reset-extraction-options))
  (setf *previous-extraction-options* `(
    :trace-level ,im::*trace-level*
    :rule-set nil
    :extraction-sequence ,im::*extraction-sequence*
    ))
  (setf im::*trace-level* (find-arg opts :trace-level))
  (let ((rule-set (find-arg opts :rule-set))
        (extraction-sequence (find-arg opts :extraction-sequence)))
    (cond
      (rule-set
        ;; NOTE: must re-load for every request, since the file may have changed
	(load (format nil *rule-set-path-format* rule-set))
	;; e.g. rule-set=\"foo\" => im::*es*='((im::fooruleset))
	(setf im::*extraction-sequence*
	      `((,(intern (format nil \"~:@(~a~)RULESET\" rule-set) :im))))
        )
      (extraction-sequence
        (setf im::*extraction-sequence* extraction-sequence))
      ))
  )

      "))
    ;; if no im-pkg
    (defun set-extraction-options (opts) (declare (ignore opts)) nil)
    ))

(defun send-utterance-to-system (utt)
  (setf (utterance-num utt) *last-uttnum*)
  ;; reset stuff throughout the system, including IM's utt record, which only
  ;; has room for 10,000 utterances
  (send-msg '(broadcast :content (tell :content (start-conversation))))
  (ensure-original-tt-parameters)
  ;; set TT options if they were given in the request
  (when (utterance-texttagger-options utt)
    (set-texttagger-options (utterance-texttagger-options utt)))
  ;; ditto parser/extraction options
  (set-parser-options (utterance-parser-options utt))
  (set-extraction-options (utterance-extraction-options utt))
  ;; make sure we get an end-of-turn message from IM
  ;; FIXME should we save the old value and restore it on end-of-turn?
  (eval (read-from-string "(setf im::*CPS-control* nil)"))

  ;; send the utterance itself off to be processed
  (send-msg `(tell :content (utterance :text ,(utterance-text utt) :uttnum ,(utterance-num utt) :direction input)))
  ;; set TT options back to the default if we changed them
  (when (utterance-texttagger-options utt)
    (set-texttagger-options *original-tt-parameters*))
  ;; parser/extraction options have to wait until the next text-unit is started
  )

;; without DrumGUI, just sending directly to TextTagger
(defun send-paragraph-to-system (para)
  (send-msg '(broadcast :content (tell :content (start-conversation))))
  (set-parser-options (paragraph-parser-options para))
  (set-extraction-options (paragraph-extraction-options para))
  (let ((tt-reply
	  (send-and-wait `(request :receiver texttagger :content
	      (tag :text ,(paragraph-text para)
		   :imitate-keyboard-manager t
		   :next-uttnum ,*last-uttnum*
		   ,(paragraph-split-mode para) t
		   ,@(substitute :type :tag-type
		       (paragraph-texttagger-options para))
		   )))))
    (unless (eq 'ok (car tt-reply))
      (error "Bad reply from TextTagger: ~s" tt-reply))
    (setf (paragraph-uttnums para) (find-arg-in-act tt-reply :uttnums))
    (setf *last-uttnum* (car (last (paragraph-uttnums para))))
    )
  )

(defun slurp-file (filename)
  "Read the entire contents of a file into a string and return it."
  (with-open-file (f filename :direction :input)
    ;; NOTE: file-length returns the length in *bytes*, but string lengths and
    ;; reads are in terms of *characters* which may be more than one byte. So
    ;; we allocate a string with enough characters that all will fit even if
    ;; all characters are bytes, but then we use the (character) position
    ;; returned by read-sequence to take only the substring that we actually
    ;; read into. Another solution to this problem would be to explicitly make
    ;; a byte sequence and read into that, but then code that expects strings
    ;; elsewhere would have to change, and we'd lose some checks for invalid
    ;; characters.
    (let ((ret (make-string (file-length f))))
      (subseq ret 0 (read-sequence ret f)))))

(defun remove-processing-instructions (xml-string)
  "Remove processing instructions (in <?...?>, including the xml declaration)
   from the beginning of an XML string, so that it may be included as part of a
   larger XML file."
  ;; iterate over PIs and whitespace until we get something that's neither
  (loop with start = 0
        while (string= "<?" (subseq xml-string start (+ 2 start)))
	do (incf start 2)
	   (setf start (search "?>" xml-string :start2 start))
	   (unless start (error "unterminated processing instruction"))
	   (incf start 2)
	   (setf start
		 (position-if-not #'whitespace-char-p xml-string :start start))
	   (unless start (error "nothing following processing instructions"))
	finally (return (subseq xml-string start))
	))

;; with DrumGUI
(defun send-paragraph-to-drum-system (para)
  (ensure-original-tt-parameters)
  (unwind-protect ; reset TT options even if we get an error
    (progn ; protected form
      ;; set TT options if they were given in the request
      (when (paragraph-texttagger-options para)
	(set-texttagger-options (paragraph-texttagger-options para)))
      ;; this one goes through DrumGUI because it needs to be in the tag request
      (unless (eq :split-clauses (paragraph-split-mode para))
        (send-msg '(request :receiver reader :content
	    (set-tag-options :split-clauses nil :split-sentences t))))
      ;; ditto parser/extraction options
      (set-parser-options (paragraph-parser-options para))
      (set-extraction-options (paragraph-extraction-options para))
      (let* ((do-inference (find-arg (paragraph-extraction-options para) :do-inference))
             (dg-reply
	       (send-and-wait `(request :receiver reader :content
		   (run-text :text ,(paragraph-text para)
			     :reply-with-ekb nil
			     ,@(when do-inference
				 '(:do-inference true)))))))
	(unless (eq 'result (car dg-reply))
	  (error "Bad reply from DrumGUI: ~s" dg-reply))
	(setf (paragraph-uttnums para) (find-arg-in-act dg-reply :uttnums))
	(let ((ekb-file (find-arg-in-act dg-reply :ekb-file)))
	  (setf (paragraph-extractions para)
		(if (stringp ekb-file)
		  (remove-processing-instructions (slurp-file ekb-file))
		  "")))
	(let ((inf-ekb-file (find-arg-in-act dg-reply :inferred-ekb-file)))
	  (when (stringp inf-ekb-file)
	    (setf (paragraph-extractions para)
		  (concatenate 'string (paragraph-extractions para)
		      (remove-processing-instructions (slurp-file inf-ekb-file))
		      ))))
	(setf *last-uttnum* (car (last (paragraph-uttnums para))))
	)
      )
    ;; cleanup forms
    ;; set Parser/TT options back to the default if we changed them
    ;; (extraction options are reset automatically before the next set)
    (when (paragraph-parser-options para)
      (reset-parser-options))
    (when (paragraph-texttagger-options para)
      (set-texttagger-options *original-tt-parameters*))
    (unless (eq :split-clauses (paragraph-split-mode para))
      (send-msg '(request :receiver reader :content
	  ;; FIXME DrumGUI doesn't let us ask it what the tag options were to
	  ;; begin with, so we have to just assume the default tag options (at
	  ;; time of writing these were the defaults in the only systems that
	  ;; use DrumGUI, drum and cwmsreader)
          (set-tag-options :split-clauses t :split-sentences t))))
    )
  )

(defun tag-text-using-texttagger (text)
  (send-msg '(broadcast :content (tell :content (start-conversation))))
  (let ((tt-reply
	  (send-and-wait `(request :receiver texttagger :content
	      (tag :text ,(text-unit-text text)
		   ,@(when (paragraph-p text)
		     `(
		       :next-uttnum ,*last-uttnum*
		       ,(paragraph-split-mode text) t
		       ))
		   ,@(substitute :type :tag-type
		       (text-unit-texttagger-options text))
		   )))))
    (when (member :input-terms (text-unit-texttagger-options text))
      (reset-input-terms))
    ;; reply content should be a list of lists
    (unless (and (listp tt-reply) (every #'listp tt-reply))
      (error "Bad reply from TextTagger: ~s" tt-reply))
    (setf (text-unit-texttagger-output text) tt-reply)
    (when (paragraph-p text)
      (setf (paragraph-uttnums text)
        (remove-duplicates
	  (mapcar (lambda (msg) (second (member :uttnum msg))) tt-reply)
	  :test #'=))
      (setf *last-uttnum* (car (last (paragraph-uttnums text))))
      )
    (pop *pending-text-units*)
    (finish-text-unit text)
    )
  )

(defun send-text-to-system (text)
    (declare (type text-unit text))
  (incf *last-uttnum*)
  ;; start capturing debug output
  (setf *original-standard-output* *standard-output*)
  (setf *standard-output* (make-string-output-stream))
  (setf (text-unit-debug-output-stream text) *standard-output*)
  (handler-bind
      ;; If there's an error sending the text, remove it from the pending list
      ;; and reset stdout to its original value, before indicating the error
      ;; the way defcomponent would (usually by sending a sorry message).
      ;; Then start on the next pending text, if any.
      ((error
        (lambda (err)
	  (finish-text-unit-with-error text err)
	  )))
    (ecase (text-unit-component text)
      (parser
	(etypecase text
	  (utterance (send-utterance-to-system text))
	  (paragraph
	    (if (or (eq :drum trips::*trips-system*)
		    (eq :cwmsreader (text-unit-service text)))
	      (send-paragraph-to-drum-system text)
	      (send-paragraph-to-system text)
	      ))
	  ))
      (texttagger
	(tag-text-using-texttagger text))
      )))

(defun options-to-xml-attributes (text)
    (declare (type text-unit text))
  "Return a keyword argument list suitable for passing to print-xml-header
   representing the combined options from the given text-unit structure."
  (let* (
	 (tto (text-unit-texttagger-options text))
	 (tt (util:find-arg tto :tag-type))
	 (it (util:find-arg tto :input-terms))
	 (nsw (util:find-arg tto :no-sense-words))
	 (sofpp (util:find-arg tto :senses-only-for-penn-poss))
	 (sm (when (paragraph-p text) (paragraph-split-mode text)))
	 (po (text-unit-parser-options text))
	 (sss-pair (assoc 'parser::*semantic-skeleton-scoring-enabled* po))
	 (eo (text-unit-extraction-options text))
	 (rs (find-arg eo :rule-set))
	 (tl (find-arg eo :trace-level))
	 (io (text-unit-interface-options text))
	 )
    ;; when component=texttagger; get only the relevant interface options
    (when (eq 'texttagger (text-unit-component text))
      (setf io (nth-value 1 (util:remove-args io
          '(:stylesheet :tagsformat :debug)))))
    `(
      :service ,(symbol-name (text-unit-service text))
      :component ,(text-unit-component text) ; not really an attribute, whatevs
      ,@io
      ,@(when tt
	 (list :tag-type (format nil "~(~s~)" tt)))
      ,@(when it
	 (list :input-terms (format nil "~s" it)))
      ,@(when nsw (list :no-sense-words (format nil "~(~{~a~^,~}~)" nsw)))
      ,@(when sofpp
	(list :senses-only-for-penn-poss (format nil "~{~a~^,~}" sofpp)))
      ,@(when sm
        (list :split-mode (format nil "~(~a~)" sm)))
      ,@(when sss-pair
	(list :semantic-skeleton-scoring
	      (when (second sss-pair) (format nil "~a" (second sss-pair)))))
      ,@(when rs (list :rule-set rs))
      ,@(when tl (list :trace-level (format nil "~s" tl)))
      ,@(when (eq 'parser (text-unit-component text))
        `(
	  ;; list of available rule sets
	  ;; (not really an option, but it's easiest to put this here)
	  :rule-sets
	    ,(format nil "~{~a~^,~}" ; comma separated
	      (sort
		(mapcar
		  (lambda (p)
		    (let ((name (pathname-name p)))
		      ;; chop "RuleSet" off the end of name
		      (subseq name 0 (- (length name) 7))))
		  (directory (format nil *rule-set-path-format* "*")))
		#'string<))))
      )))

(defun reply-without-parsing (text &optional error-msg)
    (declare (type text-unit text))
  (let ((s (make-string-output-stream))
	(attrs (options-to-xml-attributes text)))
    (when error-msg
      (push error-msg attrs)
      (push :error attrs))
    (print-xml-header attrs nil s)
    (format-xml-end s 
      (ecase (text-unit-component text)
        (parser "trips-parser-output")
	(texttagger "texttagger-output")))
    (send-msg
      `(tell
	:receiver ,(text-unit-requester text)
	:in-reply-to ,(text-unit-reply-id text)
	:content
	  (http 200
	    :content-type "text/xml"
	    :content ,(get-output-stream-string s)
	    )))))

(defun receive-text-from-user (text)
    (declare (type text-unit text))
  (cond
    ((null (text-unit-text text))
      ; no input: just send header
      (reply-without-parsing text))
    ((every #'whitespace-char-p (text-unit-text text))
      ; almost no input
      (reply-without-parsing text "input was only whitespace"))
    ((>= (length (text-unit-text text)) (parser::getmaxchartsize))
      ; too much input
      (reply-without-parsing text (format nil "input too long (must be fewer than ~s characters)" (parser::getmaxchartsize))))
    (t ; have good input: queue it, and start working on it unless we're busy
      (let ((busy (not (null *pending-text-units*))))
	(setf *pending-text-units* (nconc *pending-text-units* (list text)))
	(unless busy
	  (send-text-to-system text))))
    ))

(defun speech-act-uttnum (sa)
  (if (eq (car sa) 'compound-communication-act)
    (speech-act-uttnum (car (find-arg-in-act sa :acts)))
    (find-arg-in-act sa :uttnum)
    ))

(defun text-unit-finishable-p (text)
    (declare (type text-unit text))
  "Does finish-text-unit have a chance of succeeding, based on which messages
   we've received so far? (Assumes component=parser)"
  (and
    ;; we have output from both TextTagger and Parser
    (not (null (text-unit-texttagger-output text)))
    (not (null (text-unit-parser-output text)))
    ;; if we're doing a paragraph, we have the same number of TT utterances as
    ;; parser output messages
    (if (paragraph-p text)
      (= (length (paragraph-texttagger-output text))
         (length (paragraph-parser-output text)))
      t)
    ))

(defun try-to-finish-text-unit (text)
    (declare (type text-unit text))
  "Call finish-text-unit if we think we already received all the relevant
   messages; otherwise wait a bit for more messages and send a message to
   ourself to try again."
  (cond
    ((text-unit-finishable-p text)
      (finish-text-unit text))
    (t
      (push text *pending-text-units*)
      (sleep 1)
      (send-msg '(request :receiver webparser :content (retry-finishing)))
      )
    ))

(defun handle-retry-finishing (msg args)
    (declare (ignore msg args))
  (let ((text (pop *pending-text-units*)))
    (if (text-unit-finishable-p text)
      (finish-text-unit text)
      (finish-text-unit-with-error
          text
	  (make-condition 'simple-error :format-control
	    "WebParser didn't get enough output messages to finish its output.")
	  )
      )))

(defun finish-text-unit (text)
    (declare (type text-unit text))
  (reset-parser-options)
  (when (paragraph-p text)
    (handler-case (apply-sentence-lfs text) ; NOTE: does nothing w/o sentence-lfs
      (error (e) (warn "apply-sentence-lfs failed: ~a" e))))
  (let ((speech-act (text-unit-parser-output text))
        (s (make-string-output-stream)))
    (when (paragraph-p text)
      ;; undo reversing effect of pushing each utt's act into the output list
      (setf speech-act (reverse speech-act)))
    ;; stop capturing debug output
    (setf *standard-output* *original-standard-output*)
    (parse-to-xml-stream
      (options-to-xml-attributes text)
      (text-unit-text text)
      (get-output-stream-string (text-unit-debug-output-stream text))
      (when (paragraph-p text)
        (paragraph-extractions text))
      speech-act
      nil ;trees
      (if (eq 'parser (text-unit-component text))
	;; using push reverses tt output lists, so we undo that here
	(reverse (if (paragraph-p text)
		   (mapcar #'reverse (text-unit-texttagger-output text))
		   (text-unit-texttagger-output text)))
	;; component=texttagger was never reversed in the first place
	(text-unit-texttagger-output text)
	)
      s)
    (send-msg
      `(tell
	:receiver ,(text-unit-requester text)
	:in-reply-to ,(text-unit-reply-id text)
	:content
	  (http 200
	    :content-type "text/xml; charset=utf-8"
	    :content ,(get-output-stream-string s)
	    )))
    (when *pending-text-units*
      (send-text-to-system (first *pending-text-units*)))
    ))

(defun finish-text-unit-with-error (text err)
    (declare (type text-unit text))
  (pop *pending-text-units*)
  (setf *standard-output* *original-standard-output*)
  (dfc::indicate-error dfc:*component* err
      ;; fake the relevant parts of the request message
      (list :sender (text-unit-requester text)
	    :reply-with (text-unit-reply-id text)))
  (when *pending-text-units*
    (send-text-to-system (first *pending-text-units*)))
  )

(defun find-utt-by-root (speech-acts root)
  "Given one of several varieties of collections of UTTs (including a single
   UTT), find the UTT with the given :root, if any. This includes UTTs whose
   root term is an SA-SEQ with :acts that include the given root ID."
  (cond
    ((consp (car speech-acts))
      (loop for sa in speech-acts
	    for found = (find-utt-by-root sa root)
	    when found return found
	    finally (return nil)))
    ((eq 'compound-communication-act (car speech-acts))
      (find-utt-by-root (find-arg-in-act speech-acts :acts) root))
    ((eq 'utt (car speech-acts))
      (let ((utt-root-id (find-arg-in-act speech-acts :root)))
	(if (eq root utt-root-id) ; simple case first
	  speech-acts
	  ; else test sa-seq :acts case
	  (let* ((terms (find-arg-in-act speech-acts :terms))
		 (utt-root-term
		   (find-arg-in-act
		       (find utt-root-id terms
			   :key (lambda (term) (find-arg-in-act term :var)))
		       :lf)))
	    (when (member root (find-arg (cdddr utt-root-term) :acts))
	      speech-acts))
	  )))
    ((eq 'failed-to-parse (car speech-acts))
      nil)
    (t
      (error "Expected list, 'UTT, 'COMPOUND-COMMUNICATION-ACT, or 'FAILED-TO-PARSE, but got ~s" (car speech-acts)))
    ))

(defun handle-sentence-lfs (msg args)
    (declare (ignore msg))
  (let ((content (find-arg args :content))
	(text (car *pending-text-units*)))
    (unless content
      (error "sentence-lfs missing :content"))
    (unless text
      (warn "received sentence-lfs with no text-units pending~%"))
    (when (paragraph-p text)
      ;; save the message for later; if we try to copy the :corefs over to
      ;; parser-output now, we risk a race condition between Parser and IM
      ;; outputs (we're not guaranteed to get them in an order that makes sense
      ;; from IM's perspective)
      (push content (paragraph-sentence-lfs text)))))

(defun apply-sentence-lfs (text)
    (declare (type paragraph text))
  (dolist (content (paragraph-sentence-lfs text))
    (destructuring-bind (_ &key roots terms &allow-other-keys) content
        (declare (ignore _))
      (when (and terms (null roots))
	(error "Terms but no roots in sentence-lfs message"))
	     ;; get the relevant LFs from the new-speech-act(-hyps) messages
      (let* ((nsas (paragraph-parser-output text))
	     (utts (mapcar (lambda (root)
			     (or (find-utt-by-root nsas root)
				 (error "Can't find utt with root/act ~s; parser output was:~%~s" root nsas)))
			   roots))
	     (term-lfs (mapcan (lambda (utt)
				 (mapcar (lambda (term)
					   (find-arg-in-act term :lf))
					 (find-arg-in-act utt :terms)))
			       utts)))
	;; add the :coref arguments from the terms in the sentence-lfs
	;; message, destructively, to the corresponding terms we already
	;; stored
	(dolist (term terms)
	  (destructuring-bind (_ var __ &key coref &allow-other-keys) term
	      (declare (ignore _ __))
	    (when coref
	      (let ((lf (find var term-lfs :key #'second)))
		(unless lf
		  (error "Can't find term ~s to add :coref ~s to" var coref))
		(rplacd (last lf) (list :coref coref))
		))))))))

(defun handle-new-speech-act (msg args)
    (declare (ignore msg))
  (let* ((speech-act (first args)) ; get lf from message args
         (sa-uttnum (speech-act-uttnum speech-act))
	 ;; this doesn't work when processing a paragraph; instead we get trees
	 ;; from the utts now that that actually has all the trees and not just
	 ;; the first
         ;(trees (subst-package (show-trees) :parser)) ; get trees directly from parser via library call
	 (text (car *pending-text-units*)))
    (etypecase text
      (utterance
	(cond
	  ((null sa-uttnum)
	    (warn "no uttnum in new-speech-act"))
	  ((not (eql (utterance-num text) sa-uttnum))
	    (error "uttnum mismatch; expected ~s but got ~s in new-speech-act" (utterance-num text) sa-uttnum))
	  )
	(setf (utterance-parser-output text) speech-act)
	)
      (paragraph
        (push speech-act (paragraph-parser-output text))
        )
      (null
        (warn "got new-speech-act with no pending text-units"))
      )))

(defun handle-paragraph-completed (msg args)
    (declare (ignore args)) ; TODO check whether :id matches start-paragraph/end-paragraph's
  ;; need to wait for IM's paragraph-done in STEP so that we can apply
  ;; sentence-lfs to get corefs, but other paragraph systems can finish
  ;; earlier, with Parser's paragraph-completed. (DRUM doesn't count because it
  ;; goes through DrumGUI anyway, so we wait for its reply)
  (let ((verb (car (find-arg-in-act msg :content)))
        (step-p (eq :step trips::*trips-system*)))
    (when (eq verb (if step-p 'paragraph-done 'paragraph-completed))
      (let ((para (pop *pending-text-units*)))
	(etypecase para
	  (paragraph
	    (try-to-finish-text-unit para))
	  (utterance
	    (error "received unexpected paragraph-completed message when the first pending text-unit was an utterance, not a paragraph"))
	  (null
	    (warn "got paragraph-completed with no pending text-units"))
	  )))))

(defun handle-turn-done (msg args)
    (declare (ignore args))
  (let ((utt (first *pending-text-units*)))
    (etypecase utt
      (paragraph
        nil) ; ignore
      (utterance
        (pop *pending-text-units*)
	(try-to-finish-text-unit utt)
	)
      (null
        (warn "got ~s with no pending text-units"
	      (car (find-arg-in-act msg :content))))
      )))

;; FIXME? I'm not sure WebParser is guaranteed to receive TT output messages
;; before the corresponding Parser output message, but this assumes it. If it
;; happens the other way around, we might send a response without some
;; TextTagger output, pop the pending text-unit, and then get errors when this
;; function is finally called.
(defun handle-texttagger-output (msg args)
  "Save content of word, prefix, and prefer messages from TextTagger in the current utterance struct."
  (let ((uttnum (find-arg (cdr args) :uttnum))
        (text (car *pending-text-units*)))
    (unless uttnum
      (error "TT msg has no uttnum. msg=~s; args=~s" msg args))
    (etypecase text
      (null
	(warn "got TT output for uttnum ~s, but no utts are pending" uttnum))
      (utterance
	(unless (utterance-num text)
	  (error "pending utt has no uttnum"))
	(unless (eql uttnum (utterance-num text))
	  (error "TT msg uttnum ~s failed to match pending utt uttnum ~s" uttnum (utterance-num text)))
	(push (find-arg-in-act msg :content) (text-unit-texttagger-output text))
	)
      (paragraph
        (unless (paragraph-uttnums text)
	  (error "pending paragraph has no uttnums"))
	(unless (member uttnum (paragraph-uttnums text))
	  (error "TT msg uttnum ~s failed to match any of the uttnums in the pending paragraph: ~s" uttnum (paragraph-uttnums text)))
	(push (find-arg-in-act msg :content)
	      (car (text-unit-texttagger-output text)))
	)
      )
    ))

(defun handle-started-speaking-from-texttagger (msg args)
  (let ((uttnum (find-arg args :uttnum))
        (text (car *pending-text-units*)))
    (unless uttnum
      (error "started-speaking msg has no uttnum. msg=~s; args=~s" msg args))
    (etypecase text
      (null
	(warn "got started-speaking for uttnum ~s, but no utts are pending" uttnum))
      (utterance
	(unless (utterance-num text)
	  (error "pending utt has no uttnum"))
	(unless (eql uttnum (utterance-num text))
	  (error "started-speaking uttnum ~s failed to match pending utt uttnum ~s" uttnum (utterance-num text)))
	; TODO add this to texttagger-output slot?
        )
      (paragraph
        (unless (paragraph-uttnums text)
	  (error "pending paragraph has no uttnums"))
	(unless (member uttnum (paragraph-uttnums text))
	  (error "TT msg uttnum ~s failed to match any of the uttnums in the pending paragraph: ~s" uttnum (paragraph-uttnums text)))
	(push nil ; TODO or (list (find-arg-in-act msg :content)) ?
	      (text-unit-texttagger-output text)))
      )
    ))

;; If we sent text to TextTagger as an utterance message, and there was an
;; error, we get an error (not reject) message back
(defun handle-error-from-texttagger (msg comment)
  (when (and *pending-text-units* (utterance-p (first *pending-text-units*)))
    (finish-text-unit-with-error
        (first *pending-text-units*)
	(make-condition 'simple-error
	    :format-control "TextTagger error: ~a"
	    :format-arguments (list comment))
	)))

;(defun handle-utterance-from-texttagger (msg args)
;  ; TODO ?
;  )


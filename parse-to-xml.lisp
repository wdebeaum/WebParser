
(in-package :webparser)
#|
;; we get this from (parse '(return))
(defvar dtt-parse
 '(UTT :TYPE W::UTT :CHANNEL NIL :ROOT ONT::V32894 :TERMS
   ((TERM :LF (ONT::SPEECHACT ONT::V32894 ONT::SA_REQUEST :CONTENT ONT::V32822)
     :VAR ONT::V32894 :SEM NIL :INPUT (W::DRIVE W::THE W::TRUCK) :START 0 :END
     16)
    (TERM :LF
     (ONT::F ONT::V32822 (:* ONT::DRIVE W::DRIVE) :THEME ONT::V32844 :AGENT
      ONT::V32880 :TMA ((W::TENSE W::PRES)))
     :VAR ONT::V32822 :SEM
     ($ F::SITUATION (F::ASPECT F::UNBOUNDED) (F::TIME-SPAN F::EXTENDED)
      (F::CAUSE F::AGENTIVE) (F::LOCATIVE -) (F::TYPE F::EVENTUALITY)
      (F::ORIGIN F::ANY-ORIGIN) (F::CONTAINER -)
      (F::INFORMATION F::MENTAL-CONSTRUCT) (F::INTENTIONAL -) (F::TRAJECTORY +))
     :INPUT (W::DRIVE W::THE W::TRUCK) :START 0 :END 16)
    (TERM :LF (ONT::IMPRO ONT::V32880 ONT::PERSON :CONTEXT-REL ONT::*YOU*) :VAR
     ONT::V32880 :SEM
     ($ F::PHYS-OBJ (F::FORM F::SOLID-OBJECT) (F::MOBILITY F::SELF-MOVING)
      (F::GROUP -) (F::SPATIAL-ABSTRACTION F::SPATIAL-POINT) (F::ORIGIN F::HUMAN)
      (F::CONTAINER -) (F::INFORMATION -) (F::INTENTIONAL +) (F::TRAJECTORY -))
     :START NIL :END NIL)
    (TERM :LF (ONT::THE ONT::V32844 (:* ONT::LAND-VEHICLE W::TRUCK)) :VAR
     ONT::V32844 :SEM
     ($ F::PHYS-OBJ (F::FORM F::ENCLOSURE) (F::MOBILITY F::LAND-MOVABLE)
      (F::GROUP -) (F::SPATIAL-ABSTRACTION F::SPATIAL-POINT)
      (F::OBJECT-FUNCTION F::VEHICLE) (F::ORIGIN F::ARTIFACT)
      (F::CONTAINER (F::OR + -)) (F::INFORMATION -) (F::INTENTIONAL -)
      (F::SCALE NIL) (F::TRAJECTORY -))
     :INPUT (W::THE W::TRUCK) :START 5 :END 16))
   :UTTNUM NIL :WORDS (W::DRIVE W::THE W::TRUCK))
)
|#

(defun split-tt-output-into-utts (utts tt-output)
  "Given a list of UTTs and a list of messages output from TextTagger, return a
   partition of the tt-output list, with one sublist for each UTT."
  (let ((min-term-offsets (reverse
	  ;; get the minimum character offset mentioned in any term of each UTT,
	  ;; monotonically increasing
          (loop with prev-min = nil
	        for utt in (reverse utts)
	        for terms = (find-arg-in-act utt :terms)
		for term-offsets =
		  (remove nil
		      (mapcan
		        (lambda (term)
			  (list (find-arg-in-act term :start)
			        (find-arg-in-act term :end)))
			terms
			;; NOTE: Using :start/:end on the UTT itself doesn't
			;; work because often the parser outputs an UTT with no
			;; terms that spans the entire
			;; utterance/new-speech-act, even though it's just a
			;; piece of punctuation at the end.
			;(cons utt terms)
			))
		for min-offset = 
		  (cond
		    (prev-min (apply #'min (cons prev-min term-offsets)))
		    (term-offsets (apply #'min term-offsets))
		    (t nil)
		    )
		do (setf prev-min min-offset)
		collect min-offset
		))))
; debug
;    (loop for utt in utts
;          for uttnum = (find-arg-in-act utt :uttnum)
;          for mto in min-term-offsets
;	  for i = 0 then (1+ i)
;	  do (format t "min term offset for uttnum ~s.~s is ~s~%" uttnum i mto))
    ;; loop through TextTagger output messages, assigning each one to the last
    ;; UTT whose minimum character offset is <= the message's end offset
    (loop with ret = (make-list (length utts))
          for msg in tt-output
	  for msg-end-offset = (second (find-arg (cddr msg) :frame))
	  do (loop for min-term-offset in min-term-offsets
	           for i = 0 then (1+ i)
		   while (and min-term-offset
		              (<= min-term-offset msg-end-offset))
		   finally
		     (unless (or (= i 0)
		                 (and min-term-offset
		                      (<= min-term-offset msg-end-offset)))
		       (decf i))
		     (push msg (nth i ret))
		   )
	  ;; using push reversed the order of the messages, so we have to undo
	  ;; that for each UTT's list of messages before returning them
	  finally (return (mapcar #'reverse ret))
	  )))

(defun split-tt-output-into-utterances (tt-output)
  "Given a list of messages output from TextTagger, return a partition of the
   list, with one sublist for each utterance. remove started/stopped-speaking
   messages, and put the utterance message at the beginning of each list
   instead of the end."
  (let ((utterances (list nil)))
    (dolist (msg tt-output)
      (case (car msg)
        (utterance ; comes at the end of the msgs for an utterance
	  ;; reverse other messages to undo effect of pushing each
	  (setf (car utterances) (nreverse (car utterances)))
	  ;; put utterance message at the beginning
	  (push msg (car utterances))
	  ;; start a new utterance
	  (push nil utterances)
	  )
	((started-speaking stopped-speaking) nil) ; ignore
	(t (push msg (car utterances)))
	))
    (pop utterances) ; discard nil
    ;; reverse to undo effect of pushing each utterance list
    (nreverse utterances)))

(defun flatten-tma (term)
  (cond
    ((null term) nil)
    ((eq :tma (car term))
      (nconc
	(mapcan
	  (lambda (pair)
	    (list (intern (symbol-name (car pair)) :keyword)
		  (second pair)))
	  (cadr term))
	(flatten-tma (cddr term))
	))
    (t (cons (car term) (flatten-tma (cdr term))))
    ))

(defun wrapped-tags-to-xml-stream (tt-output s)
  (when tt-output
    (format-xml-start s '(tags ""))
    (format-xml s `(lisp ,(format nil "~s" tt-output)))
    (tags-to-xml-stream tt-output s)
    (format-xml-end s "tags")
    ))

(defun utt-to-xml-stream (utt tree tt-output s)
  (let* ((terms (mapcar
                  (lambda (term)
		    (let ((flat-lf (flatten-tma (find-arg-in-act term :lf))))
		      (if (find-arg (cdddr flat-lf) :start) ; assume also :end
			flat-lf
			; :start/:end not in lf, add them from enclosing term
			(append flat-lf
			  (list :start (find-arg-in-act term :start)
				:end (find-arg-in-act term :end)))
			)))
		  (find-arg-in-act utt :terms)))
         (words (find-arg-in-act utt :words))
	 (root (find-arg-in-act utt :root))
	 )
    (format-xml-start s '(utt ""))

    (format-xml s
      `(words
        (lisp ,(format nil "~s" words))
	,@(mapcar
	    (lambda (w)
	      `(word ,(format nil "~a" w)))
	    words)
        ))

    (wrapped-tags-to-xml-stream tt-output s)
    
    (unless tree
      (setf tree (simplify-tree (subst-package
		     (first (find-arg-in-act utt :tree)) :w))))
    (format-xml-start s '(tree ""))
    (format-xml s `(lisp ,(format nil "~s" tree)))
    (tree-to-xml-stream tree s)
    (format-xml-end s "tree")
    
    (format-xml-start s `(terms :root ,(format nil "#~a" (symbol-name root)) ""))
    (format-xml s `(lisp ,(format nil "~s" terms)))
    (lf-to-rdf-stream terms s)
    (format-xml-end s "terms")
    
    ;; NOTE: caller must do this (after adding its own children if any)
    ;(format-xml-end s "utt")
    ))

(defun fix-start-end-rec (speech-act-part tt-start tt-end)
  "Modify speech-act-part in place so that any :START keyword arguments are >=
   tt-start, and any :END keyword arguments are <= tt-end, recursively."
  (when (consp speech-act-part)
    (let ((sa-start (cdr (member :start speech-act-part)))
          (sa-end (cdr (member :end speech-act-part))))
      (when (and sa-start (numberp (car sa-start)) (< (car sa-start) tt-start))
        (setf (car sa-start) tt-start))
      (when (and sa-end (numberp (car sa-end)) (> (car sa-end) tt-end))
        (setf (car sa-end) tt-end))
      (dolist (elem speech-act-part)
        (fix-start-end-rec elem tt-start tt-end))
      )))

(defun fix-start-end (speech-act tt-output)
  "Modify speech-act in place so that all of its :START and :END character
   offsets are within the same range that the tags in tt-output are."
  (let ((tt-offsets (mapcan (lambda (tag)
			      (copy-list (find-arg (cddr tag) :frame)))
			    tt-output)))
    (when tt-offsets
      (fix-start-end-rec speech-act
          (apply #'min tt-offsets) (apply #'max tt-offsets)))))

(defun alt-hyps-to-xml-stream (alt-lfs s)
  (when alt-lfs
    (format s "<alt-hyps>~%")
    (dolist (alt-lf alt-lfs)
      ;; NOTE: we don't pass tt-output because it will be redundant
      (parse-to-xml-stream-rec alt-lf nil nil s))
    (format s "</alt-hyps>~%")
    ))

(defun parse-to-xml-stream-rec (lf alt-lfs tt-output s)
  (cond
    ((eq (car lf) 'compound-communication-act)
      (format s "<compound-communication-act>~%")
      (loop with utts = (find-arg-in-act lf :acts)
            with tt-outputs-by-utt = (split-tt-output-into-utts utts tt-output)
            for utt in utts
            for tree in
	      (or (simplify-tree (subst-package (find-arg-in-act lf :tree) :w))
		  (make-list (length utts)))
	    for msgs in tt-outputs-by-utt
	    do (utt-to-xml-stream utt tree msgs s)
	       (format-xml-end s "utt")
	    )
      (alt-hyps-to-xml-stream alt-lfs s)
      (format s "</compound-communication-act>~%")
      )
    ((eq (car lf) 'utt)
      (utt-to-xml-stream lf nil tt-output s)
      (alt-hyps-to-xml-stream alt-lfs s)
      (format-xml-end s "utt")
      )
    ((eq (car lf) 'failed-to-parse)
      (format-xml s '(failed-to-parse)))
    ((every #'listp lf)
      (when (null tt-output) ; URCS version doesn't have TT
        ;; make a list of NILs matching the utts instead
        (setf tt-output (make-list (length lf))))
      ;; FIXME? We might have gotten a simple list of utts in new-speech-act message for a non-paragraph utterance, in which case tt-output is a list of message content lists, not a list of lists of message content lists. Maybe that was just an ancient bug that got fixed, though.
      (unless (= (length lf) (length tt-output))
        (error "mismatched number of speech acts (~s) and TT utterances (~s)" (length lf) (length tt-output)))
      (loop for sa in lf
	    for i upfrom 0
	    for alt-sas = (mapcar (lambda (hyp) (nth i hyp)) alt-lfs)
            for tto in tt-output
            do (fix-start-end sa tto)
	       (parse-to-xml-stream-rec sa alt-sas tto s)))
;    ((and (= 1 (length lf)) (listp (car lf)))
;      (parse-to-xml-stream-rec (car lf) nil tt-output s)
;      )
    (t
      (format s "<bogus-lf>~%~S~%</bogus-lf>~%" lf)
      )
    ))

(defun run-program-and-read-output (program &rest args)
  "Run the given external program and return a string containing its output.
   What backticks do in Bash, Perl, Ruby, etc."
  (let ((s (make-string-output-stream)))
    (#+cmu ext:run-program
     #+sbcl sb-ext:run-program
     #+ccl ccl:run-program
       program args :output s)
    (get-output-stream-string s)))

(defvar *os-type* (intern (string-upcase (string-trim '(#\Space #\Newline #\Tab) (run-program-and-read-output "/bin/sh" "-c" "uname")))))

(defun get-file-change-date-string (filename)
  "Return a string representing the date that the named file was last changed,
   by calling the stat program with arguments appropriate for the OS."
  (string-trim '(#\Space #\Newline #\Tab)
      (ecase *os-type*
        (Darwin
	  (run-program-and-read-output
	      "/usr/bin/stat"
	      "-f" "%Sm" "-t" "%FT%T%z"
	      filename))
	(Linux
	  (run-program-and-read-output
	      "/usr/bin/stat"
	      "--printf=%y"
	      filename))
	)))

(defun get-build-date ()
  "Return a string representing the date the running system was built."
  ;; decide which file to call stat on
  (let ((urcs-file "/u/www/research/trips/parser/etc/lisp/web-parser-xml.image")
        built-file)
    ;; use the installed image on URCS
    (when (and (not (boundp 'trips::*trips-system*)) (probe-file urcs-file))
      (setf built-file urcs-file))
    ;; otherwise if $TRIPS_BASE/etc/trips-$system.* exists, use it
    (when (and (not built-file) trips::*trips-system*)
      (let ((installed-images
              (directory (trips::make-trips-pathname (concatenate 'string
		  "etc;trips-"
		  (string-downcase (symbol-name trips::*trips-system*))
		  ".*")))))
        (when installed-images
	  (setf built-file (namestring (car installed-images))))))
    ;; otherwise, fall back on the compiled version of this file
    ;; (it's sure to exist, but might be older than the actual most recent
    ;; build date of the system as a whole)
    (unless built-file
      (setf built-file
            (namestring (compile-file-pathname *compile-file-pathname*))))

    (get-file-change-date-string built-file)
    ))

(defun print-xml-header (options input s)
  (let ((*print-pretty* nil)
        (build-date (get-build-date)))
    (format s
"<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<?xml-stylesheet type=\"text/xsl\" href=\"../style/~a\"?>
<!DOCTYPE trips-parser-output SYSTEM \"../trips-parser-output.dtd\">
"
      (or (find-arg options :stylesheet) "parser-interface.xsl"))
    (format-xml-start s
      `(,(ecase (find-arg options :component)
           (parser 'trips-parser-output)
	   (texttagger 'texttagger-output)
	   )
         :parser-build-date ,build-date
	 ,@(when (boundp 'trips::*trips-system*)
	   `(:system ,(symbol-name trips::*trips-system*)))
	 ,@(remove-args options '(:stylesheet :component))
	 ,@(when input `(:input ,input))
	 "" ; avoid />
	 ))
    (format s "~%")
    ))

(defun parse-to-xml-stream (options input debug-output extractions lf alt-lfs tt-output s)
  (print-xml-header options input s)
  (format-xml s `(debug ,debug-output))
  (when (stringp extractions)
    (write-string extractions s))
  (ecase (find-arg options :component)
    (parser
      (parse-to-xml-stream-rec lf alt-lfs tt-output s)
      (format-xml-end s "trips-parser-output")
      )
    (texttagger
      (cond
        ;; paragraph
        ((some (lambda (msg) (eq 'utterance (car msg))) tt-output)
	  (dolist (utterance (split-tt-output-into-utterances tt-output))
	    (format-xml-start s
	      `(utterance :text ,(find-arg-in-act (car utterance) :text) ""))
	    (wrapped-tags-to-xml-stream (cdr utterance) s)
	    (format-xml-end s "utterance")
	    ))
	;; single utterance
	(t
	  (format-xml-start s `(utterance :text ,input ""))
	  (wrapped-tags-to-xml-stream tt-output s)
	  (format-xml-end s "utterance")
	  )
	)
      (format-xml-end s "texttagger-output")
      )
    )
  )

#| not used anymore
(defun show-trees ()
  "Do what show-tree used to do, and return all the trees instead of just the
   first one in a CCA."
  (parser::show-t1 (parser::get-answers) nil))
|#

(defun subst-package (l old-pkg &optional (new-pkg *package*))
  "Return a copy of list/tree structure l with symbols in old-pkg replaced by
   the corresponding symbols in new-pkg."
  (unless (packagep old-pkg)
    (setf old-pkg (find-package old-pkg)))
  (cond
    ((null l) nil)
    ((consp l)
      (cons (subst-package (car l) old-pkg new-pkg)
      	    (subst-package (cdr l) old-pkg new-pkg)))
    ((and (symbolp l) (eq old-pkg (symbol-package l)))
      (intern (symbol-name l) new-pkg))
    (t
      l)
    ))

(defun parse-and-output-xml (input treecontents treeformat lfformat debug s)
  (let ((options `(
	  :component parser
	  :treecontents ,treecontents
	  :treeformat ,treeformat
	  :lfformat ,lfformat
	  ,@(when debug '(:debug "on"))
	  )))
    (cond
      (input
	(multiple-value-bind (debug-output lf)
	           ;; capture debug output
	    (let* ((*standard-output* (make-string-output-stream))
	           (parser::*include-parse-tree-in-messages* '(w::lex))
	           (ig1 (parser::parse `(parser::start-sentence)))
	           (ig2 (parser::parse `(parser::word ,input :frame (0 ,(length input)))))
	           (lf (subst-package (parser::parse '(parser::end-sentence)) :parser)))
	        (declare (ignore ig1 ig2))
	      (values (get-output-stream-string *standard-output*) lf))
	  (parse-to-xml-stream options input debug-output nil lf nil nil s)))
      (t
        (when parser::*semantic-skeleton-scoring-enabled*
	  (setf options (append options '(:semantic-skeleton-scoring "on"))))
	(print-xml-header options input s)
	(format-xml-end s "trips-parser-output")
	)
      )))


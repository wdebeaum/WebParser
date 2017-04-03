(defvar *parts-of-speech* '(adj adv art conj FP infinitival-to N name neg number-unit ordinal prep pro punc quan uttword V value))
(defvar *syntactic-categories* '(ADJP ADVBL AUX CP DET N1 NP number PP PRED QUANP S SPEC UTT V1 VP VP- word))
#|
(defvar *roles* '(
Agent Asset Attribute ARG0 ARG1 ARG2 ACT-0 ACT-1 ACT-2
Beneficiary
Cause Co-Agent Co-Patient Co-Theme CONTEXT-REL
Destination
Experiencer Extent
Goal
Initial_Location Instrument
Location
Material MEMBER MOD
NEGATION
OF
Patient Pivot Predicate Product PROGR PUNCTYPE
QUAN
Recipient Result
Source Stimulus STATIVE SEQUENCE-0 SEQUENCE-1 SEQUENCE-2
Theme Time Topic Trajectory TENSE
Value VAL
))
|#
(defvar *roles* '(
  ;; see also ../DeepSemLex/code/lib/symbol-types.lisp
  action addressee affected affected1 affected-result affected-result1 affected-create affected-create1 agent agent1 along arg0 arg1 assoc-with associated-information beneficiary between cause co-agent co-result co-theme cognizer content contents cost criterion donor duration effect effect-implicit entity experiencer extent figure formal formal1 from-loc goal goal-reln goods ground instrument interval location manner material money neutral neutral1 neutral2 norole obj-val of of-state of1 of2 originator path place position-reln predicate property purpose purpose-implicit reason result result-val scale sit-val situation source source-reln spatial-loc stative stimulus theme time-duration-rel time-val to-loc val val2 value via
  ;; not really semantic roles, but might still appear as arguments to LF terms:
  arg2 act act1 act2
  and-element or-element
  operator
  context-rel
  member mod
  negation tense progr perf passive modality
  punctype
  sequence sequence1 sequence2
  vform
  focus
  assoc-poss
  name-of
  proform
  unit amount
  quan refset
  year month day dow am-pm hour minute century era phase
  ))

(defun make-dtd ()
  (with-open-file (f "trips-parser-output.dtd" :direction :output :if-exists :supersede)
    (format f "<!ELEMENT trips-parser-output (debug?, ekb?, (utt | compound-communication-act | failed-to-parse)*)>
<!ATTLIST trips-parser-output input CDATA #IMPLIED
			      system CDATA #REQUIRED
			      parser-build-date CDATA #REQUIRED
			      extscontents CDATA #IMPLIED
			      extsformat CDATA #REQUIRED
			      tagsformat CDATA #REQUIRED
			      treecontents CDATA #REQUIRED
			      treeformat CDATA #REQUIRED
			      lfformat CDATA #REQUIRED
			      debug CDATA #IMPLIED
			      tag-type CDATA #IMPLIED
			      no-sense-words CDATA #IMPLIED
			      senses-only-for-penn-poss CDATA #IMPLIED
			      rule-sets CDATA #IMPLIED
			      rule-set CDATA #IMPLIED
			      trace-level CDATA #IMPLIED>
<!ELEMENT texttagger-output (debug?, utterance*)>
<!ATTLIST texttagger-output input CDATA #IMPLIED
			      system CDATA #REQUIRED
			      parser-build-date CDATA #REQUIRED
			      tagsformat CDATA #REQUIRED
			      debug CDATA #IMPLIED
			      tag-type CDATA #IMPLIED
			      no-sense-words CDATA #IMPLIED
			      senses-only-for-penn-poss CDATA #IMPLIED>
<!ELEMENT debug (#PCDATA)>
<!ELEMENT ekb ANY>
<!ATTLIST ekb file CDATA #IMPLIED>
<!ELEMENT failed-to-parse EMPTY>
<!ELEMENT compound-communication-act (utt*)>
<!ELEMENT utt (words, tags, tree, terms)>
<!ELEMENT utterance (tags)>
<!ATTLIST utterance text CDATA #REQUIRED>
<!ELEMENT lisp (#PCDATA)>
<!ELEMENT words (lisp, word*)>
<!ELEMENT word (#PCDATA | sense-info)*>
<!ATTLIST word lex CDATA #IMPLIED
	       start CDATA #IMPLIED
	       end CDATA #IMPLIED>
<!ELEMENT prefix (sense-info*)>
<!ATTLIST prefix lex CDATA #IMPLIED
	         start CDATA #IMPLIED
	         end CDATA #IMPLIED>
<!ELEMENT tags (lisp, (word | prefix | prefer)*)>
<!ELEMENT sense-info ANY>
<!ATTLIST sense-info penn-parts-of-speech CDATA #IMPLIED
		     trips-parts-of-speech CDATA #IMPLIED
		     ont-types CDATA #IMPLIED
		     wn-sense-keys CDATA #IMPLIED
		     alternate-spellings CDATA #IMPLIED
		     score CDATA #IMPLIED>
<!ELEMENT prefer EMPTY>
<!ATTLIST prefer text CDATA #IMPLIED
		 start CDATA #IMPLIED
		 end CDATA #IMPLIED
		 penn-cats CDATA #IMPLIED
		 score CDATA #IMPLIED>
")

    (format f "~%<!ENTITY % parts-of-speech \"~{~s~^ | ~}\">~%" *parts-of-speech*)
    (dolist (pos *parts-of-speech*)
      (format f "<!ELEMENT ~s (#PCDATA)>~%" pos))

    (format f "~%<!ENTITY % syntactic-categories \"~{~s~^ | ~}\">~%" *syntactic-categories*)
    (format f "<!ENTITY % syncatdef \"((%parts-of-speech; | %syntactic-categories;)*)\">~%")
    (dolist (cat *syntactic-categories*)
      (format f "<!ELEMENT ~s %syncatdef;>~%" cat))

    (format f "~%<!ELEMENT tree (lisp, (%syntactic-categories;))>

<!ELEMENT terms (lisp, rdf:RDF)>
<!ATTLIST terms root CDATA #REQUIRED>
<!ELEMENT rdf:RDF (rdf:Description*)>
<!ATTLIST rdf:RDF
          xmlns:rdf CDATA #FIXED \"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"
          xmlns:role CDATA #FIXED \"http://www.cs.rochester.edu/research/trips/role#\"
          xmlns:LF CDATA #FIXED \"http://www.cs.rochester.edu/research/trips/LF#\"
          >
")

    (format f "~%<!ENTITY % roles \"~{role:~s~^ | ~}\">~%" *roles*)

    (format f "~%<!ENTITY % roledef \"(#PCDATA)\">~%")
    (dolist (role *roles*)
      (format f "<!ELEMENT role:~s %roledef;>~%" role))
    
    (format f "~%<!ENTITY % roleattdef \"rdf:resource CDATA #IMPLIED\">~%")
    (dolist (role *roles*)
      (format f "<!ATTLIST role:~s %roleattdef;>~%" role))

    (format f "~%~%<!ELEMENT rdf:Description (LF:indicator, LF:type, LF:word?, ((%roles;)*))>
<!ATTLIST rdf:Description rdf:ID ID #REQUIRED>
<!ELEMENT LF:indicator (#PCDATA)>
<!ELEMENT LF:type (#PCDATA)>
<!ELEMENT LF:word (#PCDATA)>
<!ELEMENT LF:start (#PCDATA)>
<!ELEMENT LF:end (#PCDATA)>
")
    ))

(in-package :webparser)

#|

Different formats for parse trees that have been used over the years:

ancient:
(defvar lisp-tree
  '(UTT (S (NP (SPEC (QUAN EVERY)) (N1 (N DOG))) (VP (VP- (V CHASES) (NP (SPEC (DET (ART A))) (N1 (N CAT)))))))
  )

(defvar lisp-tree-2
  '(UTT (S (NP (SPEC (DET (ART THE))) (N1 (N MAN))) (VP (VP- (V ATE) (NP (SPEC (DET (ART AN))) (N1 (N ORANGE)))))))
  )

old:
(subst-package (show-trees) :parser)
((UTT
  (UTT
   (S
    (NP
     (N1
      (N1 (POSSESSOR (NP (N1 (NAME :FEATS (LEX ERK)))) (^S :FEATS (LEX ^S)))
       (N1 (NP (NAME :FEATS (LEX (B PUNC-MINUS RAF))))
        (N1 (N :FEATS (LEX PHOSPHORYLATION)))))))
    (VP (VP- (V :FEATS (LEX CAUSED)) (NP (PRO :FEATS (LEX SOMETHING)))))))
  (PUNC :FEATS (LEX PUNC-PERIOD))))

new:
(subst-package (find-arg-in-act utt :TREE) :w)
((UTT :FEATS ((LEX CAUSED)) :SUB
  ((UTT :FEATS ((LEX CAUSED)) :SUB
    ((S :FEATS ((LEX CAUSED)) :SUB
      ((NP :FEATS ((LEX PHOSPHORYLATION)) :SUB
        ((N1 :FEATS ((LEX PHOSPHORYLATION)) :SUB
          ((N1 :FEATS ((LEX PHOSPHORYLATION)) :SUB
            ((POSSESSOR :FEATS ((LEX ERK)) :SUB
              ((NP :FEATS ((LEX ERK)) :SUB
                ((N1 :FEATS ((LEX ERK)) :SUB ((NAME :FEATS ((LEX ERK)))))))
               (^S :FEATS ((LEX ^S)))))
             (N1 :FEATS ((LEX PHOSPHORYLATION)) :SUB
              ((NP :FEATS ((LEX (B PUNC-MINUS RAF))) :SUB
                ((NAME :FEATS ((LEX (B PUNC-MINUS RAF))))))
               (N1 :FEATS ((LEX PHOSPHORYLATION)) :SUB
                ((N :FEATS ((LEX PHOSPHORYLATION)))))))))))))
       (VP :FEATS ((LEX CAUSED)) :SUB
        ((VP- :FEATS ((LEX CAUSED)) :SUB
          ((V :FEATS ((LEX CAUSED)))
           (NP :FEATS ((LEX SOMETHING)) :SUB
            ((PRO :FEATS ((LEX SOMETHING)))))))))))))
   (PUNC :FEATS ((LEX PUNC-PERIOD))))))

|#

(defun tree-to-xml-stream (lt s)
  "Convert a syntax tree lt to XML and write it to stream s. Accepts trees in
   the ancient or old formats (use simplify-tree for the new format)." 
  (unless (listp lt)
    (error "expected tree to be a list, but got ~s" lt))
  (let* ((tagname (format nil "~a" (car lt)))
	 (apos-pos (position #\^ tagname)))
    (when apos-pos
      (setf tagname (concatenate 'string
	  (subseq tagname 0 apos-pos)
	  (if (= 0 apos-pos) "" "-")
	  "apostrophe"
	  (if (= (- (length tagname) 1) apos-pos) "" "-")
	  (subseq tagname (1+ apos-pos))
	  )))
    (format-xml-start s (list tagname ""))
    (cond
      ((eq :feats (second lt)) ; leaf
        (escape-for-xml s
	    (let ((lex (second (assoc 'lex (cddr lt)))))
	      (if (listp lex)
		(format nil "~{~a~^ ~}" lex)
		(format nil "~a" lex)
		))))
      ((every #'listp (cdr lt)) ; internal node
	(dolist (c (cdr lt))
	  (tree-to-xml-stream c s)))
      (t ; ancient leaf
        (escape-for-xml s (format nil "~{~a~^ ~}" (cdr lt))))
      )
    (format-xml-end s tagname)
    ))

(defun simplify-tree (lt)
  "Given a tree or list of trees in one of the newer formats, simplify it down
   to the ancient format."
  (cond
    ((not (listp lt)) (error "expected tree to be a list, but got ~s" lt))
    ((every #'listp lt) (mapcar #'simplify-tree lt))
    ((member :sub lt) ; new-format internal node
      (destructuring-bind (cat &key feats sub) lt
          (declare (ignore feats))
	(cons cat (simplify-tree sub))))
    ((member :feats lt) ; non-ancient-format leaf
      (destructuring-bind (cat &key feats) lt
        (let* ((feats2 ; make sure it's actually an assoc list
	         (cond
		   ((not (consp feats)) (error "expected :feats to be a nonempty list, but got ~s" feats))
		   ((not (consp (car feats))) ; old format (N :feats (lex foo))
		     (list feats))
		   (t ; new format (N :feats ((lex foo)))
		     feats)))
	       (lex (second (assoc 'lex feats2))))
	  (if (listp lex)
            (cons cat lex) ; (N :feats ((lex (foo bar)))) -> (N foo bar)
	    (list cat lex) ; (N :feats ((lex foo-bar))) -> (N foo-bar)
	    ))))
    ((and (symbolp (car lt)) (every #'listp (cdr lt)))
      ; ancient or old format internal node, recurse
      (cons (car lt) (simplify-tree (cdr lt))))
    (t lt) ; ancient format, leave it alone
    ))


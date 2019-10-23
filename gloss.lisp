(in-package :webparser)

(defun glossenstein-frameset (msg)
  (reply-to-msg msg 'tell :content '(http 200
    :content-type "text/html; charset=utf-8"
    :content "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Frameset//EN\"
<HTML><HEAD><TITLE>Glossenstein</TITLE></HEAD>
<FRAMESET cols=\"34%, 33%, 33%\">
 <FRAMESET rows=\"20%, 20%, 60%\">
  <FRAME src=\"glossenstein?op=process-new-definition\" name=\"process-new-definition\">
  <FRAME src=\"glossenstein?op=process-gloss\" name=\"process-gloss\">
  <FRAME src=\"gloss\" name=\"parser\">
 </FRAMESET>
 <FRAMESET rows=\"50%, 50%\">
  <FRAME src=\"lex-ont?side=lex\" name=\"lexicon\">
  <FRAME src=\"lex-ont?side=ont&ret=xml&q=root\" name=\"ont-type-details\">
 </FRAMESET>
 <FRAME src=\"lex-ont?side=ont\" name=\"ontology\">
</FRAMESET>
</HTML>
")))

(defun process-new-definition-form (msg &optional (status 200) (status-text ""))
  (reply-to-msg msg 'tell :content `(http ,status
    :content-type "text/html; charset=utf-8"
    :content ,(format nil "<!DOCTYPE html>
<html><head><title>process-new-definition form</title></head><body>
<h2>Process New Definition</h2>
<form action=\"#\">
<input type=\"hidden\" name=\"op\" value=\"process-new-definition\">
<label>POS:
<select name=\"pos\">
 <option>N</option>
 <option selected>V</option>
 <option>ADJ</option>
 <option>ADV</option>
</select></label>
<label>Word: <input name=\"word\"></label>
<label>Supertype: ONT::<input name=\"supertype\"></label>
<br>
<label>Definition: <input name=\"def\" size=\"40\"></label>
<br>
<input type=\"submit\" value=\"Process!\">
</form>
~a
</body></html>
" status-text))))

(defun process-gloss-form (msg &optional (status 200) (status-text ""))
  (reply-to-msg msg 'tell :content `(http ,status
    :content-type "text/html; charset=utf-8"
    :content ,(format nil "<!DOCTYPE html>
<html><head><title>process-gloss form</title></head><body>
<h2>Process Gloss</h2>
<form action=\"#\">
<input type=\"hidden\" name=\"op\" value=\"process-gloss\">
<label>WordNet sense key: <input name=\"wn-sense-key\"></label>
<input type=\"submit\" value=\"Process!\">
</form>
~a
</body></html>
" status-text))))

(defun show-type-from-reply (reply-msg)
  (let* ((reply-content (find-arg-in-act reply-msg :content))
         (def (second reply-content))
	 (_ (when (consp (car def)) (setf def (car def))))
	 (ont-type (find-arg-in-act def :trips-type)))
      (declare (ignore _))
    (format nil "<a id=\"result\" href=\"lex-ont?side=ont&q=~(~a~)\" target=\"ontology\">~s</a><script type=\"text/javascript\">document.body.onload=function() { document.getElementById('result').click(); }</script>" (symbol-name ont-type) ont-type)))

(defun def-rejected-p (reply-msg)
  "Does the reply message indicate that the definition was rejected?"
  (dfc::match-msg-pattern '(reply &key :content (answer (rejected-definition . *))) reply-msg))

(defun handle-process-new-definition (msg pos word supertype def)
  (cond
    ;; all params missing or empty
    ((and (or (null pos) (string= "" pos))
          (or (null word) (string= "" word))
          (or (null supertype) (string= "" supertype))
	  (or (null def) (string= "" def)))
      ;; just show the initial form
      (process-new-definition-form msg))
    ;; some params specified, but some params still missing, empty, or invalid
    ((or (null pos) (not (member pos '("N" "V" "ADJ" "ADV") :test #'string=)))
      (process-new-definition-form msg 400 "pos must be one of N, V, ADJ, or ADV"))
    ((or (null word) (string= "" word) (not (every #'alpha-char-p word)))
      (process-new-definition-form msg 400 "word is required, and must contain only alphabetic characters"))
    ((not (every (lambda (c) (or (alpha-char-p c) (digit-char-p c) (member c '(#\- #\_) :test #'char=))) supertype))
      (process-new-definition-form msg 400 "supertype must contain only alphanumeric characters, dashes, or underscores"))
    ;; params valid, send the message
    (t
      (send-msg-with-continuation
	`(request :content (process-new-definition
	    :word ,(intern (string-upcase word))
	    :pos ,(intern pos)
	    :type ,(if (string= "" supertype)
		     nil
		     (intern (string-upcase supertype) :ont)
		     )
	    :definition ,def
	    ))
	(lambda (reply-msg)
	  (cond
	    ((eq 'sorry (car reply-msg))
	      (process-new-definition-form msg 500 (escape-for-xml nil (find-arg-in-act reply-msg :comment))))
	    ((def-rejected-p reply-msg)
	      (process-new-definition-form msg 500 (format nil "definition of ~a rejected" word)))
	    (t
	      (process-new-definition-form msg 200 (format nil "processed new definition of ~a<br>~a" word (show-type-from-reply reply-msg))))
	    ))
	:content-only nil
	))
    ))

(defun handle-process-gloss (msg wn-sense-key)
  (if (null wn-sense-key)
    (process-gloss-form msg))
    (setf wn-sense-key (proper-sense-key wn-sense-key))
    (if (null wn-sense-key)
      (process-gloss-form msg 400 "malformed sense key")
      (send-msg-with-continuation
	`(request :content (process-gloss :sense ,wn-sense-key))
	(lambda (reply-msg)
	  (cond
	    ((eq 'sorry (car reply-msg))
	      (process-gloss-form msg 500 (escape-for-xml nil (find-arg-in-act reply-msg :comment))))
	    ((def-rejected-p reply-msg)
	      (process-new-definition-form msg 500 (format nil "gloss of ~a rejected" wn-sense-key)))
	    (t
	      (process-gloss-form msg 200 (format nil "processed gloss of ~a<br>~a" wn-sense-key (show-type-from-reply reply-msg)))
	    )))
	:content-only nil
	)
      ))

(defun handle-glossenstein (msg query)
  (destructuring-bind (&key op pos word supertype def wn-sense-key &allow-other-keys) query
    (cond
      ((equalp op "process-new-definition")
	(handle-process-new-definition msg pos word supertype def))
      ((equalp op "process-gloss")
	(handle-process-gloss msg wn-sense-key))
      (t (glossenstein-frameset msg))
      )))


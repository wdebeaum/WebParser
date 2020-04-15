;;
;; defsys.lisp for WebParser TRIPS component
;;

(unless (find-package :trips)
  (load (make-pathname :directory '(:relative :up "config" "lisp")
                       :name "trips")))

(unless (find-package :dfc)
  (load #!TRIPS"src;defcomponent;loader"))

(unless (find-package :comm)
  (load #!TRIPS"src;Comm;defsys"))

(unless (find-package :util)
  (load #!TRIPS"src;util;defsys"))

(unless (find-package :parser)
  (load #!TRIPS"src;Parser;defsys"))

(dfc:defcomponent :webparser
		  :nicknames (:wp)
                  :use (:util :common-lisp)
		  :system (
		    :depends-on (:util :comm :parser)
		    :components (
		      "xml"
		      "lf-to-rdf"
		      "tree-to-xml"
		      "tags-to-xml"
		      "parse-to-xml"
		      "utterance"
		      "safe-reader"
		      "lex-ont"
		      "gloss"
		      "messages"
		    )))

(defvar *pending-text-units* nil "List of text-units we're received from a user but haven't yet responded to.")
(defvar *last-uttnum* 0 "The uttnum of the utterance we last got from a user.")
(defvar *original-standard-output* *standard-output*)
(defvar *original-tt-parameters* nil)
(defvar *original-parser-options* nil)
(defvar *previous-extraction-options* nil)
;; NOTE: ~ for $HOME works on SBCL but not CMUCL, but we're not using this on
;; URCS anyway
(defvar *rule-set-path-format* "~~/extraction-rules/~aRuleSet.lisp")
(defvar *show-wn-senses-only-for-core-synsets* nil)
(defvar *allow-lex-ont-editing* (trips:get-env "ALLOW_LEX_ONT_EDITING") "Allow users of the lex-ont browser to dynamically edit the lexicon and ontology. Such edits will affect everyone using this running instance of the system, so you should probably only turn this on if you know you're the only one using this instance.")
(defvar *lex-edits* nil)
(defvar *ont-edits* nil)
(defvar *max-words-for-utterance* 50 "Maximum number of words an utterance parser will accept in a single input text unit.")

(defun run ()
  (dfc:run-component :webparser))


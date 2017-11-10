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

(defun run ()
  (dfc:run-component :webparser))


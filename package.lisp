;;; package definition for html-parser

(defpackage #:breakds.html-parser
  (:use #:cl)
  (:nicknames #:html-parser)
  (:export #:get-children
	   #:get-attrib-list
	   #:get-attrib
	   #:get-tag
	   #:get-text
	   #:parse-html
	   #:parse-webpage
	   #:list-node
	   #:match-child-nodes
	   #:match-first-child
	   #:match-descendence
	   #:collect-nodes))
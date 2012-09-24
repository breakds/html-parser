;;; html-parser.asd

(asdf:defsystem #:html-parser
  :serial t
  :depends-on (#:trivial-http
	       #:cl-ppcre)
  :components ((:file "package")
	       (:file "html-parser")))
  
  
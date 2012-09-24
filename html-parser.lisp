;;; html-parser.lisp

;;; Author: BreakDS 
;;; Maintainer: BreakDS
;;; Date: May 6, 2012

;;; A simple library providing html parsing functionalities

(in-package #:breakds.html-parser)

;;; Some String Operation Utilities

(defmacro empty-string (s)
  "predicate of whether a string is empty" 
  `(= 0 (length ,s)))


(defun lstrip (str)
  "strip the leading spaces (space/tab)"
  (if (empty-string str)
      ""
      (if (or (eq (char str 0) #\  )
	      (eq (char str 0) #\tab))
	  (lstrip (subseq str 1))
	  str)))

(defun rstrip (str)
  "strip the trailing spaces (space/tab)"
  (if (empty-string str)
      ""
      (let ((tail (char str (1- (length str)))))
	(if (or (eq tail #\  )
		(eq tail #\tab))
	    (rstrip (subseq str 0 (1- (length str))))
	    str))))

(defun strip (str)
  "strip from both side"
  (lstrip (rstrip str)))


;;; Sec 0. Html Parser Utilities
;;; Node structure
;;; (tag-name (list-of-value-pairs)
;;;           (list-of-child-node)
(defun get-children (node)
  "get the children list of a particular node"
  (caddr node))

(defun get-attrib-list (node)
  "get the attributes  list of a particular node"
  (cadr node))

(defun get-attrib (node attrib-name)
  "get the value of a specific attribute. Will return nil when not
found"
  (cadr (assoc attrib-name (get-attrib-list node) :test #'equal)))


(defun get-tag (node)
  "get the tag of a node as string"
  (car node))



;;; Assumption: there is no space in value string
(defun extract-attrib (text)
  "extract the attribute list from the text and form an association list"
  (labels ((extract-attrib-iter (text accu)
	     (let* ((s (strip text))
		    (i (position #\= s))) ;; the 1st "="
	       (when i
		 ;; the second = should have some space before its variable
		 (let ((posv (nth-value 2 (cl-ppcre:scan " [^=]*(=)" s :start (1+ i)))))
		   (if posv
		       (let ((j (aref posv 0)))
			 (let ((k (position #\space s :from-end t :end j)))
			   (extract-attrib-iter 
			    (subseq s k)
			    (cons (list (subseq s 0 i) (strip (subseq s (1+ i) k))) accu))))
		       (cons (list (subseq s 0 i) (strip (subseq s (1+ i)))) accu)))))))
    (extract-attrib-iter text nil)))
		    
		    
(defun extract-tag (text)
  "extract a tag with its attributes"
  (let* ((s (strip text))
	 (i (position #\space s)))
    (if i
	(list (subseq s 0 i) (extract-attrib (subseq s (1+ i))))
	(list s nil))))

      

(defun parse-node (text)
  "parse next node, return the node as well as the rest text"
  (let* ((s (lstrip text)) 
	 (i (position #\< s)))
    (if i
      (let ((j (position #\> s)))
	(if j
	    (cond 
	      ((> i 0) (values (subseq s 0 i) (subseq s i))) ;; current node is string
	      ((eq #\/ (char s (1- j))) ;; current node is self-terminating
	       (values (append (extract-tag (subseq s (1+ i) (1- j))) nil) 
			  (subseq s (1+ j))))
	      (t (let ((header (extract-tag (subseq s (1+ i) j))))
		   (if (or (eq #\/ (char (car header) 0))  ;; ending tag
			   (eq #\! (char (car header) 0))) ;; comment tag
		       (values (append header (list nil)) 
			       (subseq s (1+ j)))
		       (multiple-value-bind (tree rest) (parse-tree (subseq s (1+ j)))
			 (values (append header (list tree)) rest))))))
	    (values nil s))) ;; current node doesn't contain anythingo
      (values nil s)))) ;; current node doesn't contain anything


(defun parse-tree (text)
  "parse text into a list of nodes"
  (labels ((parse-tree-iter (text accu)
	     (if (empty-string text)
		 (values nil "")
		 (multiple-value-bind (node rest) (parse-node text)
		   (cond
		     ((atom node) (parse-tree-iter rest (cons node accu))) ;; single string
		     ((eq #\/ (char (car node) 0)) (values (nreverse accu) rest)) ;; ending tag
		     ((eq #\! (char (car node) 0)) (parse-tree-iter rest accu)) ;; comment tag
		     (t (parse-tree-iter rest (cons node accu)))))))) ;; real child node
    (parse-tree-iter text nil)))


(defun parse-html (text)
  "parse html text, and return the <html> node"
  (let ((pos (cl-ppcre:scan "<html" text)))
    (parse-node (subseq text pos))))
	     
(defun get-text (filename)
  "Read html from a file and convert it into a string"
  (with-open-file (*standard-input* filename :direction :input)
    (reduce (lambda (x y) (concatenate 'string x y))
	    (loop for line = (read-line nil nil 'eof)
	       until (eq line 'eof) collect line))))

(defun prase-webpage (url)
  "Parse the webpage read from an url"
  (parse-html
   (let ((stream (third (trivial-http:http-get url))))
     (reduce (lambda (x y) (concatenate 'string x y))
	     (loop for line = (read-line stream nil 'eof)
		until (eq line 'eof) collect line)))))


(defun list-node (node)
  "list the content of a node"
  (fresh-line)
  (format t "<~a>~%" (get-tag node))
  (loop for item in (get-children node)
       do (format t "  <~a>~%" (get-tag item))))
  

(defun match-child-nodes (node tag-name)
  "Collect all the child nodes that have this query tag-name"
  (mapcan (lambda (x) (when (equal (car x) tag-name) (list x)))
	  (get-children node)))

(defun match-first-child (node tag-name)
  "Find the first child node that have this tag-name"
  (assoc tag-name (caddr node) :test #'equal))


(defmacro match-descendence (node &rest tag-names)
  "match a series of tags"
  (reduce (lambda (x y) (list 'match-first-child x y))
	  tag-names
	  :initial-value node))

(defun collect-nodes (node test)
  "collect all the nodes that survive the test function, the harvested
  nodes maybe at arbitrary depth. However, the content of the matched
  nodes will also be searched."
  (mapcan (lambda (x) 
	    (when (not (atom x))
	      (append (when (funcall test x) (list x))
		      (when (get-children node)
			(collect-nodes x test)))))
	  (get-children node)))


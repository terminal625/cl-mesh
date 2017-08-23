(in-package :cl-mesh)

(defparameter foo nil)

(defun separate-attribs (x)
  (let ((value (split-sequence #\/ x)))
    (case (length value)
      (1 (vector (1- (read-from-string (car value))) 0 0))
      (otherwise
       (destructuring-bind (vert uv normal) value
	 (vector (1- (read-from-string vert :eof-value 0)) 
		 (1- (read-from-string uv :eof-value 0))
		 (1- (read-from-string normal :eof-value 0))))))))

(defparameter nope (string #\Return))

(defun parse-wavefront-obj (filename)
  "Returns a hashtable with entries \"vertices\", \"normals\", and \"indices\" containing data"
  (let* ((lines
	  (iterate (for line in-file filename using #'read-line)
		   (let ((stripped-line (string-trim " \\t" line)))
		     (when (not (emptyp stripped-line))
		       (collect (delete-if (lambda (x) (or (string-equal x "")
							   (string-equal x nope)))
					   (split-sequence #\Space stripped-line))))))))
    (let* ((vertex-data
	    (mapcar #'(lambda (x)
			
			(map 'vector
			 #'(lambda (x)
			     (float (read-from-string x)))
			 (cdr x)))
		    (remove-if-not
		     (rcurry #'string-equal "v")
		     lines :key #'car)))
	   (normal-data
	    (mapcar #'(lambda (x)
			(map 'vector
			     #'(lambda (x) (float (read-from-string x)))
			     (cdr x)))
		    (remove-if-not
		     (rcurry #'string-equal "vn")
		     lines :key #'car)))
	   (uv-data
	    (mapcar #'(lambda (x)
			(map 'vector
			     #'(lambda (x) (float (read-from-string x)))
			     (cdr x)))
		    (remove-if-not
		     (rcurry #'string-equal "vt")
		     lines :key #'car)))
	   (index-data
	    (mapcar #'(lambda (x)
			(map 'vector
			 #'separate-attribs
			 (cdr x)))
		    (remove-if-not
		     (rcurry #'string-equal "f")
		     lines :key #'car)))
	   (ht (make-hash-table :test 'equal)))
      (setf (gethash "vertices" ht)
	    (make-array (length vertex-data) :initial-contents vertex-data))
      (setf (gethash "uv" ht)
	    (make-array (length uv-data) :initial-contents uv-data))
      (setf (gethash "indices" ht)
	    (make-array 
	     (length index-data) :initial-contents index-data))
      (setf (gethash "normals" ht)
	    (make-array (length normal-data) :initial-contents normal-data))
      ht)))

(in-package :geomyid)

(defvar *gopher-root*)
(defvar *host*)
(defvar *port*)

(defstruct resource
  type
  title
  selector
  host
  port
  pathname)

(define-condition no-attr (error) ())
(define-condition bad-pathname (error)
  ((path :initarg :path :reader path)))

(defun seqsep (item sequence)
  (let* ((position (position item sequence))
	 (subseq (subseq sequence 0 position))
	 (next (when position
		 (seqsep item (subseq sequence (+ position 1))))))
    (if (emptyp subseq)
	next
	(cons subseq next))))

; Yes, I am aware that this looks like I sneezed on the contacts of a
; keyboard.
(defun get-extattr (pathname attrname)
  (with-alien ((data (array char 255)))
    (let ((attrlen (alien-funcall (extern-alien
				   "extattr_get_file"
				   (function int
					     c-string
					     int
					     c-string
					     (* (array char 255))
					     unsigned))
				  (native-namestring pathname)
				  1
				  attrname
				  (addr data)
				  255)))
      (when (< attrlen 0)
	(error 'no-attr))
      (setf (deref data attrlen) 0)
      (cast data c-string))))

; This does not check existence of files or validity of pathnames, so
; you should only pass in results from (directory)
(defun selector-type (pathname)
  (if (pathname-name pathname)
      (or
       (handler-case (elt (get-extattr pathname "selector-type") 0)
	 (no-attr () nil))
       (switch ((string-downcase (pathname-type pathname)) :test 'string-equal)
	 ("hqx" #\4)
	 ("uu" #\6)
	 ("bin" #\9)
	 ("gif" #\g)
	 ("gifv" #\g)
	 ("htm" #\h)
	 ("html" #\h)
	 ("xhtml" #\h)
	 ("jpg" #\I)
	 ("png" #\I)
	 ("wav" #\s)
	 (otherwise #\0)))
      #\1))


(defun write-text-resource (stream pathname)
  (with-open-file (resource pathname)
    (handler-case (loop
		     (format stream "~A~C~C"
			     (read-line resource)
			     #\Return #\Linefeed))
      (end-of-file ()
	(format stream ".~C~C" #\Return #\Linefeed)))))

(defun write-binary-resource (stream pathname)
  (with-open-file
      (resource pathname :element-type 'unsigned-byte)
    (handler-case (loop
		     (write-byte (read-byte resource) stream))
      (end-of-file ()
	nil))))

(defun selector (pathname)
  (let ((selector "")
	(index (mismatch (pathname-directory pathname)
			 (pathname-directory *gopher-root*)
			 :test #'string-equal)))
    (when index
	  (when (< index (length (pathname-directory *gopher-root*)))
	    (error 'bad-pathname :path (namestring pathname)))
	  (dolist (component (subseq
			      (pathname-directory pathname)
			      index))
	    (setf selector (concatenate 'string
					selector
					"/"
					component))))
    (when (pathname-name pathname)
      (setf selector (concatenate 'string
				  selector
				  "/"
				  (pathname-name pathname))))
    (when (pathname-type pathname)
      (setf selector (concatenate 'string
				  selector
				  "."
				  (pathname-type pathname))))
    selector))

(defun parse-selector (selector)
  (multiple-value-bind (name type)
      (let ((index (position #\Period selector :from-end t)))
	(if
	 index
	 (values
	  (subseq selector 0 index)
	  (subseq selector (+ index 1)))
	 selector))
    (let ((components (seqsep #\Slash name)))
      (or
       (probe-file
	(merge-pathnames
	 (make-pathname
	  :directory (append '(:relative)
			     (butlast components))
	  :name (car (last components))
	  :type type)
	 *gopher-root*))
       (error 'bad-pathname :path selector)))))

(defun resource (pathname)
  (make-resource :type (selector-type pathname)
		 :title (handler-case
			    (get-extattr pathname "title")
			  (no-attr ()
			    (car (last (seqsep
					#\Slash
					(selector pathname))))))
		 :selector (handler-case
			       (get-extattr pathname "selector")
			     (no-attr () (selector pathname)))
		 :host (handler-case
			   (get-extattr pathname "host")
			 (no-attr () *host*))
		 :port (handler-case
			   (get-extattr pathname "port")
			 (no-attr () *port*))
		 :pathname pathname))

(defun write-directory-resource (stream pathname)
  (handler-case
      (with-open-file
	  (message (merge-pathnames
		    (parse-namestring
		     (get-extattr pathname "message"))
		    pathname))
	(loop
	   (format stream "i~A~Cnone~Cnone~C0~C~C"
		   (read-line message)
		   #\Tab #\Tab #\Tab
		   #\Return #\Linefeed)))
    (no-attr () nil)
    (end-of-file () nil)
    (file-error () nil)
    (stream-error () nil))
  (dolist (resource
	    (sort
	     (loop for file in
		  (directory
		   (merge-pathnames
		    pathname
		    (make-pathname
		     :name :wild
		     :type :wild)))
		collecting (resource file))
	     #'string<
	     :key #'resource-selector))
    (format stream
	    "~C~A~C~A~C~A~C~A~C~C"
	    (resource-type resource)
	    (resource-title resource)
	    #\Tab
	    (resource-selector resource)
	    #\Tab
	    (resource-host resource)
	    #\Tab
	    (resource-port resource)
	    #\Return
	    #\Linefeed))
  (format stream ".~C~C"
	  #\Return #\Linefeed))

(defun write-resource (stream pathname)
  (funcall (case (selector-type pathname)
	     ((#\5
	       #\9
	       #\g
	       #\I
	       #\s) #'write-binary-resource)
	     (#\1 #'write-directory-resource)
	     (t #'write-text-resource))
	   stream
	   pathname))

(defun write-error (stream error)
  (format stream "~C~A~C~C~%~C~A~C~C"
	  #\3 error #\Return #\Linefeed
	  #\i "Root directory listing follows:" #\Return #\Linefeed)
  (write-directory-resource stream *gopher-root*))

(defun serve (root host-name &optional (port 70) (address nil))
  (let ((*gopher-root* (probe-file root))
	(*host* host-name)
	(*port* port)
	(socket (make-instance
		 'inet-socket :type :stream :protocol :tcp)))
    (setf (sockopt-reuse-address socket) t)
    (socket-bind socket
		 (or
		  address
		  (host-ent-address
		   (get-host-by-name *host*)))
		 port)
    (socket-listen socket 20)
    (sb-posix:setgid (sb-posix:getgid))
    (sb-posix:setuid (sb-posix:getuid))

    (handler-case
	(loop
	   (let* ((client (socket-accept socket))
		  (request (socket-make-stream
			    client
			    :element-type :default
			    :input t
			    :output t)))
	     (handler-case
		 (handler-case
		     (write-resource
		      request
		      (parse-selector
		       (first
			(seqsep #\Tab
				(remove-if
				 (lambda (item)
				   (case item
				     ((#\Return
				       #\Linefeed) t)
				     (otherwise nil)))
				 (read-line request))))))
		   (bad-pathname (condition)
		     (write-error request
				  (concatenate 'string
					       "Bad selector: "
					       (path condition)))))
	       (socket-error () nil)
	       (stream-error () nil))
	     (socket-close client)))
      (t (condition)
	(sb-posix:syslog
	 sb-posix:log-err
	 (concatenate
	  'string
	  "Dying due to unknown error. Condition type: "
	  (with-output-to-string (syslog)
	    (princ (type-of condition)))))
	(socket-close socket)))))

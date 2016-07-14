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
#+(or freebsd linux)
(defun get-extattr (pathname attrname)
  (with-alien ((data (array char 255)))
    (let ((attrlen
	   (alien-funcall (extern-alien
			   #+freebsd "extattr_get_file"
			   #+linux "getxattr"
			   (function int
				     c-string
				     #+freebsd int
				     c-string
				     (* (array char 255))
				     unsigned))
			  (native-namestring pathname)
			  #+freebsd 1
			  #+freebsd attrname
			  #+linux (concatenate 'string "user." attrname)
			  (addr data)
			  255)))
      (when (<= attrlen 0)
	(error 'no-attr))
      (setf (deref data attrlen) 0)
      (cast data c-string))))
#-(or freebsd linux)
(error "Operating system not supported")

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
	     :key (lambda (resource)
		    (pathname-name
		     (resource-pathname resource)))))
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

(defun write-dynamic-resource (stream pathname)
  (let ((process (handler-case (sb-ext:run-program pathname nil :output :stream)
		   (t (error) (format t "Unknown error: ~A~%" error) nil))))
    (when process
      (let ((resource (sb-ext:process-output process)))
	(handler-case (loop
			 (format stream "~A~C~C"
				 (read-line resource)
				 #\Return #\Linefeed))
	  (end-of-file ()
	    (format stream ".~C~C" #\Return #\Linefeed)))))))

(defun write-resource (stream pathname)
  (funcall (if (pathname-name pathname)
	       (handler-case
		   (progn
		     (sb-posix:access pathname sb-posix:x-ok)
		     #'write-dynamic-resource)
		 (t () (case (selector-type pathname)
			 ((#\5
			   #\9
			   #\g
			   #\I
			   #\s) #'write-binary-resource)
			 (#\1 #'write-text-resource)
			 (t #'write-text-resource))))
	       #'write-directory-resource)
	   stream
	   pathname))

(defun write-html-redirect (stream url)
  (format stream "<html>
<head>
<meta http-equiv=\"refresh\" content=\"5; url=~A\">
</head>
<body>
<p>
The directory entry you selected was supposed to link to an HTTP-based
website. Unfortunately, your gopher client is pathetic and doesn't
support the hURL specification, so I've had to insert this shitty page
into my Gopher server. I hope you're happy.
</p>
<p>
If you're not redirected after 5 seconds of this loveliness, click the
below link to the URL you tried (and failed) to access.
</p>
<p>
The place you're supposed to be is: <a href=\"~A\">~A</a>
</p>
<p>
Gopher ftw!!1
</p>
</body>
</html>
"
	  url url url))

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
;    (setf (sockopt-reuse-port socket) t)
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
		     (let* ((path (path condition))
			    (url-start
			     (mismatch path "URL:" :test #'char=)))
		       (if (= (or url-start 0) 4)
			   (write-html-redirect
			    request
			    (subseq path url-start))
			   (write-error request
					(concatenate 'string
						     "Bad selector: "
						     path))))))
	       (socket-error () nil)
	       (stream-error () nil))
	     (handler-case
		 (socket-close client)
	       (stream-error () nil))))
      (t (condition)
	(sb-posix:syslog
	 sb-posix:log-err
	 (format
	  nil
	  "Dying due to unknown error. Condition type: ~A"
	  (type-of condition)))))
	(socket-close socket)))

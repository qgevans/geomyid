(in-package :geomyid)

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
#+win32
(defun get-extattr (pathname attrname)
  (let* ((attr-path (concatenate 'string
				 (namestring pathname)
				 ":"
				 attrname))
	 (attr (remove #\Return
		       (handler-case
			   (with-open-file (attr attr-path)
			     (read-line attr))
			 (file-error () (error 'no-attr))))))
    (unless (> (length attr) 0)
      (error 'no-attr))
    attr))
#-(or freebsd linux win32)
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

(defun bad-resource (pathname error)
  (make-resource :type #\3
		 :title (format nil "Bad resource (~A): ~A" pathname error)
		 :selector "none"
		 :host "none"
		 :port "0"
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
		collecting (handler-case
			       (resource file)
			     (bad-pathname (err)
			       (bad-resource file err))))
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
		   (t (error) (write-error stream error) nil))))
    (when process
      (let ((resource (sb-ext:process-output process)))
	(handler-case (loop
			 (format stream "~A~C~C"
				 (read-line resource)
				 #\Return #\Linefeed))
	  (end-of-file ()
	    (format stream ".~C~C" #\Return #\Linefeed)))))))

(defun executable-p (pathname)
  #+unix
  (handler-case
      (progn
	(sb-posix:access pathname sb-posix:x-ok)
	t)
    (sb-posix:syscall-error () nil))
  #+win32
  (switch ((pathname-type pathname) :test 'equalp)
    ("exe" t)
    ("cmd" t)
    ("bat" t)))

(defun write-resource (stream pathname)
  (funcall (if (pathname-name pathname)
	       (if (executable-p pathname)
		   #'write-dynamic-resource
		   (case (selector-type pathname)
		     ((#\5
		       #\9
		       #\g
		       #\I
		       #\s) #'write-binary-resource)
		     (#\1 #'write-text-resource)
		     (t #'write-text-resource)))
	       #'write-directory-resource)
	   stream
	   pathname))

(defpackage geomyid
  (:use #:common-lisp #:sb-ext #:sb-alien
	#:sb-bsd-sockets #:alexandria
	#+sb-thread #:sb-thread)
  (:export #:serve))

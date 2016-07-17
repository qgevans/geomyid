(defpackage geomyid
  (:use #:common-lisp #:sb-ext #:sb-alien
	#:sb-bsd-sockets #:alexandria
	#:solipsism
	#+sb-thread #:sb-thread)
  (:export #:serve))

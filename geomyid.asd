(in-package :asdf-user)

(defsystem "geomyid"
  :description "geomyid: A gopher server for SBCL/FreeBSD"
  :version "0.0.1"
  :author "Quinn Evans <heddwch@lambdaos.org>"
  :license "2-clause BSD"
  :depends-on (#:alexandria
	       #:solipsism
	       #:sb-bsd-sockets
	       #:sb-posix)
  :components ((:file "package")
	       (:file "resource" :depends-on ("package"))
	       (:file "serve" :depends-on ("resource" "package"))))

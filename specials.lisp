(in-package :geomyid)

(defvar *gopher-root*)
(defvar *host*)
(defvar *port*)

#+sb-thread (defconstant +threads+ 10)
#+sb-thread (defvar *clients* (make-queue))

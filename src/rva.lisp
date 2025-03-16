(defpackage :rva
  (:use :cl))

(in-package :rva)

(defparameter *version* "v0.1")

(defun error-cli (message)
  (format *error-output*
	    "~a~%Usage:
	risc_vector file
Options:
	--version, -v: print version information~%"
	    message)
  (sb-ext:exit :code 1))

(defun main ()
  (lex:stub)
  (error-cli "foobar"))

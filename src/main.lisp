(in-package #:rva)

(defparameter *banner*
  "      _/_/                                  _/_/  
     _/                                      _/   
    _/    _/  _/_/  _/      _/    _/_/_/    _/    
   _/    _/_/      _/      _/  _/    _/    _/     
  _/    _/          _/  _/    _/    _/    _/      
 _/    _/            _/        _/_/_/    _/       
_/_/                                  _/_/  "
  "Stylized ASCII logo.")

(defun print-version-number ()
  "Prints a pretty splash-screen."
  (let ((system (asdf:find-system "rva" nil)))
    (format t "~av~a~%" *banner* (asdf:component-version system))))

(defun error-cli (message)
  "Prints MESSAGE and usage information to stderr
and exits with error code 1."
  (format *error-output*
	  "~a~%Usage:
	rva file~%"
	  message)
  (sb-ext:exit :code 1))

(defun main ()
  (print-version-number)
  (error-cli "Nitimur in Vetitum"))

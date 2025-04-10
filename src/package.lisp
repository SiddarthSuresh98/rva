(defpackage #:rva
  (:use #:cl)
  (:export #:main))

(defpackage #:util
  (:use #:cl)
  (:export #:asm-extension?
           #:format-as-binary
	   #:insert-in-middle
	   #:iota
	   #:riffle
	   #:add-variable
	   #:add-label
	   #:get-variable
	   #:get-label
	   #:r-type
	   #:i-type
	   #:j-type))

(defpackage #:parse
  (:use #:cl)
  (:export #:str->ast
	   #:line-number))

(defpackage #:emit
  (:use #:cl)
  (:export #:emit))

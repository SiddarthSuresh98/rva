(defpackage #:rva
  (:use #:cl)
  (:export #:main))

(defpackage #:util
  (:use #:cl)
  (:export #:asm-extension?
           #:format-as-binary
	   #:insert-in-middle
	   #:iota
	   #:riffle))

(defpackage #:parse
  (:use #:cl)
  (:export ;; exported for testing only
	   #:text
	   #:register
	   #:instr
	   ))

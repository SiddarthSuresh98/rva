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

(defpackage #:lex
  (:use #:cl)
  (:export #:lexer-error
           #:file->tokens
           ;; exported for testing only
           #:read-token))

(defpackage #:parse
  (:use #:cl)
  (:export #:parser-error
           #:tokens->ast
	   ;; exported for testing only
	   #:register
	   #:instr
	   ))

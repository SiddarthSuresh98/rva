(in-package #:rva-tests)

(def-suite parse-tests
  :description "Test functions exported from the parser."
  :in all-tests)

;;; these tests are not exhaustive, and are meant to test basic functionality
;;; under correct circumstances.

(in-suite parse-tests)

(test esrap-register-bases
      (is (equal '(emit::p
		   (emit::d)
                   (emit::x
                    (emit::r "ADD" (emit::rr 10) (emit::rr 10) (emit::rr 10))))
                 (esrap:parse 'parse:str->ast (format nil ".DATA~%.TEXT~%~tADD $0O012 $0B1010 $0XA~%")))))

(test esrap-instr-all-type-r
      (is (equal
           '(emit::p
	     (emit::d)
             (emit::x
              (emit::r "ADDV" (emit::rr 1) (emit::rr 2) (emit::rr 3))
              (emit::r "NOT" (emit::rr 4) (emit::rr 0) (emit::rr 5))
              (emit::r "CMP" (emit::rr 6) (emit::rr 7) (emit::rr 0))))
           (esrap:parse 'parse:str->ast (format nil ".DATA~%.TEXT~%~tADDV $3 $1 $2
~tNOT $5 $4~%~tCMP $6 $7~%")))))

(test esrap-instr-all-type-i
      (is (equal
           '(emit::p
	     (emit::d)
             (emit::x
              (emit::i "LOADV" (emit::rr 8) (emit::rr 9) (emit::imm 1))
              (emit::i "STORE" (emit::rr 3) (emit::rr 5) (emit::imm 3))
              (emit::i "ADDI" (emit::rr 4) (emit::rr 5) (emit::imm 2))))
           (esrap:parse 'parse:str->ast (format nil ".DATA~%.TEXT~%~tLOADV $8 1($9)
~tSTORE $5 3($3)~%~tADDI $5 $4 2~%")))))

(test esrap-instr-type-all-type-j
      (is (equal
           '(emit::p
	     (emit::d)
             (emit::x
              (emit::j "JMP" (emit::rr 3) (emit::imm 3))
              (emit::j "JRL" (emit::rr 0) (emit::l "FOO" 8))
              (emit::j "PUSH" (emit::rr 5) (emit::imm 0))))
           (esrap:parse 'parse:str->ast (format nil ".DATA~%.TEXT~%~tJMP 3($3)
~tJRL FOO~%~tPUSH $5~%")))))

(test esrap-instr-type-i-negative
      (is (equal
           '(emit::p
	     (emit::d)
             (emit::x
              (emit::i "LOADV" (emit::rr 8) (emit::rr 3) (emit::imm -3))))
           (esrap:parse 'parse:str->ast (format nil ".DATA~%.TEXT~%~tLOADV $8 -3($3)~%")))))

(test esrap-instr-type-i-vars
      (is (equal
           '(emit::p
	     (emit::d)
             (emit::x
              (emit::i "LOADV" (emit::rr 8) (emit::rr 0) (emit::var "vector"))
              (emit::i "STORE" (emit::rr 0) (emit::rr 5) (emit::var "int"))))
           (esrap:parse 'parse:str->ast (format nil ".DATA~%.TEXT~%~tLOADV $8 vector
~tSTORE $5 int~%")))))

(test esrap-instr-type-all-lazy-spaces
      (is (equal
           '(emit::p
	     (emit::d)
             (emit::x
              (emit::j "JMP" (emit::rr 3) (emit::imm 3))
              (emit::j "JRL" (emit::rr 0) (emit::l "FOO" 14))
              (emit::j "PUSH" (emit::rr 5) (emit::imm 0))))
           (esrap:parse 'parse:str->ast (format nil ".DATA~%~%.TEXT~t~%JMP 3($3)~t
JRL FOO~t~%PUSH $5~%")))))

(test esrap-data-singleton
      (is (equal
           '(emit::p
             (emit::d
              1)
	     (emit::x))
           (esrap:parse 'parse:str->ast (format nil ".DATA~%~tA 1~%.TEXT~%")))))

(test esrap-data-loaded
      (is (equal
           '(emit::p
             (emit::d
              1 2 3 4 5 6 7 8)
	     (emit::x))
           (esrap:parse 'parse:str->ast (format nil ".DATA~%~tB 1 2 3 4 5 6 7 8
.TEXT~%")))))

(test esrap-data-triple
      (is (equal
           '(emit::p
             (emit::d
              5 6 7 8 4 3 5)
	     (emit::x))
           (esrap:parse 'parse:str->ast (format nil ".DATA~%~tC 5 6 7 8~%~tD 4
~tE 3 5~%.TEXT~%")))))

(test esrap-data-lazy-spaces
      (is (equal
           '(emit::p
             (emit::d
              5 6 7 8 4 3 5)
	     (emit::x))
           (esrap:parse 'parse:str->ast (format nil "~%~t.DATA~t~%F 5 6 7 8~t~%G 4
H 3 5~%.TEXT~%")))))

(test esrap-negative-ints
      (is (equal
	   '(emit::p
	     (emit::d
	      -1)
	     (emit::x
	      (emit::i "LOADV" (emit::rr -8) (emit::rr -3) (emit::imm -3))))
	   (esrap:parse 'parse:str->ast (format nil ".DATA~%~tm -1~%.TEXT~%~tLOADV $-8 -3($-3)~%")))))

(test esrap-data-full
      (is (equal
           '(emit::p
             (emit::d
              1 2 3 4 3 0)
             (emit::x
              (emit::i "LOAD" (emit::rr 5) (emit::rr 0) (emit::var "S"))
              (emit::i "LOAD" (emit::rr 10) (emit::rr 0) (emit::var "ARR"))
              (emit::i "LOAD" (emit::rr 6) (emit::rr 0) (emit::var "I"))
              (emit::j "JRL" (emit::rr 0) (emit::l "CMP" 20))
              (emit::r "ADD" (emit::rr 10) (emit::rr 6) (emit::rr 9))
              (emit::i "ADDI" (emit::rr 6) (emit::rr 6) (emit::imm 1))
              (emit::r "CMP" (emit::rr 6) (emit::rr 5) (emit::rr 0))
              (emit::j "BGT" (emit::rr 0) (emit::l "L" 24))))
           (esrap:parse 'parse:str->ast (format nil "
.DATA
        ARR 1 2 3 4
        S   3
        I   0

.TEXT
        LOAD $5 S
        LOAD $10 ARR
        LOAD $6 I
        JRL CMP
L:
        ADD $9 $10 $6
        ADDI $6 $6 0X1
CMP:
        CMP $6 $5
        BGT L~%")))))

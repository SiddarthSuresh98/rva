(in-package #:rva-tests)

(def-suite parse-tests
  :description "Test functions exported from the parser."
  :in all-tests)

;;; these tests are not exhaustive, and are meant to test basic functionality
;;; under correct circumstances.

(in-suite parse-tests)

(test esrap-register-bases
      (is (equal '(parse::p
                   (parse::x
                    (parse::r "ADD" (parse::rr 10) (parse::rr 10) (parse::rr 10))))
                 (esrap:parse 'parse::str->ast (format nil ".TEXT~%~tADD $0O012 $0B1010 $0XA~%")))))

(test esrap-instr-all-type-r
      (is (equal
           '(parse::p
             (parse::x
              (parse::r "ADDV" (parse::rr 1) (parse::rr 2) (parse::rr 3))
              (parse::r "NOT" (parse::rr 4) (parse::rr 0) (parse::rr 5))
              (parse::r "CMP" (parse::rr 6) (parse::rr 7) (parse::rr 0))))
           (esrap:parse 'parse::str->ast (format nil ".TEXT~%~tADDV $3 $1 $2
~tNOT $5 $4~%~tCMP $6 $7~%")))))

(test esrap-instr-all-type-i
      (is (equal
           '(parse::p
             (parse::x
              (parse::i "LOADV" (parse::rr 8) (parse::rr 9) (parse::imm 1))
              (parse::i "STORE" (parse::rr 3) (parse::rr 5) (parse::imm 3))
              (parse::i "ADDI" (parse::rr 5) (parse::rr 4) (parse::imm 2))))
           (esrap:parse 'parse::str->ast (format nil ".TEXT~%~tLOADV $8 1($9)
~tSTORE $5 3($3)~%~tADDI $5 $4 2~%")))))

(test esrap-instr-type-all-type-j
      (is (equal
           '(parse::p
             (parse::x
              (parse::j "JMP" (parse::rr 3) (parse::imm 3))
              (parse::j "JRL" (parse::rr 0) (parse::l "FOO"))
              (parse::j "PUSH" (parse::rr 5) (parse::imm 0))))
           (esrap:parse 'parse::str->ast (format nil ".TEXT~%~tJMP 3($3)
~tJRL FOO~%~tPUSH $5~%")))))

(test esrap-instr-type-i-vars
      (is (equal
           '(parse::p
             (parse::x
              (parse::i "LOADV" (parse::rr 8) (parse::rr 0) (parse::var "vector"))
              (parse::i "STORE" (parse::rr 0) (parse::rr 5) (parse::var "int"))))
           (esrap:parse 'parse::str->ast (format nil ".TEXT~%~tLOADV $8 vector
~tSTORE $5 int~%")))))

(test esrap-instr-type-all-lazy-spaces
      (is (equal
           '(parse::p
             (parse::x
              (parse::j "JMP" (parse::rr 3) (parse::imm 3))
              (parse::j "JRL" (parse::rr 0) (parse::l "FOO"))
              (parse::j "PUSH" (parse::rr 5) (parse::imm 0))))
           (esrap:parse 'parse::str->ast (format nil "~t~%.TEXT~t~%JMP 3($3)~t
JRL FOO~t~%PUSH $5~%")))))

(test esrap-data-singleton
      (is (equal
           '(parse::p
             (parse::d
              1))
           (esrap:parse 'parse:str->ast (format nil ".DATA~%~tA 1~%")))))

(test esrap-data-loaded
      (is (equal
           '(parse::p
             (parse::d
              1 2 3 4 5 6 7 8))
           (esrap:parse 'parse:str->ast (format nil ".DATA~%~tB 1 2 3 4 5 6 7 8~%")))))

(test esrap-data-triple
      (is (equal
           '(parse::p
             (parse::d
              5 6 7 8 4 3 5))
           (esrap:parse 'parse:str->ast (format nil ".DATA~%~tC 5 6 7 8~%~tD 4
~tE 3 5~%")))))

(test esrap-data-lazy-spaces
      (is (equal
           '(parse::p
             (parse::d
              5 6 7 8 4 3 5))
           (esrap:parse 'parse:str->ast (format nil "~%~t.DATA~t~%F 5 6 7 8~t~%G 4
H 3 5~%")))))

(test esrap-data-full
      (is (equal
           '(parse::p
             (parse::d
              1 2 3 4 3 0)
             (parse::x
              (parse::i "LOAD" (parse::rr 5) (parse::rr 0) (parse::var "S"))
              (parse::i "LOAD" (parse::rr 10) (parse::rr 0) (parse::var "ARR"))
              (parse::i "LOAD" (parse::rr 6) (parse::rr 0) (parse::var "I"))
              (parse::j "JRL" (parse::rr 0) (parse::l "CMP"))
              (parse::r "ADD" (parse::rr 10) (parse::rr 6) (parse::rr 9))
              (parse::i "ADDI" (parse::rr 6) (parse::rr 6) (parse::imm 1))
              (parse::r "CMP" (parse::rr 6) (parse::rr 5) (parse::rr 0))
              (parse::j "BGT" (parse::rr 0) (parse::l "L"))))
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

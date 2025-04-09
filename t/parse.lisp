(in-package #:rva-tests)

(def-suite parse-tests
  :description "Test functions exported from the parser."
  :in all-tests)

;;; these tests are not exhaustive, and are meant to test basic functionality
;;; under correct circumstances.

(in-suite parse-tests)

(test esrap-register-decimal-ten
      (is (equal (list 'parse::rr 10)
                 (esrap:parse 'parse::register "$10"))))

(test esrap-register-binary-ten
      (is (equal (list 'parse::rr 10)
                 (esrap:parse 'parse::register "$0b1010"))))

(test esrap-register-octal-ten
      (is (equal (list 'parse::rr 10)
                 (esrap:parse 'parse::register "$0o12"))))

(test esrap-register-hex-ten
      (is (equal (list 'parse::rr 10)
                 (esrap:parse 'parse::register "$0xa"))))

(test esrap-r-type-1
      (is (equal '(parse::r "ADD" (parse::rr 5) (parse::rr 8) (parse::rr 1))
                 (esrap:parse 'parse:instr "ADD $1 $5 $8"))))

(test esrap-r-type-2
      (is (equal '(parse::r "NOT" (parse::rr 5) (parse::rr 0) (parse::rr 1))
                 (esrap:parse 'parse:instr "NOT $1 $5"))))

(test esrap-r-type-3
      (is (equal '(parse::r "CMP" (parse::rr 1) (parse::rr 5) (parse::rr 0))
                 (esrap:parse 'parse:instr "CMP $1 $5"))))

;; (test esrap-i-type-1
;;       (is (equal (list 'parse::i "LOAD" (list 'parse::rr 8) (list 'parse::rr 9) (list 'parse::r 1))
;;                  (esrap:parse 'parse:instr "LOAD $8 1($9)"))))

;; (test esrap-i-type-2
;;       (is (equal (list 'parse::i "STORE" (list 'parse::rr 3) (list 'parse::rr 5) (list 'parse::rr 3))
;;                  (esrap:parse 'parse:instr "STORE $5 3($3)"))))

;; (test esrap-i-type-3
;;       (is (equal (list 'parse::i "ORI" (list 'parse::rr 5) (list 'parse::rr 4) (list 'parse::r 2))
;;                  (esrap:parse 'parse:instr "ORI $5 $4 2"))))

(test esrap-j-type-2
      (is (equal '(parse::j "JRL" (parse::rr 0) (parse::l "FOO"))
		 (esrap:parse 'parse:instr "JRL FOO"))))

(test esrap-j-type-3
      (is (equal '(parse::j "PUSH" (parse::rr 1) 0)
		 (esrap:parse 'parse:instr "PUSH $1"))))

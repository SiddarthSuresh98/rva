(in-package #:parse)

(esrap:defrule space
    (+ (or #\space #\tab))
  (:constant nil))

(esrap:defrule newline
    (+ #\newline))

;;; defines rules to parse an integer in various bases

(esrap:defrule binary (and #\0 #\B (+ (or "0" "1")))
  (:lambda (list) (parse-integer (esrap:text (cddr list)) :radix 2)))

(esrap:defrule octal (and #\0 #\O (+ (or (esrap:character-ranges (#\0 #\7)))))
  (:lambda (list) (parse-integer (esrap:text (cddr list)) :radix 8)))

(esrap:defrule decimal (+ (or (esrap:character-ranges (#\0 #\9))))
  (:lambda (list) (parse-integer (esrap:text list) :radix 10)))

(esrap:defrule hex (and #\0 #\X (+ (or (esrap:character-ranges (#\0 #\9))
				       "A" "B" "C" "D" "E" "F")))
  (:lambda (list) (parse-integer (esrap:text (cddr list)) :radix 16)))

(esrap:defrule int (or binary octal hex decimal))

;;; defines rules to parse an operand

(esrap:defrule register (and #\$ (or int reg-id))
  (:lambda (list) (list 'rr (cadr list))))

(esrap:defrule dereference (and (esrap:? (or #\+ #\-)) int #\( register #\))
  (:destructure (s i1 w1 r w2)
    (declare (ignore w1 w2))
    (list r (list 'imm (if (and s (string= s "-")) (- i1) i1)))))

(esrap:defrule immediate int
  (:lambda (i) (list 'imm i)))

;;; defines rules to parse labels

(esrap:defrule label (+ (alphanumericp character))
  (:lambda (list) (list 'l (esrap:text list))))

(esrap:defrule label-decl (and label #\:)
  (:destructure (l w)
    (declare (ignore w))
    l))

;;; defines rules to parse instruction types

(esrap:defrule r-type-1-m (or "ADDV" "SUBV" "MULV" "DIVV" "ADD" "SUB" "MUL"
			      "QUOT" "REM" "SFTR" "SFTL" "AND" "OR" "NOT" "XOR" ))
(esrap:defrule r-type-2-m "NOT")
(esrap:defrule r-type-3-m (or "CMP" "CEV"))
(esrap:defrule i-type-1-m (or "LOADV" "LOAD"))
(esrap:defrule i-type-2-m (or "STOREV" "STORE"))
(esrap:defrule i-type-3-m (or "ADDI" "SUBI" "SFTRI" "SFTLI" "ANDI" "ORI" "XORI"))
(esrap:defrule j-type-1-m (or "JMP" "JAL"))
(esrap:defrule j-type-2-m (or "JRL" "BEQ" "BGT" "BUF" "BOF"))
(esrap:defrule j-type-3-m (or "PUSH" "POP"))

(defmacro defrule-instr (name type-id order &rest destructure-pattern)
  "Defines the boilerplate for a common esrap instruction rule.
NAME is the name of the non-terminal symbol.
TYPE-ID is the symbol which appears as the first element of a successful parse.
ORDER is the order to place the parsed tokens in the resulting list.
DESTRUCTURE-PATTERN is the list of non-terminals on the right side of the grammar rule."
  (let* ((pattern-size (length destructure-pattern))
	 (spaces (mapcar (lambda (x) (read-from-string (format nil "w~A" x))) (util:iota pattern-size)))
	 (vars (mapcar (lambda (x) (read-from-string (format nil "s~A" x))) (util:iota pattern-size))))
    `(esrap:defrule ,name
	 (and ,(read-from-string (format nil "~A-m" name)) ,@(util:riffle (make-list pattern-size :initial-element 'space) destructure-pattern))
       (:destructure (m ,@(util:riffle spaces vars))
	 (declare (ignore ,@spaces))
	 (list ,type-id m ,@(mapcar (lambda (x) (or (nth x vars) ''(rr 0))) order))))))

(defrule-instr r-type-1 'r (1 2 0) register register register)
(defrule-instr r-type-2 'r (1 2 0) register register)
(defrule-instr r-type-3 'r (0 1 2) register register)

(esrap:defrule i-type-1 (and i-type-1-m space register space dereference)
  (:destructure (m w1 s w2 di)
    (declare (ignore w1 w2))
    `(i ,m ,s ,@di)))

(esrap:defrule i-type-2 (and i-type-2-m space register space dereference)
  (:destructure (m w1 s w2 di)
    (declare (ignore w1 w2))
    `(i ,m ,@(util:insert-in-middle di s))))

(defrule-instr i-type-3 'i (0 1 2) register register immediate)
(esrap:defrule j-type-1 (and j-type-1-m space dereference)
  (:destructure (m w di)
    (declare (ignore w))
    `(j ,m ,@di)))

(defrule-instr j-type-2 'j (1 0) label)
(esrap:defrule j-type-3 (and j-type-3-m space register)
  (:destructure (m w r)
    (declare (ignore w))
    `(j ,m ,r (imm 0))))

(esrap:defrule instr (or r-type-1 r-type-2 r-type-3 i-type-1 i-type-2
			 i-type-3 j-type-1 j-type-2 j-type-3 label-decl))

;;; defines rules to parse the .text segment

(esrap:defrule instr-clean (and (esrap:? space) instr newline)
  (:destructure (w1 i w2)
    (declare (ignore nl l))
    i))

(esrap:defrule text (and ".TEXT" newline
			 (* instr-clean))
  (:destructure (txt nl is)
    (declare (ignore txt nl))
    (list 'text is)))

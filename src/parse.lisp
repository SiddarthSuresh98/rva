(in-package #:parse)

(esrap:defrule space
    (+ (or #\Space #\Tab))
  (:constant nil))

;;; defines rules to parse an integer in various bases

(esrap:defrule binary (and #\0 #\b (+ (or "0" "1")))
  (:lambda (list) (parse-integer (esrap:text (cddr list)) :radix 2)))

(esrap:defrule octal (and #\0 #\o (+ (or (esrap:character-ranges (#\0 #\7)))))
  (:lambda (list) (parse-integer (esrap:text (cddr list)) :radix 8)))

(esrap:defrule decimal (+ (or (esrap:character-ranges (#\0 #\9))))
  (:lambda (list) (parse-integer (esrap:text list) :radix 10)))

(esrap:defrule hex (and #\0 #\x (+ (or (esrap:character-ranges (#\0 #\9))
				       "a" "b" "c" "d" "e" "f"
				       "A" "B" "C" "D" "E" "F")))
  (:lambda (list) (parse-integer (esrap:text (cddr list)) :radix 16)))

(esrap:defrule int (or binary octal hex decimal))

;;; defines rules to parse an operand

(esrap:defrule register (and #\$ (or int reg-id))
  (:lambda (list) (list 'rr (cadr list))))

(esrap:defrule dereference (and (esrap:? (or #\+ #\-)) int #\( register #\))
  (:destructure (s i1 w1 r w2)
    (declare (ignore w1 w2))
    (list r (if (and s (string= s "-")) (- i1) i1))))

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
;;(defrule-instr j-type-1 'j (0 1) dereference)
(defrule-instr j-type-2 'j (1 0) label)
(defrule-instr j-type-3 'j (0 1) register)

;; (esrap:defrule i-type-1 (and i-type-1-m space register space dereference)
;;   (:destructure (m w1 s w2 di)
;;     (declare (ignore w1 w2))
;;     `(i ,m ,s ,@di)))
;; (esrap:defrule i-type-2 (and i-type-2-m space register space dereference)
;;   (:destructure (m w1 s w2 di)
;;     (declare (ignore w1 w2))
;;     `(i ,m ,@(util:insert-in-middle di s))))
;; (esrap:defrule i-type-3 (and i-type-3-m space register space register space int)
;;   (:destructure (m w1 s1 w2 s2 w3 i)
;;     (declare (ignore w1 w2 w3))
;;     (list i m s1 s2 i)))
;; (esrap:defrule j-type-1 (and j-type-1-m space dereference)
;;   (:destructure (m w di)
;;     (declare (ignore w))
;;     `(j ,m ,@di)))
;; (esrap:defrule j-type-2 (and j-type-2-m space label)
;;   (:destructure (m w label)
;;     (declare (ignore w))
;;     (list j m "00000" label)))
;; (esrap:defrule j-type-3 (and j-type-3-m space register)
;;   (:destructure (m w r)
;;     (declare (ignore w))
;;     (list j m r "00000")))

(esrap:defrule instr (or r-type-1 r-type-2 r-type-3 j-type-2 j-type-3))


;; (esrap:parse 'register "$3")

;; (esrap:parse 'j-type-1 "JMP 3($3)")
;; (esrap:parse 'j-type-2 "JRL FOO")
;; (esrap:parse 'j-type-3 "PUSH $1")

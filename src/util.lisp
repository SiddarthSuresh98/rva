(in-package #:util)

(defun asm-extension? (file)
  "Returns t if FILE is extended with .asm, nil otherwise."
  (string= (pathname-type file) "asm"))

;; TODO this won't work for negative numbers of odd sizes quite yet.
(defun format-as-binary (num len)
  "Formats NUM as a binary number, and pads to LEN with zeros."
  (declare (type number        num))
  (declare (type (integer 0 *) len))
  (format nil "~V,'0b" len num))

(defun insert-in-middle (list element)
  (append (list (car list)) (list element) (cdr list)))

(defun iota (n)
  "Generates a number sequence from 0 to N."
  (when (> n 0)
    (do ((i 0 (1+ i))
	 (item 0 (1+ item))
	 (result nil (push item result)))
	((= i n) (nreverse result)))))

(defun riffle (lst1 lst2)
  "Given LST1 and LST2, returns a new list which is the an alternative sequence
of the elements from both lists. Returns nil if the lists are not equal size."
  (when (eq (length lst1) (length lst2))
    (loop for l1 in lst1
	  for l2 in lst2
	  append (list l1 l2))))

(defparameter type-r
  '(ADD SUB MUL QUOT REM SFTR SFTL AND OR NOT XOR ADDV SUBV MULV DIVV CMP CEV)
  "R-type instructions.")

(defparameter type-i
  '(LOAD LOADV ADDI SUBI SFTRI SFTLI ANDI ORI XORI STORE STOREV MOV)
  "I-type instructions.")

(defparameter type-j
  '(JMP JRL JAL BEQ BGT BUF BOF PUSH POP)
  "J-type instructions.")

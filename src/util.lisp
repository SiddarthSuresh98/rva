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

(defparameter type-r
  '(ADD SUB MUL QUOT REM SFTR SFTL AND OR NOT XOR ADDV SUBV MULV DIVV CMP CEV)
  "R-type instructions.")

(defparameter type-i
  '(LOAD LOADV ADDI SUBI SFTRI SFTLI ANDI ORI XORI STORE STOREV)
  "I-type instructions.")

(defparameter type-j
  '(JMP JRL JAL BEQ BGT BUF BOF PUSH POP)
  "J-type instructions.")

(defparameter label-loc '()
  "A symbol table mapping label names to line indices.")

(in-package #:util)

(defun asm-extension? (file)
  "Returns t if FILE is extended with .asm, nil otherwise."
  (string= (pathname-type file) "asm"))


(defun format-as-binary (num len)
  "Formats NUM as a binary number, and pads to LEN with zeros."
  (declare (type number        num))
  (declare (type (integer 0 *) len))
  (let ((max-val (1- (expt 2 len))))
    (format nil "~V,'0b" len (logand num max-val))))

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
  "Given LST1 and LST2, returns a new list which is the an alternating sequence
of the elements from both lists. Returns nil if the lists are not equal size."
  (when (eq (length lst1) (length lst2))
    (loop for l1 in lst1
          for l2 in lst2
          append (list l1 l2))))

(defvar variable-table (make-hash-table :test #'equalp))
(defvar label-table (make-hash-table :test #'equalp))

(defun add-variable (name value)
  (if (gethash name variable-table)
      (error "~@<Variable declared twice: ~S.~@:>" name)
      (setf (gethash name variable-table) value)))

(defun add-label (name value)
  (if (gethash name label-table)
      (error "~@<Label declared twice: ~S.~@:>" name)
      (setf (gethash name label-table) value)))

(defun get-variable (name)
  (alexandria:if-let ((value (gethash name variable-table)))
    value
    (progn (maphash #'(lambda (k v) (format t "~A => ~A~%" k v)) variable-table)
	   (error "~@<Variable ~S not declared.~@:>" name))))

(defun get-label (name)
  (alexandria:if-let ((value (gethash name label-table)))
    value
    (error "~@<Label ~S not found.~@:>" name)))

(defvar r-type
  '("ADD" "SUB" "MUL" "QUOT" "REM" "SFTR" "SFTL" "AND" "OR"
    "NOT" "XOR" "ADDV" "SUBV" "MULV" "DIVV" "CMP" "CEV")
  "R-type instructions.")

(defvar i-type
  '("LOAD" "LOADV" "ADDI" "SUBI" "SFTRI" "SFTLI" "ANDI" "ORI" "XORI" "STORE" "STOREV")
  "I-type instructions.")

(defvar j-type
  '("JMP" "JRL" "JAL" "BEQ" "BGT" "BUF" "BOF" "PUSH" "POP")
  "J-type instructions.")

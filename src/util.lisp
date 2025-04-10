(in-package #:util)

(defun asm-extension? (file)
  "Returns t if FILE is extended with .asm, nil otherwise."
  (string= (pathname-type file) "asm"))

(defun fits-in-X-bits (n)
  "Returns the number of bits required to represent N"
  (ceiling (/ (log (ceiling n (log 2))) (log 2))))

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
  "Given LST1 and LST2, returns a new list which is the an alternating sequence
of the elements from both lists. Returns nil if the lists are not equal size."
  (when (eq (length lst1) (length lst2))
    (loop for l1 in lst1
          for l2 in lst2
          append (list l1 l2))))

(defvar variable-table (make-hash-table :test #'equal))
(defvar label-table (make-hash-table :test #'equal))

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
    (error "~@<Variable ~S not declared.~@:>" name)))

(defun get-label (name)
  (alexandria:if-let ((value (gethash name label-table)))
    value
    (error "~@<Label ~S not found.~@:>" name)))

(defmacro generate-type-map (ops)
  "Generates an alist where the key corresponds to an element in
OPS, while the value is the index of that key (padded to the minimum
number of bits required to represent all
concatenated with TYPE."
  `(let ((i 0)
         (opsize (fits-in-X-bits (length ,ops))))
     (mapcar (lambda (x)
               (incf i)
               (cons x (format-as-binary i opsize)))
             ,ops)))

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

(defvar mnemonic-loc
  `(,@(generate-type-map r-type)
    ,@(generate-type-map i-type)
    ,@(generate-type-map j-type))
  "An alist mapping known mnemonics to their binary representation.")

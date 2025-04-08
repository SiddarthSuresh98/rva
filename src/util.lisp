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

(defmacro generate-type-map (type opsize ops)
  "Generates an alist where the key corresponds to an element in
OPS, while the value is the index of that key (padded to OPSIZE)
concatenated with TYPE."
  `(let ((i 0))
     (mapcar (lambda (x)
               (incf i)
               (cons x (concatenate 'string ,type
                                    (format-as-binary i ,opsize))))
             ,ops)))

(defparameter label-loc '()
  "A symbol table mapping label names to line indices.")

(defparameter mnemonic-loc
  `(,@(generate-type-map "00" 5
                         '(ADD SUB MUL QUOT REM SFTR SFTL AND OR NOT
                           XOR ADDV SUBV MULV DIVV CMP CEV))
    ,@(generate-type-map "01" 4
                         '(LOAD LOADV ADDI SUBI SFTRI SFTLI ANDI ORI
                           XORI STORE STOREV))
    ,@(generate-type-map "10" 4
                         '(JMP JRL JAL BEQ BGT BUF BOF PUSH POP)))
  "An alist mapping known mnemonics to their binary representation.")

(in-package #:emit)

(defun fits-in-X-bits (n)
  "Returns the number of bits required to represent N"
  (ceiling (/ (log (ceiling n (log 2))) (log 2))))

(defmacro generate-type-map (ops)
  "Generates an alist where the key corresponds to an element in
OPS, while the value is the index of that key (padded to the minimum
number of bits required to represent all
concatenated with TYPE."
  `(let ((i 0)
         (opsize (fits-in-X-bits (length ,ops))))
     (mapcar (lambda (x)
               (incf i)
               (cons x (util:format-as-binary i opsize)))
             ,ops)))

(defvar mnemonic-loc
  `(,@(generate-type-map util:r-type)
    ,@(generate-type-map util:i-type)
    ,@(generate-type-map util:j-type))
  "An alist mapping known mnemonics to their binary representation.")

(defun lookup-mnemonic (mnemonic)
  (cdr (assoc mnemonic mnemonic-loc :test #'string=)))

(defun p (d x)
  (append x d))

(defun d (&rest lst)
  (mapcar (lambda (x) (util:format-as-binary x 32)) lst))

(defun x (&rest lst)
  lst)

(defun r (mnemonic s1 s2 d)
  (concatenate
   'string (format nil "~10,'0d" 0) d s2 s1 (lookup-mnemonic mnemonic) "00"))

(defun i (mnemonic s d i)
  (concatenate
   'string (util:format-as-binary i 16) d s (lookup-mnemonic mnemonic) "01"))

(defun j (mnemonic b d)
  (concatenate
   'string (util:format-as-binary d 21) b (lookup-mnemonic mnemonic) "10"))

(defun rr (val)
  (if (<= 0 val 23)
      (util:format-as-binary val 5)
      (error (format nil "~a is not a valid register id!~%" val))))

(defun imm (val) val)

(defun l (l s)
  (let ((d (util:get-label l)))
    (- d s)))

(defun var (s)
  (let ((pos (util:get-variable s)))
    (+ pos parse:line-number)))

(defun emit (p)
  (format t "~a~%" p)
  (eval p))

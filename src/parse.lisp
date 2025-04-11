(in-package #:parse)

(defparameter line-number 0
  "The number of real instructions processed up until this point.")
(defparameter var-offset 0
  "The number of variables processed up until this point.")

(esrap:defrule space
    (+ (or #\space #\tab))
  (:constant nil))

(esrap:defrule eol (and (esrap:? space) (esrap:? (and #\; (* (not #\newline)))) #\newline)
  (:constant nil))

(esrap:defrule newline (+ eol)
  (:constant nil))

(esrap:defrule sign (or #\+ #\-))

(esrap:defrule alphanumeric (+ (alphanumericp character))
  (:text t))

;;; defines rules to parse an integer in various bases

(defmacro define-number-rule ())

(esrap:defrule binary-number (and #\0 #\B (+ (or "0" "1")))
  (:lambda (e) (parse-integer (esrap:text (cddr e)) :radix 2)))

(esrap:defrule octal-number (and #\0 #\O (+ (or (esrap:character-ranges (#\0 #\7)))))
  (:lambda (e) (parse-integer (esrap:text (cddr e)) :radix 8)))

(esrap:defrule decimal-number (+ (or (esrap:character-ranges (#\0 #\9))))
  (:lambda (e) (parse-integer (esrap:text e) :radix 10)))

(esrap:defrule hexadecimal-number (and #\0 #\X (+ (or (esrap:character-ranges (#\0 #\9))
                                       "A" "B" "C" "D" "E" "F")))
  (:lambda (e) (parse-integer (esrap:text (cddr e)) :radix 16)))

(esrap:defrule integer (and (esrap:? sign) (or binary-number octal-number
					   hexadecimal-number decimal-number))
  (:destructure (s i)
    (if (and s (string= s "-")) (- i) i)))

;;; defines rules to parse an operand

(esrap:defrule register (and #\$ integer)
  (:function cadr)
  (:lambda (e) (list 'emit::rr e)))

(esrap:defrule variable alphanumeric
  (:lambda (e) (list 'emit::var e)))

(esrap:defrule immediate (or integer variable)
  (:lambda (e) e))

(esrap:defrule dereference (and immediate #\( register #\))
  (:destructure (i1 w1 r w2)
    (declare (ignore w1 w2))
    (list r i1)))

;;; defines rules to parse labels

(esrap:defrule label alphanumeric
  (:lambda (e) (list 'emit::l e line-number)))

(esrap:defrule label-declaration (and alphanumeric #\:)
  (:function car)
  (:lambda (e)
    (util:add-label e line-number)
    nil))

;;; defines rules to parse instruction types

(defun generate-mnemonic (name ops)
  (let ((expr `(or ,@ops)))
    (esrap:add-rule
     name (make-instance 'esrap:rule :expression expr))))

;; define special cases first
(generate-mnemonic 'r-type-1-m '("NOT"))
(generate-mnemonic 'r-type-2-m '("CMP" "CEV"))
(generate-mnemonic 'i-type-1-m '("LOADV" "LOAD"))
(generate-mnemonic 'i-type-2-m '("STOREV" "STORE"))
(generate-mnemonic 'j-type-1-m '("JMP" "JAL"))
(generate-mnemonic 'j-type-2-m '("PUSH" "POP"))

;; we need to reverse to ensure rules like "ADDV" are matched before "ADD"
(generate-mnemonic 'r-type-3-m (reverse util:r-type))
(generate-mnemonic 'i-type-3-m (reverse util:i-type))
(generate-mnemonic 'j-type-3-m (reverse util:j-type))

;; TODO this is pretty gross
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
         (list ,type-id m ,@(mapcar (lambda (x) (or (nth x vars) ''(emit::rr 0))) order))))))

(defrule-instr r-type-1 'emit::r (1 2 0) register register)
(defrule-instr r-type-2 'emit::r (0 1 2) register register)
(defrule-instr r-type-3 'emit::r (1 2 0) register register register)
(defrule-instr i-type-3 'emit::i (1 0 2) register register immediate)
(defrule-instr j-type-3 'emit::j (1 0) label)

(esrap:defrule i-type-1 (and i-type-1-m space register space dereference)
  (:destructure (m w1 s w2 di)
    (declare (ignore w1 w2))
    `(emit::i ,m ,s ,@di)))

(esrap:defrule i-type-2 (and i-type-2-m space register space dereference)
  (:destructure (m w1 s w2 di)
    (declare (ignore w1 w2))
    `(emit::i ,m ,@(util:insert-in-middle di s))))

(esrap:defrule j-type-1 (and j-type-1-m space dereference)
  (:destructure (m w di)
    (declare (ignore w))
    `(emit::j ,m ,@di)))

(esrap:defrule j-type-2 (and j-type-2-m space register)
  (:destructure (m w r)
    (declare (ignore w))
    `(emit::j ,m ,r 0)))

(esrap:defrule instr (or r-type-1 r-type-2 r-type-3 i-type-1 i-type-2
                         i-type-3 j-type-1 j-type-2 j-type-3))

;;; defines rules to parse the .text segment

(esrap:defrule instr-clean (and (esrap:? space) instr newline)
  (:function cadr)
  (:lambda (i) (incf line-number) i))

(esrap:defrule label-clean (and label-declaration newline)
  (:function car))

(esrap:defrule text-line (or instr-clean label-clean))

(esrap:defrule text (and ".TEXT" (esrap:? space) newline (* text-line))
  (:function cadddr)
  (:lambda (e) `(emit::x ,@(remove nil e))))

;;; defines rules to parse the .data segment

(esrap:defrule data-word (and (esrap:? space) integer)
  (:function cadr)
  (:lambda (e)
    (incf var-offset)
    e))

(esrap:defrule var-declaration alphanumeric
  (:lambda (e)
    (util:add-variable e var-offset)
    nil))

(esrap:defrule data-line (and (esrap:? space) var-declaration (+ data-word) newline)
  (:function caddr))

(esrap:defrule data (and ".DATA" (esrap:? space) newline (* data-line))
  (:function cadddr)
  (:lambda (e) `(emit::d ,@(apply #'append e))))

;;; defines rules to parse a program

(esrap:defrule str->ast (and (* (or space newline)) data text)
  (:function cdr)
  (:lambda (e) `(emit::p ,@e)))

;; Assembler for the RISC-V[ECTOR] mini-ISA.
;; Copyright (C) 2025

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

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
  (:lambda (e) (list 'emit::l e)))

(esrap:defrule label+pos alphanumeric
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

;; we need to reverse to ensure rules like "ADDV" are matched before "ADD"
(let* ((lst (reverse util:r-type))
       (type-1 '("NOT"))
       (type-2 '("CMP" "CEV"))
       (type-3 (remove-if (lambda (x) (member x (append type-1 type-2))) lst)))
  (generate-mnemonic 'r-type-1-m type-1)
  (generate-mnemonic 'r-type-2-m type-2)
  (generate-mnemonic 'r-type-3-m type-3))

(let* ((lst (reverse util:i-type))
       (type-1 '("LOADV" "LOAD"))
       (type-2 '("STOREV" "STORE"))
       (type-3 (remove-if (lambda (x) (member x (append type-1 type-2))) lst)))
  (generate-mnemonic 'i-type-1-m type-1)
  (generate-mnemonic 'i-type-2-m type-2)
  (generate-mnemonic 'i-type-3-m type-3))

(let* ((lst (reverse util:j-type))
       (type-1 '("JMP" "JAL"))
       (type-2 '("PUSH" "POP"))
       (type-3 '("RET" "NOP"))
       (type-4 (remove-if (lambda (x) (member x (append type-1 type-2 type-3))) lst)))
  (generate-mnemonic 'j-type-1-m type-1)
  (generate-mnemonic 'j-type-2-m type-2)
  (generate-mnemonic 'j-type-3-m type-3)
  (generate-mnemonic 'j-type-4-m type-4))

;; TODO this is pretty gross
;; while I'm at it, make a macro for the above too
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
(defrule-instr j-type-1 'emit::j (1 0) label)
(defrule-instr j-type-4 'emit::j (1 0) label+pos)

(esrap:defrule i-type-1 (and i-type-1-m space register space dereference)
  (:destructure (m w1 s w2 di)
    (declare (ignore w1 w2))
    `(emit::i ,m ,s ,@di)))

(esrap:defrule i-type-2 (and i-type-2-m space register space dereference)
  (:destructure (m w1 s w2 di)
    (declare (ignore w1 w2))
    `(emit::i ,m ,@(util:insert-in-middle di s))))

(esrap:defrule j-type-2 (and j-type-2-m space register)
  (:destructure (m w r)
    (declare (ignore w))
    `(emit::j ,m ,r 0)))

(esrap:defrule j-type-3 j-type-3-m
  (:lambda (m)
    `(emit::j ,m (emit::rr 0) 0)))

(esrap:defrule instr (or r-type-1 r-type-2 r-type-3 i-type-1 i-type-2
                         i-type-3 j-type-1 j-type-2 j-type-3 j-type-4))

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

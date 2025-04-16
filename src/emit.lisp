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

(in-package #:emit)

(defun fits-in-X-bits (n)
  "Returns the number of bits required to represent N"
  (ceiling (/ (log n) (log 2))))

(defmacro generate-type-map (ops)
  "Generates an alist where the key corresponds to an element in
OPS, while the value is the index of that key (padded to the minimum
number of bits required to represent all
concatenated with TYPE."
  `(let ((i 0)
         (opsize (fits-in-X-bits (length ,ops))))
     (mapcar (lambda (x) (incf i)
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
  (append lst
	  ;; add a halt to the end of the instructions list
	  (list (r "QUOT" (rr 0) (rr 0) (rr 0)))))

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

(defun l (l &optional (pos 0))
  (let ((d (util:get-label l)))
    (- d pos)))

(defun var (s)
  (let ((pos (util:get-variable s)))
    (+ pos parse:line-number 1)))

(defun emit (p)
  (eval p))

(defun ast->str (p)
  (format nil "~a" p))

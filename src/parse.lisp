(in-package #:parse)

(defparameter reg-loc
  '(("zr" . 0) ("lk" . 1)
    ("sp" . 2) ("cc" . 3)
    ("vl" . 4) ("fp" . 5))
  "A symbol table mapping register aliases to identifiers. If you want to add
a new alias, do it here.")

(defparameter label-loc '()
  "A symbol table mapping label names to line indices.")

(define-condition parser-error (error)
  ((message :initarg :message
            :initform nil
            :reader message))
  (:report (lambda (condition stream)
             (format stream "~A" (message condition))))
  (:documentation "Dedicated error for an invalid parse."))

(defun to-register (tokens)
  "Attempts to parse a register from the start of TOKENS. If it is badly formed,
throws a PARSER-ERROR."
  (or (and (equal (car tokens) 'LEX::DOLLAR)
           (cadr tokens)
           (let ((r (cadr tokens)))
             (cond ((stringp r) (cdr (assoc r reg-loc :test #'string=)))
                   ((numberp r) (and (<= 0 r 23) r)))))
      (error 'parser-error
             :message
             (format nil "PARSE failed--Expected register, got ~a.~%"
                     (subseq tokens 0 (min 2 (length tokens)))))))

(defun tokens->ast (program)
  "Given PROGRAM, which is a list of lists of symbols,
filters out the labels and parses."
  ;; TODO add directives
  (let ((program (remove nil (mapcar #'extract-label program)))
        (i 0))
    (mapcar (lambda (l) (extract-instruction l i)) program)))

(let ((i 0))
  (defun extract-label (line)
    "Given a series of tokens LINE, determines if LINE is
in the form STRING {colon}. If it is, then it is treated as a
label, and pushed onto the stack with the line index.

Note that this function is intended to be called using mapcar,
so that labels can be added to a map and otherwise removed from
processing."
    (trivia:match line
      ((list (type string) 'lex::colon)
       (progn (push (cons (read-from-string (car line)) i) label-loc) nil))
      (_ (progn (incf i) line)))))

(defun extract-instruction (line i)
  "Given instruction LINE, determines the expected type format and passes
LINE and the index I to the the respective function."
  (let* ((mnemonic (intern (string-upcase (car line)) :util))
         ;; TODO add pseudo-ops (i.e., nop, leave, ret...)
         ;; should probably be their own extract function
         (type-fn (cond
                    ((member mnemonic util:type-r) #'extract-r-type)
                    ((member mnemonic util:type-i) #'extract-i-type)
                    ((member mnemonic util:type-j) #'extract-j-type))))
    (if type-fn
        (funcall type-fn line i)
        (error 'parser-error
               :message
               (format nil "PARSE failed--~a is not a known keyword.~%" mnemonic)))))

(defun extract-r-type (line i)
  (let ((mnemonic (intern (string-upcase (car line)) :util)))
    (defun die ()
      (error 'parser-error
             :message
             (format nil "PARSE failed---Incorrect number of operands for ~a" mnemonic)))
    (defun eat-registers (registers-so-far lst)
      (if (not (null lst))
          (eat-registers (cons (to-register lst) registers-so-far)
                         (cddr lst))
          (reverse registers-so-far)))
    (let* ((registers (eat-registers '() (cdr line)))
           ;; handle special cases
           (registers (cond ((member mnemonic '(util::CMP util::CEV))
                             (if (= 2 (length registers))
                                 (cons 0 registers)
                                 (die)))
                            ((member mnemonic '(util::NOT))
                             (if (= 2 (length registers))
                                 (append registers (list 0))
                                 (die)))
                            (t (if (= 3 (length registers)) registers (die))))))
      (list :op mnemonic :d (car registers) :s1 (cadr registers) :s2 (caddr registers)))))

(defun extract-i-type (line i)
  line)

(defun extract-j-type (line i)
  line)

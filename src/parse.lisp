helper(in-package #:parse)

(define-condition parser-error (error)
  ((message :initarg :message
            :initform nil
            :reader message))
  (:report (lambda (condition stream)
             (format stream "~A" (message condition))))
  (:documentation "Dedicated error for an invalid parse."))

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
      ((list (and id (type string))
             (satisfies (lambda (x) (equal x 'lex::colon))))
       (progn (push (cons (read-from-string id) i) util:label-loc) nil))
      (_ (progn (incf i) line)))))

(defun extract-instruction (line i)
  "Given instruction LINE, determines the expected type format and passes
LINE and the index I to the the respective function."
  ;; TODO add pseudo-ops (i.e., nop, mov, ...)
  (let* ((type-map '((r-type . extract-r-type)
                     (i-type . extract-i-type)
                     (j-type . extract-j-type)))
         (keyword (car line))
         (type-fn (cdr (assoc keyword type-map))))
    (if type-fn
        (funcall type-fn line i)
        (error 'parser-error
               (format nil "PARSE failed--~a is not a known keyword.~%" (keyword))))))

(defun extract-r-type (line i)
  'r)

(defun extract-i-type (line i)
  'i)

(defun extract-j-type (line i)
  'j)

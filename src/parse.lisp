(in-package #:parse)

(defun tokens->ast (program)
  (let ((program (remove nil (mapcar #'extract-label program))))
    ;; TODO
    program))

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

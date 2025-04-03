(in-package #:lex)

(define-condition invalid-immediate-or-keyword (error)
  ((chr :initarg :chr
        :initform nil
        :reader chr)
   (instance :initarg :instance
             :initform nil
             :reader instance))
  (:report (lambda (condition stream)
             (format stream
                     "Lex failed--encountered ~a while reading ~a."
                     (chr condition) (instance condition))))
  (:documentation "Dedicated error for immediates/keywords which contain
invalid characters."))

(defun file->tokens (file)
  "Opens FILE and parses returns a list of tokens, or
NIL if the file could not be opened."

  (defun read-instr (lst tokens-so-far)
    "Collects tokens in FILE into TOKENS-SO-FAR, splitting on a newline."
    (let ((token (read-token)))
      (cond ((null token) (reverse tokens-so-far))
            ((eq token 'nl)
             (cons (reverse tokens-so-far) (read-instr nil nil)))
            (t (read-instr lst (cons token tokens-so-far))))))

  (and (probe-file file)
       (with-open-file (*standard-input* file :direction :input)
         (remove nil (read-instr '() '())))))

(defun read-token ()
  "Reads *STANDARD-INPUT* and returns a token, or nil if the end
of file has been reached.
Whitespace, commas, colons, and parentheses are token delimiters.
Comments start with a semi-colon ';' and all tokens after are ignored."
  (let ((chr (read-char *standard-input* nil)))
    (cond
      ((null chr) chr)

      ((char= chr #\linefeed) 'nl)

      ((whitespace-char-p chr)
       (read-token))

      ((char= chr #\;)
       (progn (read-line *standard-input* nil)
              'nl))

      ((char= chr #\() 'left-paren)
      ((char= chr #\)) 'right-paren)

      ((char= chr #\:) 'colon)
      ((char= chr #\$) 'dollar)

      ((digit-char-p chr)
       (read-immediate chr))

      ((alpha-char-p chr)
       (read-keyword chr))

      (t (error (format nil "~a is not a valid lexical symbol.~%" chr))))))

(defun read-immediate (chr)
  (defun read-immediate-helper (chrs-so-far)
    (let ((chr (peek-char nil *standard-input* nil)))
      (cond ((and (not (null chr)) (digit-char-p chr))
             (read-immediate-helper (cons (read-char *standard-input* nil) chrs-so-far)))
            ((and (not (null chr)) (alpha-char-p chr))
             (error 'invalid-immediate-or-keyword :chr chr :instance "immediate"))
            (t (reverse chrs-so-far)))))
  (parse-integer (coerce (read-immediate-helper (list chr)) 'string)))

(defun read-keyword (chr)
  (defun read-keyword-helper (chrs-so-far)
    (let ((chr (peek-char nil *standard-input* nil)))
      (cond ((and (not (null chr)) (alpha-char-p chr))
             (read-keyword-helper (cons (read-char *standard-input* nil) chrs-so-far)))
            ((and (not (null chr)) (digit-char-p chr))
             (error 'invalid-immediate-or-keyword :chr chr :instance "keyword"))
            (t (reverse chrs-so-far)))))
  (coerce (read-keyword-helper (list chr)) 'string))

(defun whitespace-char-p (x)
  (or (char= #\space x)
      (not (graphic-char-p x))))

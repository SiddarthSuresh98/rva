(in-package #:lex)

(defun file->tokens (file)
  "Opens FILE and parses returns a list of tokens, or
NIL if the file could not be opened."

  (defun read-tokens (tokens-so-far)
    "Collects tokens in FILE into TOKENS-SO-FAR."
    (let ((token (read-token)))
      (if token
          (read-tokens (cons token tokens-so-far))
          (reverse tokens-so-far))))

  (and (probe-file file)
       (with-open-file (*standard-input* file :direction :input)
         (read-tokens '()))))

(defun read-token ()
  "Reads *STANDARD-INPUT* and returns a token, or nil if the end
of file has been reached.
Whitespace, commas, colons, and parentheses are token delimiters.
Comments start with a semi-colon ';' and all tokens after are ignored."
  (let ((chr (read-char *standard-input* nil)))
    (cond
      ((null chr) chr)
      ((whitespace-char-p chr)
       (read-token))

      ((char= chr #\;)
       (progn (read-line *standard-input* nil)
              (read-token)))

      ((char= chr #\() 'left-paren)
      ((char= chr #\)) 'right-paren)

      ((digit-char-p chr)
       (read-immediate chr))

      ((alpha-char-p chr)
       (read-identifier chr))

      (t (error (format nil "~a is not a valid lexical symbol.~%" chr))))))

(defun read-immediate (chr)
  'immediate)

(defun read-identifier (chr)
  'id)

(defun whitespace-char-p (x)
  (or (char= #\space x)
      (not (graphic-char-p x))))

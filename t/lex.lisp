(in-package #:rva-tests)

(defmacro read-this (str &body body)
  `(let ((*standard-input* (make-string-input-stream ,str)))
     ,@body))

(def-suite lex-tests
  :description "Test functions exported from the lexer."
  :in all-tests)

(in-suite lex-tests)

(test read-token-reads-eof
      (read-this ""
                 (is (not (lex:read-token)))))

(test read-token-reads-nl
      (read-this "
"
                 (is (eq (lex:read-token) 'lex::nl))))

(test read-token-reads-left-paren
      (read-this "."
                 (is (eq (lex:read-token) 'lex::period))))

(test read-token-reads-left-paren
      (read-this "("
                 (is (eq (lex:read-token) 'lex::left-paren))))

(test read-token-reads-right-paren
      (read-this ")"
                 (is (eq (lex:read-token) 'lex::right-paren))))

(test read-token-reads-left-paren
      (read-this "$"
                 (is (eq (lex:read-token) 'lex::dollar))))

(test read-token-reads-plus
      (read-this "+"
                 (is (eq (lex:read-token) 'lex::plus))))

(test read-token-reads-minus
      (read-this "-"
                 (is (eq (lex:read-token) 'lex::minus))))

(test read-token-ignores-space
      (read-this " ("
                 (is (eq (lex:read-token) 'lex::left-paren))))

(test read-token-ignores-tab
      (read-this "      ("
                 (is (eq (lex:read-token) 'lex::left-paren))))

(test read-token-ignores-comment
      (read-this "; this is a comment
("
                 (is (eq (lex:read-token) 'lex::nl))))

(test read-token-immediate-zero
      (read-this "0"
                 (is (= (lex:read-token) 0))))

(test read-token-immediate-all-digits
      (read-this "123456789"
                 (is (= (lex:read-token) 123456789))))

(test read-token-immediate-binary
      (read-this "0b00101010"
                 (is (= (lex:read-token) 42))))

(test read-token-immediate-octal
      (read-this "0o052"
                 (is (= (lex:read-token) 42))))

(test read-token-immediate-hexadecimal
      (read-this "0x200"
                 (is (= (lex:read-token) 512))))

(test read-token-immediate-invalid-immediate
      (handler-case
          (progn (read-this "0v0" (lex:read-token))
                 (fail))
        (lex:lexer-error ())))

;; do we want a custom error for this too?
(test read-token-immediate-radix
      (handler-case
          (progn (read-this "0x" (lex:read-token))
                 (fail))
        (sb-int:simple-parse-error ())))

(test read-token-keyword-single
      (read-this "a"
                 (is (string= (lex:read-token) "a"))))

(test read-token-keyword-add
      (read-this "addi"
                 (is (string= (lex:read-token) "addi"))))

(test read-token-immediate-invalid-keyword
      (handler-case
          (progn (read-this "sub0" (lex:read-token))
                 (fail))
        (lex:lexer-error ())))

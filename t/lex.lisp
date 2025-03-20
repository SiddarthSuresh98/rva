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

(test read-token-reads-left-paren
      (read-this "("
                 (is (eq (lex:read-token) 'lex::left-paren))))

(test read-token-reads-right-paren
      (read-this ")"
                 (is (eq (lex:read-token) 'lex::right-paren))))

(test read-token-ignores-space
      (read-this " ("
                 (is (eq (lex:read-token) 'lex::left-paren))))

(test read-token-ignores-tab
      (read-this "      ("
                 (is (eq (lex:read-token) 'lex::left-paren))))

(test read-token-ignores-newline
      (read-this "
("
                 (is (eq (lex:read-token) 'lex::left-paren))))

(test read-token-ignores-comment
      (read-this "; this is a comment
("
                 (is (eq (lex:read-token) 'lex::left-paren))))

(test read-token-ignores-comment-eof
      (read-this ";"
                 (is (not (lex:read-token)))))

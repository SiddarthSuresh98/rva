(in-package #:rva-tests)

(def-suite parse-tests
  :description "Test functions exported from the parser."
  :in all-tests)

(in-suite parse-tests)

(test extract-label-is-a-label
      (is (not (parse:extract-label '("LOOP" lex::colon)))))

(test extract-label-not-a-label-one
      (let ((lst '("NICE" "TRY")))
        (is (equal lst
                   (parse:extract-label lst)))))

(test extract-label-not-a-label-two
      (let ((lst '("LOOP" lex::colon lex::colon)))
        (is (equal lst
                   (parse:extract-label lst)))))

(test extract-line-invalid-type
      (handler-case
          (progn (parse:tokens->ast '(("foo" LEX::DOLLAR)))
                 (fail))
        (lex:parser-error ())))

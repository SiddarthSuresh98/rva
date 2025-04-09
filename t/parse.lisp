(in-package #:rva-tests)

(defmacro expect-parse-error (body)
  `(handler-case
       (progn ,body
              (fail))
     (parse::parser-error ())))

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
      (expect-parse-error (parse:tokens->ast '(("foo" lex::dollar)))))

(test to-register-nil
      (expect-parse-error (parse:to-register '())))

(test to-register-singleton
      (expect-parse-error (parse:to-register '(lex::dollar))))

(test to-register-zero
      (is (= 0 (parse:to-register '(lex::dollar 0)))))

(test to-register-one
      (is (= 1 (parse:to-register '(lex::dollar 1)))))

(test to-register-twenty-three
      (is (= 23 (parse:to-register '(lex::dollar 23)))))

(test to-register-zero-named
      (is (= 0 (parse:to-register '(lex::dollar "zr")))))

(test to-register-twenty-four
      (expect-parse-error (parse:to-register '(lex::dollar 24))))

(test to-register-negative-one
      (expect-parse-error (parse:to-register '(lex::dollar -1))))

(test extract-r-type-no-registers
      (expect-parse-error (parse:extract-r-type '("add") 0)))

(test extract-r-type-two-registers
      (expect-parse-error (parse:extract-r-type '("add" lex::dollar 2 lex::dollar 3) 0)))

(test extract-r-type-cmp-three-registers
      (expect-parse-error (parse:extract-r-type '("cmp" lex::dollar 2
                                                  lex::dollar 3 lex::dollar 4) 0)))

(test extract-r-type-simple-add
      (is (equal '(:op util::ADD :d 2 :s1 3 :s2 4)
                 (parse:extract-r-type '("add" lex::dollar 2
                                         lex::dollar 3 lex::dollar 4) 0))))

(test extract-r-type-simple-not
      (is (equal '(:op util::NOT :d 2 :s1 3 :s2 0)
                 (parse:extract-r-type '("not" lex::dollar 2 lex::dollar 3) 0))))

(test extract-r-type-simple-cmp
      (is (equal '(:op util::CMP :d 0 :s1 2 :s2 3)
                 (parse:extract-r-type '("cmp" lex::dollar 2 lex::dollar 3) 0))))

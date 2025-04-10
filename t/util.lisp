(in-package #:rva-tests)

(def-suite util-tests
  :description "Test functions designated as miscellaneous."
  :in all-tests)

(in-suite util-tests)

(test asm-extension?-returns-false-obvious-case
      (is (not (util:asm-extension? "quux.c"))))

(test asm-extension?-returns-false-pattern-match-case
      (is (not (util:asm-extension? "quux.asm.c"))))

(test asm-extension?-returns-true-obvious-case
      (is (util:asm-extension? "quux.asm")))

(test format-as-binary-unsigned-no-pad
      (is (string= "0"
		   (util:format-as-binary 0 0))))

(test format-as-binary-unsigned-no-pad-fourty-two
      (is (string= "101010"
		   (util:format-as-binary 42 6))))

(test format-as-binary-unsigned-pad-fourty-two
      (is (string= "0000101010"
		   (util:format-as-binary 42 10))))

(test format-as-binary-overflow
      (is (string= "10"
		   (util:format-as-binary 10 2))))

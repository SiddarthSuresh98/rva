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

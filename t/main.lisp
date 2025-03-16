(in-package #:rva-tests)

(def-suite all-tests
  :description "The master suite of rva tests.")

(in-suite all-tests)

(defun test-rva ()
  (run! 'all-tests))

(test dummy-tests
  "Just a placeholder."
  (is (listp (list 1 2)))
  (is (= 5 (+ 2 3))))

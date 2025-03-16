(in-package #:rva-tests)

(def-suite all-tests
  :description "The master suite of rva tests.")

(in-suite all-tests)

(defun test-rva ()
  (run! 'all-tests))

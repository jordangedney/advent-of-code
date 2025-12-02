(defpackage advent/tests/main
  (:use :cl
        :advent
        :rove))
(in-package :advent/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :advent)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))

(defpackage cl-salesforce-bulk/tests/main
  (:use :cl
        :cl-salesforce-bulk
        :rove))
(in-package :cl-salesforce-bulk/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-salesforce-bulk)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))

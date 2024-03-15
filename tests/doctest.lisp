(defpackage :sijo-doctest/tests
  (:use #:cl #:lisp-unit2)
  (:local-nicknames (#:doctest #:sijo-doctest)))

(in-package :sijo-doctest/tests)

(defun assert-doctest (expected documentation &key (expected-output ""))
  (let* ((*standard-output* (make-string-output-stream))
         (result (multiple-value-list (assert-equalp expected (doctest:test documentation)))))
    (let* ((actual-output (get-output-stream-string *standard-output*))
           (expected-output (if (string-equal "" actual-output)
                                expected-output
                                ;; The output always ends in a newline, so we
                                ;; inject it to simplify the tests
                                (format nil "~a~%" expected-output))))
      (assert-equalp expected-output actual-output))
    (values (first result) (second result))))

(defun test-parse-function-lambda-expression (function &key
                                                         (expected-parameters nil)
                                                         (expected-docstring nil)
                                                         (expected-declare nil)
                                                         (expected-body nil))
  (multiple-value-bind (name parameters docstring declare body) (doctest::parse-function-lambda-expression function)
    (assert-true name)
    (assert-equalp expected-parameters parameters)
    (assert-equalp expected-docstring docstring)
    (assert-equalp expected-declare declare)
    (assert-equalp expected-body body)))

(define-test parse-function-lambda-expression ()
  (test-parse-function-lambda-expression (lambda ()))
  (test-parse-function-lambda-expression (lambda (a b))
                                         :expected-parameters '(a b))
  (test-parse-function-lambda-expression (lambda () "body")
                                         :expected-body '("body"))
  (test-parse-function-lambda-expression (lambda () (declare) "body")
                                         :expected-declare '(declare)
                                         :expected-body '("body"))
  (test-parse-function-lambda-expression (lambda () "docstring" "body")
                                         :expected-docstring "docstring"
                                         :expected-body '("body"))
  (test-parse-function-lambda-expression (lambda () "docstring" (declare) "body")
                                         :expected-docstring "docstring"
                                         :expected-declare '(declare)
                                         :expected-body '("body"))
  (test-parse-function-lambda-expression (lambda () (declare))
                                         :expected-declare '(declare))
  (test-parse-function-lambda-expression (lambda () "docstring" (declare))
                                         :expected-docstring "docstring"
                                         :expected-declare '(declare)))

(define-test doctest ()
  ;; Test the documentation for the library itself
  (multiple-value-bind (failed passed) (doctest:test #'doctest:test)
    (assert-eql 0 failed)
    (assert-true (> passed 0)))
  ;; No tests reports 0/0
  (assert-doctest (values 0 0) nil)
  (assert-doctest (values 0 0) "")
  (assert-doctest (values 0 0) ">>> t nil")
  (assert-doctest (values 0 0) "> t nil")
  ;; Single pass
  (assert-doctest (values 0 1) ">> t t")
  ;; Single fail
  (assert-doctest (values 1 0) ">> t nil"
                  :expected-output "[1] T returned T, expected NIL.")
  ;; Error
  (assert-doctest (values 0 1) ">> (error 'foo) 'error")
  ;; Catches subtypes
  (assert-doctest (values 0 1) ">> (error 'foo) 'condition")
  ;; Ok output, ok result
  (assert-doctest (values 0 1) ">> (format t \"foo\") -> \"foo\" nil")
  ;; Ok output, fail result
  (assert-doctest (values 1 0) ">> (format t \"foo\") -> \"foo\" t"
                  :expected-output "[1] (FORMAT T foo) returned NIL, expected T.")
  ;; Fails output, ok result
  (assert-doctest (values 1 0) ">> (format t \"foo\") -> \"bar\" nil"
                  :expected-output "[1] (FORMAT T foo) printed \"foo\", expected \"bar\".")
  ;; Fails output, fail result
  (assert-doctest (values 1 0) ">> (format t \"foo\") -> \"bar\" t"
                  :expected-output "[1] (FORMAT T foo) printed \"foo\", expected \"bar\".")
  ;; Output can test using symbols
  (assert-doctest (values 0 1) ">> (format t \"foo\") -> |foo| nil")
  ;; Output is enumerated
  (assert-doctest (values 2 1) ">> t nil >> t t >> nil t"
                  :expected-output "[1] T returned T, expected NIL.
[3] NIL returned NIL, expected T.")
  ;; Test variable
  (defparameter test-variable :variable)
  (setf (documentation 'test-variable 'variable) ">> sijo-doctest/tests::test-variable :variable")
  (assert-doctest (values 0 1) 'test-variable)
  ;; Test function
  (defun test-function () :function)
  (setf (documentation 'test-function 'function) ">> (sijo-doctest/tests::test-function) :function")
  (assert-doctest (values 0 1) 'test-function)
  ;; Test macro
  ;; The following fails on ccl. Maybe a bug in ccl?
  #-ccl
  (progn
    (defmacro test-macro () :macro)
    (setf (documentation 'test-macro 'function) ">> (sijo-doctest/tests::test-macro) :macro")
    (assert-doctest (values 0 1) 'test-macro)))

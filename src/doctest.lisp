;;; Doctests for Common Lisp.
;;; Copyright (C) 2009 - 2013 Johan Lindberg, Pulp Software
;;; Copyright (C) 2023 - 2024 Simen Endsjø <simen.endsjo@gmail.com>

;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.

;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(defpackage :sijo-doctest
  (:use #:cl)
  (:export #:test
           #:test-docstring
           #:test-function
           #:test-macro
           #:test-file
           #:test-package))

(in-package :sijo-doctest)

(defun whitespace-p (c)
  "Returns T if <c> is a whitespace character, otherwise NIL."
  (or (eql #\Space c)
      (eql #\Tab c)
      (eql #\Newline c)))

(defun remove-ws (string)
  "Return <string> (as a string) with *all* whitespace characters removed."
  (if (stringp string)
      (remove-if #'whitespace-p (copy-seq string))
      (remove-if #'whitespace-p (copy-seq (string string)))))

(defun string-equal-ignore-ws (string1 string2)
  (string-equal (remove-ws string1) (remove-ws string2)))

(defun run-doctest (test-form expected-result expected-output output count)
  (let* ((test-form-signaled-condition 'NIL)
         (actual-output (make-array '(0) :element-type 'base-char
                                         :fill-pointer 0
                                         :adjustable t))
         (actual-result (multiple-value-list
                         (handler-case
                             (with-output-to-string
                                 (*standard-output* actual-output)
                               (eval test-form))
                           (error (co)
                             (progn
                               (setf test-form-signaled-condition t)
                               co)))))
         (expected-result (multiple-value-list (eval expected-result)))
         (expected-output-matches-actual-output
           (if expected-output
               (string-equal-ignore-ws actual-output expected-output)
               T))
         (result T))
    (if test-form-signaled-condition
        (when (not (subtypep (type-of (car actual-result))
                             (car expected-result)))
          (unless (subtypep (type-of (car actual-result)) 'warning)
            (setf result 'NIL)
            (format output "~&[~A] ~A signaled a ~A: ~A, expected ~A.~%"
                    count test-form
                    (type-of (car actual-result)) (car actual-result)
                    (car expected-result))))
        (unless (and (equalp actual-result expected-result)
                     expected-output-matches-actual-output)
          (setf result 'NIL)
          (if expected-output-matches-actual-output
              (format output "~&[~A] ~A returned~{ ~A~}, expected~{ ~A~}.~%"
                      count test-form
                      actual-result
                      expected-result)
              (format output "~&[~A] ~A printed \"~A\", expected \"~A\".~%"
                      count test-form
                      actual-output
                      expected-output))))
    result))

(defun run-doctests (docstring output)
  "Run-doctests is used by the test functions to perform the actual work.
   It returns the number of tests failed and passed and prints to <output>."
  (let ((tests-failed 0)
        (tests-passed 0)
        (count 0))
    (when (and docstring (not (eq :eof (peek-char nil docstring nil :eof))))
      (do ((c (read-char docstring)
              (read-char docstring nil 'EOF)))
          ((eq c 'EOF))
        (when (and (equal #\> c)
                   (equal #\> (read-char docstring))
                   (whitespace-p (peek-char nil docstring)))
          (let ((test-form (read docstring))
                (expected-result (list (read docstring)))
                (expected-output 'NIL))
            (when (and (symbolp (car expected-result))
                       (equal (string (car expected-result)) "->"))
              (setf expected-output (read docstring))
              (setf expected-result (list (read docstring))))

            (if (run-doctest test-form
                             (car expected-result)
                             expected-output
                             output
                             (incf count))
                (incf tests-passed)
                (incf tests-failed))))))

    (values tests-failed tests-passed)))

(defun test (thing &key (output t))
  "Test extracts and tests code snippets embedded in the documentation string
   of <thing>. It returns the number of tests failed and passed and prints a
   description to <output>.

   In order to have a code snippet evaluated as a doctest it must be preceded by
   two '>' characters followed by whitespace. That combination will cause the
   next form to be read and evaluated, and the next or the two next forms after
   that to be read (but not evaluated).

   Here is the simplest possible example:

   >> 1 ; NOTE! You can use comments to clarify!
   1

   If you expect more than one value you should use the values function in the
   expected results.

   >> (values 1 2)
   (values 1 2)

   Newlines and other whitespace (including comments) doesn't particularly
   matter. We could just as well have written >> (values 1
   2) (values 1 2) instead.

   If you test a thing that doesn't have a documentation string, test will
   return (values 0 0).
   ECL NOTE: I pass an empty string here as redefining a function in ECL won't
   remove the old docstring unless a docstring is explicitly passed.

   >> (defun sqr (x)
        \"\"
        (* x x))
   'SQR
   >> (sijo-doctest::test #'sqr)
   (values 0 0)

   If you need to test that a function signals a condition for certain inputs
   you can use the name of the condition as the expected return value.

   >> (sqr 'x)
   'TYPE-ERROR

   If we add a documentation string for sqr with a doctest, we can verify that
   tests can fail as well.

   >> (defun sqr (x)
        \"Returns <x> squared.

          This test will fail:
          >> (sqr 3) 3\"
        (* x x))
   'SQR

   Testing sqr with test should now return 1 failed and 0 passed.

   >> (sijo-doctest::test #'sqr)
   (values 1 0)

   If you need to test the output of a function you can add an expected output
   form (written as -> <expected-output>) *between* the function call and the
   return value. Expected output must be one form so you should either use a
   string or wrap it in '|' characters.

   >> (defun sqr (x)
        \"Prints <x> * <x> = <x*x> to standard output and returns NIL.

          This test will pass,

          >> (sqr 2)
          -> |2 * 2 = 4|
          NIL

          as will this, because it ignores the output.

          >> (sqr 2)
          NIL

          Perhaps surprisingly, this will pass as well,

          >> (sqr 2) -> |2*2=4| NIL

          the reason it passes even though it doesn't exactly match the
          actual output is because the comparison is done after all
          whitespace characters are removed.

          This test will fail because expected output doesn't match the
          actual output.

          >> (sqr 2)
          -> |Blah blah blah|
          NIL\"
        (format t \"~A * ~A = ~A\" x x (* x x)))
   'SQR

   Testing sqr with test should now return 1 failed and 2 passed. It should
   also inform us that:

   (SQR 2) printed \"2 * 2 = 4\", expected \"Blah blah blah\".
   Results for SQR (FUNCTION): 1 of 4 failed.

   NOTE! Whitespace is ignored when output is compared.

   >> (sijo-doctest::test #'sqr :output T)
   -> |[4] (SQR 2) printed \"2 * 2 = 4\", expected \"Blah blah blah\".
       Results for SQR (FUNCTION): 1 of 4 failed.|
   (values 1 3)"
  (cond ((null thing)
         (values 0 0))
        ((stringp thing)
         (test-docstring thing :output output))
        ((functionp thing)
         (test-function thing :output output))
        ((pathnamep thing)
         (test-file thing :output output))
        ((and (symbolp thing)
              (macro-function thing))
         (test-macro thing :output output))
        ((packagep thing)
         (test-package thing :output output))
        (t
         (error "~&No suitable testing-function available for ~A~%" thing))))

(defun test-docstring (documentation &key (output t))
  (with-input-from-string (docstring documentation)
    (run-doctests docstring output)))

(defun extract-function-documentation-and-name (function)
  ;; ABCL doesn't give documentation for (documentation function 'function) for all expressions.
  ;; We try function-lambda-expression too
  (multiple-value-bind (lambda-expression closure-p name) (function-lambda-expression function)
    (declare (ignore closure-p))
    (values (or (documentation function 'function) (third lambda-expression))
            (symbol-name name))))

(defun test-function (function &key (output t))
  "Test-function extracts and tests code snippets in <function>'s documentation
   string. It returns the number of tests failed and passed and prints a
   description to <output>.

   See also the documentation string for test."
  (multiple-value-bind (documentation function-name) (extract-function-documentation-and-name function)
    (if documentation
          (multiple-value-bind (tests-failed tests-passed)
              (test-docstring documentation :output output)
            (print-results function-name 'function output tests-failed tests-passed))
          (values 0 0))))

(defun test-macro (macro &key (output t))
  "Test-macro extracts and tests code snippets in <macro>'s documentation string.
   It returns the number of tests failed and passed and prints a description to
   <output>.

   See also the documentation string for test."
  (if (documentation macro 'function)
      (let ((macro-name (third (multiple-value-list (function-lambda-expression (macro-function macro))))))
        (multiple-value-bind (tests-failed tests-passed)
            (test-docstring (documentation macro 'function) :output output)
          (print-results macro-name 'macro output tests-failed tests-passed)))
      (values 0 0)))


(defun test-file (filename &key (output t))
  "Test-file extracts and tests code snippets in the contents of <filename>. It
   returns the number of tests failed and passed and prints a description to
   <output>.

   See also the documentation string for test."
  (multiple-value-bind (tests-failed tests-passed)
      (with-open-file (docstring filename :direction :input)
        (test-docstrting docstring :output output))
    (print-results filename 'file output tests-failed tests-passed)))

(defun test-package (package &key (output t))
  (let ((total-failed 0)
        (total-passed 0))
    (let ((*package* (find-package package)))
      (do-symbols (symbol (find-package package))
        (when (and (eq *package* (symbol-package symbol))
                   (fboundp symbol))
          (multiple-value-bind (tests-failed tests-passed) (test-function (symbol-function symbol) :output output)
            (incf total-failed tests-failed)
            (incf total-passed tests-passed)))))))

(defun print-results (test-name test-type output tests-failed tests-passed)
  (when (> tests-failed 0)
    (format output "~&Results for ~A (~A): ~D of ~D failed.~%"
            test-name test-type
            tests-failed (+ tests-failed tests-passed)))
  (values tests-failed tests-passed))

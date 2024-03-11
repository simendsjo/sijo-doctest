(defsystem :sijo-doctest
  :in-order-to ((test-op (test-op :sijo-doctest/tests)))
  :description "Doctests for Common Lisp"
  :version "0.3.1"
  :author "Johan Lindberg (Pulp Software) <johan@pulp.se>, Simen Endsjø <simendsjo@gmail.com>"
  :licence "GPL"
  :serial t
  :pathname "src/"
  :components ((:file "doctest")))

(defsystem :sijo-doctest/tests
  :depends-on (#:sijo-doctest
               #:lisp-unit2)
  :perform (test-op (o c)
                    (eval (read-from-string "
                        (lisp-unit2:with-summary ()
                            (lisp-unit2:run-tests
                                :package :sijo-doctest/tests
                                :name :sijo-doctest))")))
  :pathname "tests/"
  :components ((:file "doctest")))

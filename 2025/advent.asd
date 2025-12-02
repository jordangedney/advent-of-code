(defsystem "advent"
  :version "0.0.1"
  :author ""
  :license ""
  :depends-on (:clump
               :cl-syntax
               :alexandria)
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "advent/tests"))))

(defsystem "advent/tests"
  :author ""
  :license ""
  :depends-on ("advent"
               "rove"
               :clump
               :cl-syntax)
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for advent"
  :perform (test-op (op c) (symbol-call :rove :run c)))

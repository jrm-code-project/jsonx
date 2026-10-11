(defsystem "jsonx-test"
  :description "FiveAM tests for jsonx"
  :author "Joe Marshall"
  :license "MIT"
  :depends-on ("jsonx" "fiveam" "cl-json" "alexandria" "quri")
  :components ((:file "jsonx-test"))
  :perform (test-op (op c)
             (declare (ignore op c))
             (unless (uiop:symbol-call :fiveam :run! (uiop:find-symbol* :jsonx-suite :jsonx-test))
               (error "jsonx tests failed"))))

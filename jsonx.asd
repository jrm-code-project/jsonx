(defsystem "jsonx"
  :description "Extension to cl-json"
  :author "Joe Marshall"
  :license "MIT"
  :depends-on ("alexandria" "cl-json" "quri")
  :components ((:file "jsonx" :depends-on ("package"))
               (:file "package")))

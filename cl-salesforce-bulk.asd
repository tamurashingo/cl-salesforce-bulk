(defsystem "cl-salesforce-bulk"
  :version "0.0.1-SNAPSHOT"
  :author "tamura shingo"
  :license "MIT"
  :depends-on (:cl-ppcre
               :cxml
               :dexador
               :xpath)
  :components ((:module "src"
                :components
                ((:file "main")
                 (:file "connection")
                 (:file "job" :depends-on ("connection")))))
  :description "salesforce bulk api client"
  :in-order-to ((test-op (test-op "cl-salesforce-bulk/tests"))))

(defsystem "cl-salesforce-bulk/tests"
  :author "tamura shingo"
  :license "MIT"
  :depends-on ("cl-salesforce-bulk"
               "mockingbird"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main")
                 (:file "connection"))))
  :description "Test system for cl-salesforce-bulk"
  :perform (test-op (op c) (symbol-call :rove :run c)))

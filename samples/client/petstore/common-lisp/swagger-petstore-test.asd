#|
  This file is a part of swagger-petstore project.
  Copyright (c) 2019 
|#

(defsystem "swagger-petstore-test"
  :defsystem-depends-on ("prove-asdf")
  :author ""
  :license ""
  :depends-on ("swagger-petstore"
               "prove")
  :components ((:module "tests"
                :components
                ((:module "api"
                  :components (
                  (:file "pet")
                  (:file "store")
                  (:file "user")
                  )))))
  :description "Test system for swagger-petstore"

  :perform (test-op (op c) (symbol-call :prove-asdf :run-test-system c)))

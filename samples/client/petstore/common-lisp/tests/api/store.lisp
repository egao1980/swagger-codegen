(in-package :cl-user)
(defpackage swagger-petstore.test/store
  (:use :cl
        :swagger-petstore.core
        :swagger-petstore.api/store
        :prove))

(in-package :swagger-petstore.test/store)

;; NOTE: To run this test file, execute `(asdf:test-system :swagger-petstore)' in your Lisp.

(plan nil)



(finalize)

(in-package :cl-user)
(defpackage swagger-petstore.test/pet
  (:use :cl
        :swagger-petstore.core
        :swagger-petstore.api/pet
        :prove))

(in-package :swagger-petstore.test/pet)

;; NOTE: To run this test file, execute `(asdf:test-system :swagger-petstore)' in your Lisp.

(plan nil)



(finalize)

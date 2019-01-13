(in-package :cl-user)
(defpackage swagger-petstore.test/user
  (:use :cl
        :swagger-petstore.core
        :swagger-petstore.api/user
        :prove))

(in-package :swagger-petstore.test/user)

;; NOTE: To run this test file, execute `(asdf:test-system :swagger-petstore)' in your Lisp.

(plan nil)



(finalize)

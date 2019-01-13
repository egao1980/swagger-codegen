#|
  This file is a part of swagger-petstore project.
  Copyright (c) 2019 
|#

#|
  This is a sample server Petstore server.  You can find out more about Swagger at [http://swagger.io](http://swagger.io) or on [irc.freenode.net, #swagger](http://swagger.io/irc/).  For this sample, you can use the api key &#x60;special-key&#x60; to test the authorization filters.

  Author: 
|#

(defsystem "swagger-petstore"
  :version "1.0.0"
  :author ""
  :license "Apache-2.0"
  :depends-on ("drakma"
               "cl-json"
               "cl-ppcre")
  :components ((:module "src"
                :components
                ((:file "core")
                 (:file "swagger-petstore" :depends-on ("core" "api"))
                 (:module "api"
                  :depends-on ("core")
                  :components (
                   (:file "pet")
                   (:file "store")
                   (:file "user")
                   )))))
  :description "This is a sample server Petstore server.  You can find out more about Swagger at [http://swagger.io](http://swagger.io) or on [irc.freenode.net, #swagger](http://swagger.io/irc/).  For this sample, you can use the api key &#x60;special-key&#x60; to test the authorization filters."
  :long-description
  #.(read-file-string
     (subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op "swagger-petstore-test"))))

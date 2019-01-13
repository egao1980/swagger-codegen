(in-package :cl-user)
(defpackage swagger-petstore
  (:use
   :cl
   :swagger-petstore.core)

  (:import-from :swagger-petstore.api/pet
                :add-pet
                :delete-pet
                :find-pets-by-status
                :find-pets-by-tags
                :get-pet-by-id
                :update-pet
                :update-pet-with-form
                :upload-file)
  (:import-from :swagger-petstore.api/store
                :delete-order
                :get-inventory
                :get-order-by-id
                :place-order)
  (:import-from :swagger-petstore.api/user
                :create-user
                :create-users-with-array-input
                :create-users-with-list-input
                :delete-user
                :get-user-by-name
                :login-user
                :logout-user
                :update-user)

  (:export 
   :add-pet
   :delete-pet
   :find-pets-by-status
   :find-pets-by-tags
   :get-pet-by-id
   :update-pet
   :update-pet-with-form
   :upload-file
   :delete-order
   :get-inventory
   :get-order-by-id
   :place-order
   :create-user
   :create-users-with-array-input
   :create-users-with-list-input
   :delete-user
   :get-user-by-name
   :login-user
   :logout-user
   :update-user))
(in-package :swagger-petstore)

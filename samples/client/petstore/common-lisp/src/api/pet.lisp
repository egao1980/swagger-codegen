(in-package :cl-user)
(defpackage swagger-petstore.api/pet
  (:use :cl
        :swagger-petstore.core))
(in-package :swagger-petstore.api/pet)


(defun add-pet (body)
  "Add a new pet to the store
   "
  (call-api "/pet"
            :post
            :path-params   `()
            :header-params `()
            :query-params  `()
            :form-params   `()
            :body-param    body
            :content-types '("application/json" "application/xml")
            :accepts       '("application/xml" "application/json")
            :auth-names    '("petstore_auth")))

(defun delete-pet (pet-id  &key api-key)
  "Deletes a pet
   "
  (call-api "/pet/{petId}"
            :delete
            :path-params   `(("petId" . ,pet-id))
            :header-params `(("api_key" . ,api-key))
            :query-params  `()
            :form-params   `()
            :content-types '()
            :accepts       '("application/xml" "application/json")
            :auth-names    '("petstore_auth")))

(defun find-pets-by-status (status)
  "Finds Pets by status
   Multiple status values can be provided with comma separated strings"
  (call-api "/pet/findByStatus"
            :get
            :path-params   `()
            :header-params `()
            :query-params  `(("status" . ,status))
            :form-params   `()
            :content-types '()
            :accepts       '("application/xml" "application/json")
            :auth-names    '("petstore_auth")))

(defun find-pets-by-tags (tags)
  "Finds Pets by tags
   Muliple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing."
  (call-api "/pet/findByTags"
            :get
            :path-params   `()
            :header-params `()
            :query-params  `(("tags" . ,tags))
            :form-params   `()
            :content-types '()
            :accepts       '("application/xml" "application/json")
            :auth-names    '("petstore_auth")))

(defun get-pet-by-id (pet-id)
  "Find pet by ID
   Returns a single pet"
  (call-api "/pet/{petId}"
            :get
            :path-params   `(("petId" . ,pet-id))
            :header-params `()
            :query-params  `()
            :form-params   `()
            :content-types '()
            :accepts       '("application/xml" "application/json")
            :auth-names    '("api_key")))

(defun update-pet (body)
  "Update an existing pet
   "
  (call-api "/pet"
            :put
            :path-params   `()
            :header-params `()
            :query-params  `()
            :form-params   `()
            :body-param    body
            :content-types '("application/json" "application/xml")
            :accepts       '("application/xml" "application/json")
            :auth-names    '("petstore_auth")))

(defun update-pet-with-form (pet-id  &key name status)
  "Updates a pet in the store with form data
   "
  (call-api "/pet/{petId}"
            :post
            :path-params   `(("petId" . ,pet-id))
            :header-params `()
            :query-params  `()
            :form-params   `(("name" . ,name) ("status" . ,status))
            :content-types '("application/x-www-form-urlencoded")
            :accepts       '("application/xml" "application/json")
            :auth-names    '("petstore_auth")))

(defun upload-file (pet-id  &key additional-metadata file)
  "uploads an image
   "
  (call-api "/pet/{petId}/uploadImage"
            :post
            :path-params   `(("petId" . ,pet-id))
            :header-params `()
            :query-params  `()
            :form-params   `(("additionalMetadata" . ,additional-metadata) ("file" . ,file))
            :content-types '("multipart/form-data")
            :accepts       '("application/json")
            :auth-names    '("petstore_auth")))

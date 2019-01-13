(in-package :cl-user)
(defpackage swagger-petstore.api/user
  (:use :cl
        :swagger-petstore.core))
(in-package :swagger-petstore.api/user)


(defun create-user (body)
  "Create user
   This can only be done by the logged in user."
  (call-api "/user"
            :post
            :path-params   `()
            :header-params `()
            :query-params  `()
            :form-params   `()
            :body-param    body
            :content-types '()
            :accepts       '("application/xml" "application/json")
            :auth-names    '()))

(defun create-users-with-array-input (body)
  "Creates list of users with given input array
   "
  (call-api "/user/createWithArray"
            :post
            :path-params   `()
            :header-params `()
            :query-params  `()
            :form-params   `()
            :body-param    body
            :content-types '()
            :accepts       '("application/xml" "application/json")
            :auth-names    '()))

(defun create-users-with-list-input (body)
  "Creates list of users with given input array
   "
  (call-api "/user/createWithList"
            :post
            :path-params   `()
            :header-params `()
            :query-params  `()
            :form-params   `()
            :body-param    body
            :content-types '()
            :accepts       '("application/xml" "application/json")
            :auth-names    '()))

(defun delete-user (username)
  "Delete user
   This can only be done by the logged in user."
  (call-api "/user/{username}"
            :delete
            :path-params   `(("username" . ,username))
            :header-params `()
            :query-params  `()
            :form-params   `()
            :content-types '()
            :accepts       '("application/xml" "application/json")
            :auth-names    '()))

(defun get-user-by-name (username)
  "Get user by user name
   "
  (call-api "/user/{username}"
            :get
            :path-params   `(("username" . ,username))
            :header-params `()
            :query-params  `()
            :form-params   `()
            :content-types '()
            :accepts       '("application/xml" "application/json")
            :auth-names    '()))

(defun login-user (username password)
  "Logs user into the system
   "
  (call-api "/user/login"
            :get
            :path-params   `()
            :header-params `()
            :query-params  `(("username" . ,username) ("password" . ,password))
            :form-params   `()
            :content-types '()
            :accepts       '("application/xml" "application/json")
            :auth-names    '()))

(defun logout-user ()
  "Logs out current logged in user session
   "
  (call-api "/user/logout"
            :get
            :path-params   `()
            :header-params `()
            :query-params  `()
            :form-params   `()
            :content-types '()
            :accepts       '("application/xml" "application/json")
            :auth-names    '()))

(defun update-user (username body)
  "Updated user
   This can only be done by the logged in user."
  (call-api "/user/{username}"
            :put
            :path-params   `(("username" . ,username))
            :header-params `()
            :query-params  `()
            :form-params   `()
            :body-param    body
            :content-types '()
            :accepts       '("application/xml" "application/json")
            :auth-names    '()))

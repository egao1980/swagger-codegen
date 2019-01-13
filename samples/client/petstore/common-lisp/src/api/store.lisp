(in-package :cl-user)
(defpackage swagger-petstore.api/store
  (:use :cl
        :swagger-petstore.core))
(in-package :swagger-petstore.api/store)


(defun delete-order (order-id)
  "Delete purchase order by ID
   For valid response try integer IDs with positive integer value. Negative or non-integer values will generate API errors"
  (call-api "/store/order/{orderId}"
            :delete
            :path-params   `(("orderId" . ,order-id))
            :header-params `()
            :query-params  `()
            :form-params   `()
            :content-types '()
            :accepts       '("application/xml" "application/json")
            :auth-names    '()))

(defun get-inventory ()
  "Returns pet inventories by status
   Returns a map of status codes to quantities"
  (call-api "/store/inventory"
            :get
            :path-params   `()
            :header-params `()
            :query-params  `()
            :form-params   `()
            :content-types '()
            :accepts       '("application/json")
            :auth-names    '("api_key")))

(defun get-order-by-id (order-id)
  "Find purchase order by ID
   For valid response try integer IDs with value >= 1 and <= 10. Other values will generated exceptions"
  (call-api "/store/order/{orderId}"
            :get
            :path-params   `(("orderId" . ,order-id))
            :header-params `()
            :query-params  `()
            :form-params   `()
            :content-types '()
            :accepts       '("application/xml" "application/json")
            :auth-names    '()))

(defun place-order (body)
  "Place an order for a pet
   "
  (call-api "/store/order"
            :post
            :path-params   `()
            :header-params `()
            :query-params  `()
            :form-params   `()
            :body-param    body
            :content-types '()
            :accepts       '("application/xml" "application/json")
            :auth-names    '()))

(in-package :cl-user)
(defpackage swagger-petstore.core
  (:use :cl)
  (:export call-api))
(in-package :swagger-petstore.core)

(defun set-path-params (url params)
  (cl-ppcre:regex-replace-all "{([a-zA-Z\.\-_0-9]+)}"
                              url
                              #'(lambda (match &rest args)
                                  (let ((var (car args)))
                                    (or (cdr (assoc (car args) params :test #'string=))
                                        (car args))))
                              :simple-calls t))

(defun call-api (url
                 &optional (method :get)
                 &key (base-url "http://petstore.swagger.io/v2") body-param path-params header-params query-params form-params auth-names
                   (content-types '("application/json")) (accepts '("application/json")))
  "Call remote API endpoint"
  (multiple-value-bind (stream code)
      (drakma:http-request (format nil "~a~a" base-url (set-path-params url path-params))
                           :parameters query-params
                           :additional-headers header-params
                           :content body-param
                           :accept (car accepts)
                           :content-type (car content-types)
                           :want-stream t
                           :method method)
    (if (equal code 200)
        (progn (setf (flexi-streams:flexi-stream-external-format stream) :utf-8)
               (cl-json:decode-json stream))
        (format t "HTTP CODE : ~A ~%" code))))

;;;; middleware.lisp
(in-package :cl-user)
(uiop:define-package :conduit.middleware
  (:use :cl)
  (:import-from :conduit.errors
                #:validation-error
                #:signal-validation-error)
  (:import-from :conduit.auth
                #:verify-auth-token)
  (:import-from :tiny-routes
                #:request-get
                #:request-header
                #:request-body
                #:request-append
                #:pipe
                #:wrap-request-body
                #:wrap-request-mapper
                #:wrap-response-mapper
                #:wrap-response-content-type
                #:wrap-response-body-mapper
                #:wrap-post-match-middleware
                #:bad-request
                #:query-string)
  (:export #:error-response
           #:json-body
           #:wrap-request-json-body
           #:wrap-response-json-body
           #:wrap-condition
           #:auth-get
           #:wrap-auth
           #:query-parameters
           #:wrap-query-parameters))

(in-package :conduit.middleware)

(defun error-message (condition)
  (apply #'format nil
         (simple-condition-format-control condition)
         (simple-condition-format-arguments condition)))

(defun error-response (error-message)
  (list :|error| error-message))

(defun json-body (request)
  (request-get request :json-body))

(defun wrap-request-json-body (handler)
  (wrap-request-body
   (wrap-request-mapper
    handler
    (lambda (request)
      (let* ((request-body (request-body request))
             (json-body (jojo:parse request-body)))
        (request-append request :json-body json-body))))))

(defun wrap-response-json-body (handler)
  (wrap-response-mapper
   handler
   (lambda (response)
     (pipe response
       (tiny:header-response :content-type "application/json")
       (tiny:body-mapper-response #'jojo:to-json)))))

(defun wrap-condition (handler)
  (lambda (request)
    (handler-case (funcall handler request)
      (jojo:<jonathan-error> ()
        (bad-request (error-response "Unparsable JSON")))
      (validation-error (c)
        (bad-request (error-response (error-message c)))))))

(defun auth-get (request key &optional default)
  (let ((claims (request-get request :claims)))
    (getf claims key default)))

(defun wrap-auth--internal (handler)
  (wrap-request-mapper
   handler
   (lambda (request)
     (let ((authorization (request-header request "authorization")))
       (unless authorization
         (signal-validation-error "Missing authorization header"))
       (unless (uiop:string-prefix-p "Token " authorization)
         (signal-validation-error "Missing Token"))
       (let* ((token (second (uiop:split-string authorization)))
              (claims (verify-auth-token token)))
         (request-append request :auth (append (list :token token) claims)))))))

(defun wrap-auth (handler)
  ;; Wrap auth after path-info and HTTP method matching
  (wrap-post-match-middleware handler #'wrap-auth--internal))

(defun query-parameters (request)
  (request-get request :query-parameters))

(defun parse-query-parameters (query-string)
  (let (params)
    (dolist (pair (uiop:split-string query-string :separator '(#\&)))
      (destructuring-bind (&optional key value &rest rest) (uiop:split-string pair :separator '(#\=))
        (when (and key value (null rest))
          (push value params)
          (push (intern key :keyword) params))))
    params))

(defun wrap-query-parameters (handler)
  (wrap-request-mapper
   handler
   (lambda (request)
     (let ((params (parse-query-parameters (query-string request))))
       (request-append request :query-parameters params)))))

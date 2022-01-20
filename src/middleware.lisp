;;;; middleware.lisp
(in-package :cl-user)
(uiop:define-package :conduit.middleware
  (:use :cl)
  (:local-nicknames (:errors :conduit.errors)
                    (:log :conduit.logger)
                    (:auth :conduit.auth))
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
  (tiny:request-get request :json-body))

(defun wrap-request-json-body (handler)
  (tiny:wrap-request-body
   (tiny:wrap-request-mapper
    handler
    (lambda (request)
      (let* ((request-body (tiny:request-body request))
             (json-body (jojo:parse request-body)))
        (tiny:request-append request :json-body json-body))))))

(defun wrap-response-json-body (handler)
  (tiny:wrap-response-mapper
   handler
   (lambda (response)
     (tiny:pipe response
       (tiny:header-response :content-type "application/json")
       (tiny:body-mapper-response #'jojo:to-json)))))

(defun wrap-condition (handler)
  (lambda (request)
    (handler-case (funcall handler request)
      (jojo:<jonathan-error> ()
        (tiny:bad-request (error-response "Unparsable JSON")))
      (errors:validation-error (c)
        (tiny:bad-request (error-response (error-message c))))
      (simple-error (c)
        (log:error :middleware c)
        (tiny:unprocessable-entity (error-response (format nil "~s" c)))))))

(defun auth-get (request key &optional default)
  (let ((claims (tiny:request-get request :claims)))
    (getf claims key default)))

(defun wrap-auth--internal (handler)
  (tiny:wrap-request-mapper
   handler
   (lambda (request)
     (let ((authorization (tiny:request-header request "authorization")))
       (unless authorization
         (errors:signal-validation-error "Missing authorization header"))
       (unless (uiop:string-prefix-p "Token " authorization)
         (errors:signal-validation-error "Missing Token"))
       (let* ((token (second (uiop:split-string authorization)))
              (claims (auth:verify-auth-token token)))
         (tiny:request-append request :auth (append (list :token token) claims)))))))

(defun wrap-auth (handler)
  ;; Wrap auth after path-info and HTTP method matching
  (tiny:wrap-post-match-middleware handler #'wrap-auth--internal))

(defun query-parameters (request)
  (tiny:request-get request :query-parameters))

(defun parse-query-parameters (query-string)
  (let (params)
    (dolist (pair (uiop:split-string query-string :separator '(#\&)))
      (destructuring-bind (&optional key value &rest rest) (uiop:split-string pair :separator '(#\=))
        (when (and key value (null rest))
          (push value params)
          (push (intern key :keyword) params))))
    params))

(defun wrap-query-parameters (handler)
  (tiny:wrap-request-mapper
   handler
   (lambda (request)
     (let ((params (parse-query-parameters (tiny:query-string request))))
       (tiny:request-append request :query-parameters params)))))

(in-package :conduit)

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
       (tiny:application/json-response)
       (tiny:body-mapper-response #'jojo:to-json)))))

(defun wrap-condition (handler)
  (lambda (request)
    (handler-case (funcall handler request)
      (jojo:<jonathan-error> ()
        (tiny:bad-request (error-response "Unparsable JSON")))
      (validation-error (c)
        (tiny:bad-request (error-response (error-message c)))))))

(defun claims-get (request key &optional default)
  (let ((claims (tiny:request-get request :claims)))
    (getf claims key default)))

(defun wrap-auth--internal (handler)
  (tiny:wrap-request-mapper
   handler
   (lambda (request)
     (let ((authorization (tiny:request-header request "authorization")))
       (unless authorization
         (signal-validation-error "Missing authorization header"))
       (unless (uiop:string-prefix-p "Token " authorization)
         (signal-validation-error "Missing Token"))
       (let* ((token (second (uiop:split-string authorization)))
              (claims (verify-auth-token token)))
         (tiny:pipe request
           (tiny:request-append :claims claims)
           (tiny:request-append :token token)))))))

(defun wrap-auth (handler)
  ;; Wrap auth after path-info and HTTP method matching
  (tiny:wrap-post-match-middleware handler #'wrap-auth--internal))

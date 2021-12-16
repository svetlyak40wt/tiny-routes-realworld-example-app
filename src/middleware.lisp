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

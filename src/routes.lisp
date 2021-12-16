(in-package :conduit)

(defun ok (body)
  (tiny:ok body))

(defun forbidden (body)
  (tiny:forbidden (error-response body)))

(defun not-found (body)
  (tiny:not-found (error-response body)))

(define-routes authentication-routes
  (define-post "/api/users/login" (request)
    (let* ((json-body (json-body request))
           (user (getf json-body :|user|))
           (user (users/login-user (getf user :|email|) (getf user :|password|))))
      (if user
          (ok (list :|user| user))
          (forbidden "Invalid credentials")))))

(define-routes api-routes
  authentication-routes
  (define-route () (not-found "NOT_FOUND")))

(define-routes app-routes
  (tiny:pipe api-routes
    (wrap-request-json-body)
    (wrap-response-json-body)))

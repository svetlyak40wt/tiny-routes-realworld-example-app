(in-package :conduit)

(defun ok (body)
  (tiny:ok body))

(defun forbidden (body)
  (tiny:forbidden (error-response body)))

(defun not-found (body)
  (tiny:not-found (error-response body)))

(defun unprocessable-entity (body)
  (tiny:unprocessable-entity (error-response body)))

(define-routes authentication-routes
  (define-post "/api/users/login" (request)
    (let* ((json-body (json-body request))
           (user (getf json-body :|user|))
           (user (users/login-user (getf user :|email|) (getf user :|password|))))
      (if user
          (ok (list :|user| user))
          (forbidden "Invalid credentials"))))

  (define-post "/api/users" (request)
    (let* ((json-body (json-body request))
           (rendition (parse-user-registration-rendition (getf json-body :|user|)))
           (user (users/register-user rendition)))
      (if user
          (ok (list :|user| user))
          (unprocessable-entity "Unable to register user")))))

(define-routes api-routes
  authentication-routes
  (define-route () (not-found "NOT_FOUND")))

(define-routes app-routes
  (tiny:pipe api-routes
    (wrap-request-json-body)
    (wrap-condition)
    (wrap-response-json-body)))
